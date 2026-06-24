// ============ StudyPilot — pomodoro.js (v4 fixed) ============
// Loaded in <head>, so document.body may NOT exist yet.
// All DOM access is deferred to DOMContentLoaded or jQuery ready.

try {

// ============ SERVICE WORKER REGISTRATION ============
if ('serviceWorker' in navigator) {
  navigator.serviceWorker.register('sw.js').then(function(reg) {
    console.log('[SP] SW registered');
  }).catch(function(err) {
    console.log('[SP] SW registration failed:', err);
  });
}

// ============ PWA INSTALL PROMPT ============
var deferredInstallPrompt = null;

window.addEventListener('beforeinstallprompt', function(e) {
  e.preventDefault();
  deferredInstallPrompt = e;
  var btn = document.getElementById('btn_install_app');
  if (btn) { btn.style.display = 'inline-flex'; }
  console.log('[SP] Install prompt captured');
});

function triggerPWAInstall() {
  if (!deferredInstallPrompt) {
    alert('Para instalar StudyPilot:\n\n📱 Móvil: Menú ⋮ → "Añadir a pantalla de inicio"\n💻 PC: Barra de dirección → icono de instalar ⬇');
    return;
  }
  deferredInstallPrompt.prompt();
  deferredInstallPrompt.userChoice.then(function(choice) {
    console.log('[SP] Install choice:', choice.outcome);
    deferredInstallPrompt = null;
    if (choice.outcome === 'accepted') {
      var btn = document.getElementById('btn_install_app');
      if (btn) btn.style.display = 'none';
    }
  });
}

window.addEventListener('appinstalled', function() {
  deferredInstallPrompt = null;
  var btn = document.getElementById('btn_install_app');
  if (btn) btn.style.display = 'none';
});

// ============ OFFLINE DETECTION ============
var shinyDisconnected = false;

function updateOnlineStatus() {
  var isOnline = navigator.onLine;
  if (window.Shiny && Shiny.setInputValue) {
    Shiny.setInputValue('app_online', isOnline, {priority: 'event'});
  }
  var banner = document.getElementById('offline-banner');
  if (banner) banner.style.display = isOnline ? 'none' : 'flex';
  ['syllabus_extract_btn', 'gcal_sync', 'btn_gen_schedule', 'schedule_extract_btn'].forEach(function(id) {
    var el = document.getElementById(id);
    if (el) { el.disabled = !isOnline; el.style.opacity = isOnline ? '1' : '0.5'; }
  });
  if (isOnline && shinyDisconnected) {
    setTimeout(function() { location.reload(); }, 3000);
  }
}

window.addEventListener('online', updateOnlineStatus);
window.addEventListener('offline', updateOnlineStatus);

// ============ NUCLEAR SHINY DISCONNECT OVERRIDE ============
function nukeShinyDisconnectUI() {
  ['shiny-disconnected-overlay', 'shiny-reconnect-text'].forEach(function(id) {
    var el = document.getElementById(id);
    if (el) el.remove();
  });
  document.querySelectorAll('.shiny-notification, #shiny-notification-panel, .shiny-notification-panel, .modal-backdrop').forEach(function(el) {
    el.remove();
  });
  if (document.body) {
    document.body.style.pointerEvents = 'auto';
    document.body.style.filter = 'none';
    document.body.classList.remove('shiny-busy');
  }
  document.querySelectorAll('[id*="disconnected"], [class*="disconnected"]').forEach(function(el) {
    el.style.display = 'none';
  });
}

// ============ LOCALSTORAGE CALENDAR CACHE ============
function loadCalendarFromCache() {
  try {
    var cached = localStorage.getItem('sp_calendar_events');
    var ts = localStorage.getItem('sp_calendar_ts');
    if (!cached) return;
    var events = JSON.parse(cached);
    if (!events || events.length === 0) return;
    var calContainer = document.querySelector('.cal-container');
    if (!calContainer) return;
    if (calContainer.querySelector('.offline-notice')) return;
    var notice = document.createElement('div');
    notice.className = 'offline-notice';
    notice.style.cssText = 'background:#fef3c7;color:#92400e;padding:8px 16px;border-radius:8px;margin-bottom:8px;font-size:0.8rem;font-weight:600;text-align:center;';
    notice.textContent = '📦 Vista offline — datos del ' + (ts ? new Date(ts).toLocaleString() : 'caché');
    calContainer.insertBefore(notice, calContainer.firstChild);
  } catch(e) { console.log('[SP] Cache load error:', e); }
}

// ============ POMODORO TIMER (DELTA-TIME) ============
var pomo = {
  running: false,
  mode: 'work',
  durationSec: 25 * 60,
  endTimestamp: null,
  pausedRemaining: null,
  sessions: 0,
  tickId: null,
  workMin: 25,
  shortMin: 5,
  longMin: 15
};

var music = { playing: false, type: null, ctx: null, node: null, gain: null, vol: 0.3 };

function getTimeLeft() {
  if (!pomo.running) return pomo.pausedRemaining !== null ? pomo.pausedRemaining : pomo.durationSec * 1000;
  return Math.max(0, pomo.endTimestamp - Date.now());
}

function updateDisplay() {
  var el = document.getElementById('pomo-timer');
  if (!el) return;

  var totalSec = Math.ceil(getTimeLeft() / 1000);
  var mm = String(Math.floor(totalSec / 60)).padStart(2, '0');
  var ss = String(totalSec % 60).padStart(2, '0');
  el.textContent = mm + ':' + ss;
  el.style.color = pomo.mode === 'work' ? '#dc2626' : '#16a34a';

  var modeEl = document.getElementById('pomo-mode-label');
  if (modeEl) {
    modeEl.textContent = pomo.mode === 'work' ? '🎯 Tiempo de Estudio' :
      pomo.mode === 'short' ? '☕ Descanso Corto' : '🌴 Descanso Largo';
  }

  var toggleBtn = document.getElementById('pomo_toggle');
  if (toggleBtn) {
    toggleBtn.innerHTML = pomo.running ? '⏸ Pausar' : '▶ Iniciar';
  }

  var dotsEl = document.getElementById('pomo-dots');
  if (dotsEl) {
    var dots = '';
    for (var i = 0; i < 4; i++) {
      var cls = 'pomo-dot';
      if (i < pomo.sessions % 4) cls += ' done';
      else if (i === pomo.sessions % 4 && pomo.running) cls += ' active';
      dots += '<span class="' + cls + '"></span>';
    }
    dotsEl.innerHTML = dots;
  }

  var sessEl = document.getElementById('pomo-sessions');
  if (sessEl) sessEl.textContent = pomo.sessions;
  var totalEl = document.getElementById('pomo-total');
  if (totalEl) totalEl.textContent = (pomo.sessions * pomo.workMin) + ' min';
}

function pomoTick() {
  if (!pomo.running) return;
  var msLeft = pomo.endTimestamp - Date.now();
  if (msLeft <= 0) {
    pomo.running = false;
    pomo.pausedRemaining = null;
    clearTimeout(pomo.tickId);
    if (pomo.mode === 'work') {
      pomo.sessions++;
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue('pomo_session_done', pomo.sessions, {priority: 'event'});
      }
      var isLong = pomo.sessions % 4 === 0;
      pomo.mode = isLong ? 'long' : 'short';
      pomo.durationSec = isLong ? pomo.longMin * 60 : pomo.shortMin * 60;
      pomo.pausedRemaining = pomo.durationSec * 1000;
      showNotif(isLong ? '🌴 Descanso largo! Sesiones: ' + pomo.sessions : '☕ Descanso corto!', 'work_done');
    } else {
      pomo.mode = 'work';
      pomo.durationSec = pomo.workMin * 60;
      pomo.pausedRemaining = pomo.durationSec * 1000;
      showNotif('🎯 A estudiar! (' + pomo.workMin + ' min)', 'break_done');
    }
    updateDisplay();
    return;
  }
  updateDisplay();
  pomo.tickId = setTimeout(pomoTick, 1000);
}

// ============ NOTIFICATION SOUNDS (Web Audio API) ============
function playSound(type) {
  try {
    var ctx = new (window.AudioContext || window.webkitAudioContext)();
    var now = ctx.currentTime;
    var freqs = type === 'work_done' ? [523.25, 659.25, 783.99] : [880, 1108.73];
    var waveform = type === 'work_done' ? 'sine' : 'triangle';
    var spacing = type === 'work_done' ? 0.25 : 0.2;
    freqs.forEach(function(freq, i) {
      var osc = ctx.createOscillator();
      var gain = ctx.createGain();
      osc.type = waveform;
      osc.frequency.value = freq;
      gain.gain.setValueAtTime(0, now + i * spacing);
      gain.gain.linearRampToValueAtTime(0.4, now + i * spacing + 0.05);
      gain.gain.exponentialRampToValueAtTime(0.001, now + i * spacing + 0.6);
      osc.connect(gain);
      gain.connect(ctx.destination);
      osc.start(now + i * spacing);
      osc.stop(now + i * spacing + 0.7);
    });
    setTimeout(function() { ctx.close(); }, 3000);
  } catch(e) { console.log('[SP] Sound error:', e); }
}

function showNotif(msg, type) {
  playSound(type);
  var el = document.createElement('div');
  el.className = 'pomo-notif ' + (type === 'work_done' ? 'notif-success' : 'notif-primary');
  el.textContent = msg;
  document.body.appendChild(el);
  setTimeout(function() { el.style.opacity = '0'; setTimeout(function() { el.remove(); }, 500); }, 4000);
  if ('Notification' in window && Notification.permission === 'granted') {
    new Notification('🍅 Pomodoro', { body: msg });
  } else if ('Notification' in window && Notification.permission !== 'denied') {
    Notification.requestPermission();
  }
}

// ============ TIMER CONTROLS ============
function getPomoDuration() {
  var input = document.querySelector('#pomo_duration input[type="number"]');
  return input ? (parseInt(input.value) || 25) : 25;
}

function pomoToggle() {
  console.log('[SP] pomoToggle called, running:', pomo.running);
  if (pomo.running) {
    pomo.pausedRemaining = Math.max(0, pomo.endTimestamp - Date.now());
    pomo.running = false;
    clearTimeout(pomo.tickId);
  } else {
    if (pomo.pausedRemaining === null) {
      pomo.workMin = getPomoDuration();
      pomo.durationSec = pomo.workMin * 60;
    }
    var startMs = pomo.pausedRemaining !== null ? pomo.pausedRemaining : pomo.durationSec * 1000;
    pomo.endTimestamp = Date.now() + startMs;
    pomo.pausedRemaining = null;
    pomo.running = true;
    pomoTick();
  }
  updateDisplay();
}

function pomoReset() {
  console.log('[SP] pomoReset called');
  pomo.running = false;
  clearTimeout(pomo.tickId);
  pomo.mode = 'work';
  pomo.workMin = getPomoDuration();
  pomo.durationSec = pomo.workMin * 60;
  pomo.pausedRemaining = null;
  updateDisplay();
}

function pomoSkip() {
  console.log('[SP] pomoSkip called');
  pomo.running = false;
  clearTimeout(pomo.tickId);
  if (pomo.mode === 'work') {
    pomo.sessions++;
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('pomo_session_done', pomo.sessions, {priority: 'event'});
    }
    pomo.mode = pomo.sessions % 4 === 0 ? 'long' : 'short';
    pomo.durationSec = pomo.mode === 'long' ? pomo.longMin * 60 : pomo.shortMin * 60;
  } else {
    pomo.mode = 'work';
    pomo.durationSec = pomo.workMin * 60;
  }
  pomo.pausedRemaining = null;
  updateDisplay();
}

function pomoUndo() {
  if (pomo.sessions > 0) { pomo.sessions--; updateDisplay(); }
}

// ============ AMBIENT MUSIC (Web Audio API — procedural noise) ============
function toggleMusic(type) {
  console.log('[SP] toggleMusic:', type);
  if (music.playing && music.type === type) { stopMusic(); return; }
  stopMusic();
  try {
    music.type = type;
    music.playing = true;
    var ctx = new (window.AudioContext || window.webkitAudioContext)();
    music.ctx = ctx;
    // Resume context if suspended (Chrome autoplay policy)
    if (ctx.state === 'suspended') { ctx.resume(); }
    var bufSize = 4096;
    var node = ctx.createScriptProcessor(bufSize, 1, 1);
    var gain = ctx.createGain();
    gain.gain.value = music.vol;
    music.gain = gain;
    music.node = node;
    var b1 = 0;
    node.onaudioprocess = function(e) {
      var out = e.outputBuffer.getChannelData(0);
      for (var i = 0; i < bufSize; i++) {
        var w = Math.random() * 2 - 1;
        if (type === 'white') { out[i] = w; }
        else if (type === 'brown') { b1 += w * 0.02; b1 -= b1 * 0.02; out[i] = b1 * 3.5; }
        else { b1 = 0.99886 * b1 + w * 0.0555179; out[i] = b1 * 0.5; } // pink
        out[i] = Math.max(-1, Math.min(1, out[i]));
      }
    };
    node.connect(gain);
    gain.connect(ctx.destination);
    console.log('[SP] Music playing:', type);
  } catch(e) {
    console.error('[SP] Music error:', e);
    music.playing = false;
  }
}

function stopMusic() {
  try {
    if (music.node) { music.node.disconnect(); music.node = null; }
    if (music.gain) { music.gain.disconnect(); music.gain = null; }
    if (music.ctx) { music.ctx.close(); music.ctx = null; }
  } catch(e) {}
  music.playing = false;
  music.type = null;
}

// ============ JQUERY READY — all DOM-dependent setup ============
$(function() {
  console.log('[SP] pomodoro.js DOM ready — binding events');

  // Nuke observer (MUST wait for document.body to exist)
  try {
    var nukeObserver = new MutationObserver(function(mutations) {
      mutations.forEach(function(m) {
        m.addedNodes.forEach(function(node) {
          if (node.nodeType !== 1) return;
          var id = node.id || '';
          var cls = (typeof node.className === 'string') ? node.className : '';
          if (id.indexOf('disconnected') !== -1 || id.indexOf('shiny-notification') !== -1 ||
              cls.indexOf('shiny-notification') !== -1 || cls.indexOf('modal-backdrop') !== -1) {
            node.remove();
          }
        });
      });
    });
    nukeObserver.observe(document.body, { childList: true, subtree: true });
    console.log('[SP] MutationObserver active');
  } catch(e) { console.error('[SP] MutationObserver error:', e); }

  // Shiny disconnect events
  $(document).on('shiny:disconnected shiny:error', function(event) {
    if (event.type === 'shiny:disconnected') shinyDisconnected = true;
    nukeShinyDisconnectUI();
    setTimeout(nukeShinyDisconnectUI, 100);
    setTimeout(nukeShinyDisconnectUI, 500);
    setTimeout(nukeShinyDisconnectUI, 2000);
    var banner = document.getElementById('offline-banner');
    if (banner) banner.style.display = 'flex';
    loadCalendarFromCache();
    if (navigator.onLine) setTimeout(function() { location.reload(); }, 4000);
  });

  // ====== POMODORO BUTTON HANDLERS ======
  $(document).on('click', '#pomo_toggle', function(e) {
    e.preventDefault(); e.stopPropagation();
    console.log('[SP] pomo_toggle CLICKED');
    pomoToggle();
    return false;
  });
  $(document).on('click', '#pomo_reset', function(e) {
    e.preventDefault(); e.stopPropagation();
    pomoReset();
    return false;
  });
  $(document).on('click', '#pomo_skip', function(e) {
    e.preventDefault(); e.stopPropagation();
    pomoSkip();
    return false;
  });
  $(document).on('click', '#pomo_undo', function(e) {
    e.preventDefault(); e.stopPropagation();
    pomoUndo();
    return false;
  });

  // ====== MUSIC BUTTON HANDLERS ======
  $(document).on('click', '#music_white', function(e) {
    e.preventDefault(); e.stopPropagation();
    toggleMusic('white'); return false;
  });
  $(document).on('click', '#music_brown', function(e) {
    e.preventDefault(); e.stopPropagation();
    toggleMusic('brown'); return false;
  });
  $(document).on('click', '#music_pink', function(e) {
    e.preventDefault(); e.stopPropagation();
    toggleMusic('pink'); return false;
  });
  $(document).on('click', '#music_stop', function(e) {
    e.preventDefault(); e.stopPropagation();
    stopMusic(); return false;
  });
  $(document).on('input', '#music-volume', function() {
    music.vol = this.value / 100;
    if (music.gain) music.gain.gain.value = music.vol;
  });

  // ====== PWA INSTALL BUTTON ======
  $(document).on('click', '#btn_install_app', function(e) {
    e.preventDefault(); e.stopPropagation();
    triggerPWAInstall(); return false;
  });

  // ====== DURATION CHANGE ======
  $(document).on('change', '#pomo_duration input', function() {
    if (!pomo.running && pomo.mode === 'work') {
      pomo.workMin = parseInt(this.value) || 25;
      pomo.durationSec = pomo.workMin * 60;
      pomo.pausedRemaining = null;
      updateDisplay();
    }
  });

  // ====== SHINY CONNECTED ======
  $(document).on('shiny:connected', function() {
    console.log('[SP] Shiny connected');
    shinyDisconnected = false;
    updateDisplay();
    updateOnlineStatus();
  });

  console.log('[SP] All event handlers registered successfully');
});

// ============ CALENDAR DRAG-TO-MOVE ============
$(function() {
  var dragState = null;
  var HOUR_H = 50;

  $(document).on('mousedown touchstart', '.cal-event', function(e) {
    if (e.type === 'mousedown' && e.which !== 1) return;
    var clientY = e.type === 'touchstart' ? e.originalEvent.touches[0].clientY : e.clientY;
    dragState = {
      el: this,
      startY: clientY,
      origTop: parseInt(this.style.top) || 0,
      moved: false
    };
    this.style.zIndex = '10';
    this.style.opacity = '0.8';
    e.preventDefault();
  });

  $(document).on('mousemove touchmove', function(e) {
    if (!dragState) return;
    var clientY = e.type === 'touchmove' ? e.originalEvent.touches[0].clientY : e.clientY;
    var dy = clientY - dragState.startY;
    if (Math.abs(dy) > 5) dragState.moved = true;
    var snap = HOUR_H / 4;
    var newTop = Math.max(0, Math.round((dragState.origTop + dy) / snap) * snap);
    dragState.el.style.top = newTop + 'px';
    e.preventDefault();
  });

  $(document).on('mouseup touchend', function(e) {
    if (!dragState) return;
    dragState.el.style.zIndex = '';
    dragState.el.style.opacity = '';
    if (dragState.moved) {
      var newTop = parseInt(dragState.el.style.top) || 0;
      var newHour = newTop / HOUR_H;
      var h = Math.floor(newHour);
      var m = Math.round((newHour - h) * 60 / 15) * 15;
      if (m >= 60) { h++; m = 0; }
      var newStart = String(h).padStart(2, '0') + ':' + String(m).padStart(2, '0');
      var nameEl = dragState.el.querySelector('.cal-ev-name');
      var timeEl = dragState.el.querySelector('.cal-ev-time');
      var title = nameEl ? nameEl.textContent : '';
      var oldTime = timeEl ? timeEl.textContent : '';
      var parts = oldTime.split('–');
      var oldS = (parts[0] || '').trim().split(':');
      var oldE = (parts[1] || '').trim().split(':');
      var durMin = 60;
      if (oldS.length === 2 && oldE.length === 2) {
        durMin = (parseInt(oldE[0]) * 60 + parseInt(oldE[1])) - (parseInt(oldS[0]) * 60 + parseInt(oldS[1]));
        if (durMin <= 0) durMin = 60;
      }
      var endMin = h * 60 + m + durMin;
      var newEnd = String(Math.floor(endMin / 60)).padStart(2, '0') + ':' + String(endMin % 60).padStart(2, '0');
      if (timeEl) timeEl.textContent = newStart + ' – ' + newEnd;
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue('cal_drag_move', {
          title: title, new_start: newStart, new_end: newEnd, old_time: oldTime.trim()
        }, {priority: 'event'});
      }
    }
    dragState = null;
  });
});

} catch(e) {
  console.error('[SP] FATAL ERROR in pomodoro.js:', e);
}
