// ============ SERVICE WORKER REGISTRATION ============
if ('serviceWorker' in navigator) {
  navigator.serviceWorker.register('sw.js').then(function(reg) {
    console.log('[SP] SW registered');
  }).catch(function(err) {
    console.log('[SP] SW failed:', err);
  });
}

// ============ PWA INSTALL PROMPT ============
var deferredInstallPrompt = null;

window.addEventListener('beforeinstallprompt', function(e) {
  e.preventDefault();
  deferredInstallPrompt = e;
  // Show the install button if it exists
  var btn = document.getElementById('btn_install_app');
  if (btn) {
    btn.style.display = 'inline-flex';
    btn.classList.add('pwa-install-ready');
  }
  console.log('[SP] Install prompt captured');
});

function triggerPWAInstall() {
  if (!deferredInstallPrompt) {
    // Fallback: tell user how to install manually
    alert('Para instalar StudyPilot:\n\n' +
      '📱 Móvil: Menú ⋮ → "Añadir a pantalla de inicio"\n' +
      '💻 PC: Barra de dirección → icono de instalar ⬇');
    return;
  }
  deferredInstallPrompt.prompt();
  deferredInstallPrompt.userChoice.then(function(choice) {
    console.log('[SP] Install choice:', choice.outcome);
    deferredInstallPrompt = null;
    var btn = document.getElementById('btn_install_app');
    if (btn && choice.outcome === 'accepted') btn.style.display = 'none';
  });
}

window.addEventListener('appinstalled', function() {
  console.log('[SP] App installed');
  deferredInstallPrompt = null;
  var btn = document.getElementById('btn_install_app');
  if (btn) btn.style.display = 'none';
});

// ============ OFFLINE DETECTION + SHINY DISCONNECT OVERRIDE ============
var shinyDisconnected = false;

function updateOnlineStatus() {
  var isOnline = navigator.onLine;
  if (window.Shiny && Shiny.setInputValue) {
    Shiny.setInputValue('app_online', isOnline, {priority: 'event'});
  }
  var banner = document.getElementById('offline-banner');
  if (banner) banner.style.display = isOnline ? 'none' : 'flex';

  // Disable API buttons when offline
  ['syllabus_extract_btn', 'gcal_sync', 'btn_gen_schedule', 'schedule_extract_btn'].forEach(function(id) {
    var el = document.getElementById(id);
    if (el) { el.disabled = !isOnline; el.style.opacity = isOnline ? '1' : '0.5'; }
  });

  // If back online and Shiny was disconnected, try to reconnect
  if (isOnline && shinyDisconnected) {
    setTimeout(function() { location.reload(); }, 2000);
  }
}

window.addEventListener('online', updateOnlineStatus);
window.addEventListener('offline', updateOnlineStatus);

// ============ NUCLEAR: kill ALL Shiny disconnect UI ============
function nukeShinyDisconnectUI() {
  // Remove overlays
  ['shiny-disconnected-overlay', 'shiny-reconnect-text'].forEach(function(id) {
    var el = document.getElementById(id);
    if (el) el.remove();
  });
  // Remove notification panel and all notifications
  document.querySelectorAll('.shiny-notification, #shiny-notification-panel, .shiny-notification-panel, .modal-backdrop').forEach(function(el) {
    el.remove();
  });
  // Unblock body
  document.body.style.pointerEvents = 'auto';
  document.body.style.filter = 'none';
  document.body.classList.remove('shiny-busy');
  // Remove any inline gray overlay Shiny injects
  document.querySelectorAll('[id*="disconnected"], [class*="disconnected"]').forEach(function(el) {
    el.style.display = 'none';
    el.style.visibility = 'hidden';
  });
}

// Run nuke on multiple events and with MutationObserver
$(document).on('shiny:disconnected shiny:error shiny:recalculating', function(event) {
  if (event.type === 'shiny:disconnected') {
    shinyDisconnected = true;
  }
  nukeShinyDisconnectUI();
  // Run again after delays (Shiny injects elements asynchronously)
  setTimeout(nukeShinyDisconnectUI, 50);
  setTimeout(nukeShinyDisconnectUI, 200);
  setTimeout(nukeShinyDisconnectUI, 500);
  setTimeout(nukeShinyDisconnectUI, 1500);
});

// MutationObserver: catch any disconnect element Shiny creates after our initial nuke
var shinyNukeObserver = new MutationObserver(function(mutations) {
  mutations.forEach(function(m) {
    m.addedNodes.forEach(function(node) {
      if (node.nodeType !== 1) return;
      var id = node.id || '';
      var cls = node.className || '';
      if (id.indexOf('disconnected') !== -1 || id.indexOf('shiny-notification') !== -1 ||
          cls.indexOf('shiny-notification') !== -1 || cls.indexOf('modal-backdrop') !== -1) {
        node.remove();
      }
    });
  });
});
shinyNukeObserver.observe(document.body, { childList: true, subtree: true });

// Override Shiny's disconnect behavior: keep UI visible
$(document).on('shiny:disconnected', function(event) {
  shinyDisconnected = true;
  nukeShinyDisconnectUI();

  // Show the offline banner
  var banner = document.getElementById('offline-banner');
  if (banner) banner.style.display = 'flex';

  // Load calendar from localStorage fallback
  loadCalendarFromCache();

  // If online, auto-reload after delay (likely server restart, not network issue)
  if (navigator.onLine) {
    setTimeout(function() { location.reload(); }, 4000);
  }
});

// ============ LOCALSTORAGE CALENDAR CACHE ============
// Handlers are registered inside shiny:connected (see below)

// Fallback: when disconnected, render cached calendar as static HTML
function loadCalendarFromCache() {
  try {
    var cached = localStorage.getItem('sp_calendar_events');
    var ts = localStorage.getItem('sp_calendar_ts');
    if (!cached) return;
    var events = JSON.parse(cached);
    if (!events || events.length === 0) return;

    // Find the calendar container and show a notice
    var calContainer = document.querySelector('.cal-container');
    if (!calContainer) return;

    var notice = document.createElement('div');
    notice.style.cssText = 'background:#fef3c7;color:#92400e;padding:8px 16px;border-radius:8px;margin-bottom:8px;font-size:0.8rem;font-weight:600;text-align:center;';
    notice.textContent = '📦 Vista offline — datos guardados el ' + (ts ? new Date(ts).toLocaleString() : 'anteriormente');
    calContainer.insertBefore(notice, calContainer.firstChild);

    console.log('[SP] Loaded', events.length, 'events from cache');
  } catch(e) {
    console.log('[SP] Cache load failed:', e);
  }
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
  var modeEl = document.getElementById('pomo-mode-label');
  var dotsEl = document.getElementById('pomo-dots');
  var sessEl = document.getElementById('pomo-sessions');
  var totalEl = document.getElementById('pomo-total');
  if (!el) return;

  var totalSec = Math.ceil(getTimeLeft() / 1000);
  el.textContent = String(Math.floor(totalSec / 60)).padStart(2, '0') + ':' + String(totalSec % 60).padStart(2, '0');
  el.style.color = pomo.mode === 'work' ? '#dc2626' : '#16a34a';

  if (modeEl) {
    modeEl.textContent = pomo.mode === 'work' ? '🎯 Tiempo de Estudio' :
      pomo.mode === 'short' ? '☕ Descanso Corto' : '🌴 Descanso Largo';
  }
  var toggleBtn = document.getElementById('pomo_toggle');
  if (toggleBtn) toggleBtn.textContent = pomo.running ? '⏸ Pausar' : '▶ Iniciar';

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
  if (sessEl) sessEl.textContent = pomo.sessions;
  if (totalEl) totalEl.textContent = (pomo.sessions * pomo.workMin) + ' min';
}

function pomoTick() {
  if (!pomo.running) return;
  var msLeft = pomo.endTimestamp - Date.now();
  if (msLeft <= 0) {
    pomo.running = false;
    pomo.pausedRemaining = null;
    if (pomo.mode === 'work') {
      pomo.sessions++;
      if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('pomo_session_done', pomo.sessions, {priority: 'event'});
      var isLong = pomo.sessions % 4 === 0;
      pomo.mode = isLong ? 'long' : 'short';
      pomo.durationSec = isLong ? pomo.longMin * 60 : pomo.shortMin * 60;
      pomo.pausedRemaining = pomo.durationSec * 1000;
      updateDisplay();
      showNotif(isLong ? '🌴 Descanso largo! Sesiones: ' + pomo.sessions : '☕ Descanso corto!', 'work_done');
    } else {
      pomo.mode = 'work';
      pomo.durationSec = pomo.workMin * 60;
      pomo.pausedRemaining = pomo.durationSec * 1000;
      updateDisplay();
      showNotif('🎯 A estudiar! (' + pomo.workMin + ' min)', 'break_done');
    }
    return;
  }
  updateDisplay();
  pomo.tickId = setTimeout(pomoTick, 1000);
}

function playSound(type) {
  try {
    var ctx = new (window.AudioContext || window.webkitAudioContext)();
    var now = ctx.currentTime;
    if (type === 'work_done') {
      [523.25, 659.25, 783.99].forEach(function(freq, i) {
        var o = ctx.createOscillator(), g = ctx.createGain();
        o.type = 'sine'; o.frequency.value = freq;
        g.gain.setValueAtTime(0, now + i*0.25);
        g.gain.linearRampToValueAtTime(0.4, now + i*0.25 + 0.05);
        g.gain.exponentialRampToValueAtTime(0.001, now + i*0.25 + 0.6);
        o.connect(g); g.connect(ctx.destination);
        o.start(now + i*0.25); o.stop(now + i*0.25 + 0.7);
      });
    } else {
      [880, 1108.73].forEach(function(freq, i) {
        var o = ctx.createOscillator(), g = ctx.createGain();
        o.type = 'triangle'; o.frequency.value = freq;
        g.gain.setValueAtTime(0, now + i*0.2);
        g.gain.linearRampToValueAtTime(0.5, now + i*0.2 + 0.03);
        g.gain.exponentialRampToValueAtTime(0.001, now + i*0.2 + 0.3);
        o.connect(g); g.connect(ctx.destination);
        o.start(now + i*0.2); o.stop(now + i*0.2 + 0.35);
      });
    }
    setTimeout(function() { ctx.close(); }, 3000);
  } catch(e) {}
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

// Helper: get numeric input value from Shiny's wrapper div
function getPomoDuration() {
  var input = document.querySelector('#pomo_duration input[type="number"]');
  return input ? (parseInt(input.value) || 25) : 25;
}

function pomoToggle() {
  if (pomo.running) {
    pomo.pausedRemaining = Math.max(0, pomo.endTimestamp - Date.now());
    pomo.running = false;
    clearTimeout(pomo.tickId);
    updateDisplay();
  } else {
    if (pomo.pausedRemaining === null) {
      pomo.workMin = getPomoDuration();
      pomo.durationSec = pomo.workMin * 60;
    }
    pomo.endTimestamp = Date.now() + (pomo.pausedRemaining !== null ? pomo.pausedRemaining : pomo.durationSec * 1000);
    pomo.pausedRemaining = null;
    pomo.running = true;
    updateDisplay();
    pomoTick();
  }
}

function pomoReset() {
  pomo.running = false; clearTimeout(pomo.tickId); pomo.mode = 'work';
  pomo.workMin = getPomoDuration();
  pomo.durationSec = pomo.workMin * 60; pomo.pausedRemaining = null;
  updateDisplay();
}

function pomoSkip() {
  pomo.running = false; clearTimeout(pomo.tickId);
  if (pomo.mode === 'work') {
    pomo.sessions++;
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('pomo_session_done', pomo.sessions, {priority: 'event'});
    pomo.mode = pomo.sessions % 4 === 0 ? 'long' : 'short';
    pomo.durationSec = pomo.mode === 'long' ? pomo.longMin * 60 : pomo.shortMin * 60;
  } else { pomo.mode = 'work'; pomo.durationSec = pomo.workMin * 60; }
  pomo.pausedRemaining = null; updateDisplay();
}

function pomoUndo() { if (pomo.sessions > 0) { pomo.sessions--; updateDisplay(); } }

// ============ AMBIENT MUSIC ============
function toggleMusic(type) {
  if (music.playing && music.type === type) { stopMusic(); return; }
  stopMusic(); music.type = type; music.playing = true;
  var ctx = new (window.AudioContext || window.webkitAudioContext)();
  music.ctx = ctx;
  var node = ctx.createScriptProcessor(4096, 1, 1);
  var gain = ctx.createGain(); gain.gain.value = music.vol;
  music.gain = gain; music.node = node;
  var b1 = 0;
  node.onaudioprocess = function(e) {
    var out = e.outputBuffer.getChannelData(0);
    for (var i = 0; i < 4096; i++) {
      var w = Math.random() * 2 - 1;
      if (type === 'white') out[i] = w;
      else if (type === 'brown') { b1 += w*0.02; b1 -= b1*0.02; out[i] = b1*3.5; }
      else { b1 = 0.99886*b1 + w*0.0555179; out[i] = b1*0.5; }
      out[i] = Math.max(-1, Math.min(1, out[i]));
    }
  };
  node.connect(gain); gain.connect(ctx.destination);
}
function stopMusic() {
  if (music.node) { music.node.disconnect(); music.node = null; }
  if (music.gain) { music.gain.disconnect(); music.gain = null; }
  if (music.ctx) { try { music.ctx.close(); } catch(e) {} music.ctx = null; }
  music.playing = false; music.type = null;
}

// ============ SHINY BINDINGS ============
// NOTE: Auto-login handlers are INLINE in app.R HTML (immune to SW cache)

$(document).on('shiny:connected', function() {
  shinyDisconnected = false;
  updateDisplay();
  updateOnlineStatus();

  // Listen on the actual input inside Shiny's wrapper
  $(document).on('change', '#pomo_duration input', function() {
    if (!pomo.running && pomo.mode === 'work') {
      pomo.workMin = parseInt(this.value) || 25;
      pomo.durationSec = pomo.workMin * 60;
      pomo.pausedRemaining = null;
      updateDisplay();
    }
  });
});

$(document).on('click', '#pomo_toggle', function(e) { e.preventDefault(); pomoToggle(); });
$(document).on('click', '#pomo_reset', function(e) { e.preventDefault(); pomoReset(); });
$(document).on('click', '#pomo_skip', function(e) { e.preventDefault(); pomoSkip(); });
$(document).on('click', '#pomo_undo', function(e) { e.preventDefault(); pomoUndo(); });
$(document).on('click', '#btn_install_app', function(e) { e.preventDefault(); triggerPWAInstall(); });
$(document).on('click', '#music_white', function(e) { e.preventDefault(); toggleMusic('white'); });
$(document).on('click', '#music_brown', function(e) { e.preventDefault(); toggleMusic('brown'); });
$(document).on('click', '#music_pink', function(e) { e.preventDefault(); toggleMusic('pink'); });
$(document).on('click', '#music_stop', function(e) { e.preventDefault(); stopMusic(); });
$(document).on('input', '#music-volume', function() {
  music.vol = this.value / 100;
  if (music.gain) music.gain.gain.value = music.vol;
});

// ============ CALENDAR DRAG-TO-MOVE ============
// Allows vertical dragging of .cal-event divs to change time
(function() {
  var dragState = null;
  var HOUR_H = 50; // Must match CSS and R

  $(document).on('mousedown touchstart', '.cal-event', function(e) {
    // Only allow drag with left mouse button or touch
    if (e.type === 'mousedown' && e.which !== 1) return;
    var el = this;
    var rect = el.getBoundingClientRect();
    var colRect = el.parentElement.getBoundingClientRect();
    var clientY = e.type === 'touchstart' ? e.originalEvent.touches[0].clientY : e.clientY;

    dragState = {
      el: el,
      startY: clientY,
      origTop: parseInt(el.style.top) || 0,
      colTop: colRect.top + window.scrollY,
      moved: false
    };
    el.style.zIndex = '10';
    el.style.opacity = '0.8';
    e.preventDefault();
  });

  $(document).on('mousemove touchmove', function(e) {
    if (!dragState) return;
    var clientY = e.type === 'touchmove' ? e.originalEvent.touches[0].clientY : e.clientY;
    var dy = clientY - dragState.startY;
    if (Math.abs(dy) > 5) dragState.moved = true;
    // Snap to 15-min increments (HOUR_H/4)
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
      // Calculate new time from pixel position
      var newTop = parseInt(dragState.el.style.top) || 0;
      var newHour = newTop / HOUR_H; // hours from CAL_FIRST_HOUR (0)
      var h = Math.floor(newHour);
      var m = Math.round((newHour - h) * 60 / 15) * 15;
      if (m >= 60) { h++; m = 0; }
      var newStart = String(h).padStart(2,'0') + ':' + String(m).padStart(2,'0');

      // Get event title from the element
      var nameEl = dragState.el.querySelector('.cal-ev-name');
      var timeEl = dragState.el.querySelector('.cal-ev-time');
      var title = nameEl ? nameEl.textContent : '';
      var oldTime = timeEl ? timeEl.textContent : '';

      // Calculate duration from old time display (e.g. "08:00 – 10:00")
      var parts = oldTime.split('–');
      var oldStartParts = (parts[0] || '').trim().split(':');
      var oldEndParts = (parts[1] || '').trim().split(':');
      var durMin = 60;
      if (oldStartParts.length === 2 && oldEndParts.length === 2) {
        durMin = (parseInt(oldEndParts[0])*60 + parseInt(oldEndParts[1])) -
                 (parseInt(oldStartParts[0])*60 + parseInt(oldStartParts[1]));
        if (durMin <= 0) durMin = 60;
      }
      var endMin = h * 60 + m + durMin;
      var endH = Math.floor(endMin / 60);
      var endM = endMin % 60;
      var newEnd = String(endH).padStart(2,'0') + ':' + String(endM).padStart(2,'0');

      // Update display
      if (timeEl) timeEl.textContent = newStart + ' – ' + newEnd;

      // Send to Shiny
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue('cal_drag_move', {
          title: title, new_start: newStart, new_end: newEnd,
          old_time: oldTime.trim()
        }, {priority: 'event'});
      }
    }
    dragState = null;
  });
})();
