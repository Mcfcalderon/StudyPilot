// ============ AUTO-RECONNECT ============
$(document).on('shiny:disconnected', function(event) {
  // Show friendly message and auto-reload
  var msg = document.createElement('div');
  msg.style.cssText = 'position:fixed;top:50%;left:50%;transform:translate(-50%,-50%);background:#1e293b;color:#fff;padding:24px 32px;border-radius:14px;z-index:99999;text-align:center;font-family:Inter,sans-serif;box-shadow:0 8px 30px rgba(0,0,0,.3)';
  msg.innerHTML = '<div style="font-size:1.5rem;margin-bottom:8px">🔄</div><div style="font-weight:600">Reconectando...</div><div style="font-size:0.85rem;opacity:0.7;margin-top:4px">Espera un momento</div>';
  document.body.appendChild(msg);
  setTimeout(function() { location.reload(); }, 2000);
});

// ============ POMODORO TIMER (client-side) ============
let pomo = {
  running: false,
  mode: 'work', // work, short, long
  timeLeft: 25 * 60,
  sessions: 0,
  intervalId: null,
  workMin: 25,
  shortMin: 5,
  longMin: 15
};

// Music state
let music = { playing: false, type: null, ctx: null, node: null, gain: null, vol: 0.3 };

// ============ TIMER DISPLAY ============
function updateDisplay() {
  const el = document.getElementById('pomo-timer');
  const modeEl = document.getElementById('pomo-mode-label');
  const dotsEl = document.getElementById('pomo-dots');
  const sessEl = document.getElementById('pomo-sessions');
  const totalEl = document.getElementById('pomo-total');
  if (!el) return;

  const mm = String(Math.floor(pomo.timeLeft / 60)).padStart(2, '0');
  const ss = String(pomo.timeLeft % 60).padStart(2, '0');
  el.textContent = mm + ':' + ss;
  el.style.color = pomo.mode === 'work' ? '#dc2626' : '#16a34a';

  if (modeEl) {
    modeEl.textContent = pomo.mode === 'work' ? '🎯 Tiempo de Estudio' :
      pomo.mode === 'short' ? '☕ Descanso Corto' : '🌴 Descanso Largo';
  }

  // Update toggle button text
  const toggleBtn = document.getElementById('pomo_toggle');
  if (toggleBtn) toggleBtn.textContent = pomo.running ? '⏸ Pausar' : '▶ Iniciar';

  // Dots
  if (dotsEl) {
    let dots = '';
    for (let i = 0; i < 4; i++) {
      let cls = 'pomo-dot';
      if (i < pomo.sessions % 4) cls += ' done';
      else if (i === pomo.sessions % 4 && pomo.running) cls += ' active';
      dots += '<span class="' + cls + '"></span>';
    }
    dotsEl.innerHTML = dots;
  }

  if (sessEl) sessEl.textContent = pomo.sessions;
  if (totalEl) totalEl.textContent = (pomo.sessions * pomo.workMin) + ' min';
}

// ============ NOTIFICATION SOUND ============
function playSound(type) {
  const ctx = new (window.AudioContext || window.webkitAudioContext)();
  const now = ctx.currentTime;

  if (type === 'work_done') {
    // Ascending chime C5-E5-G5 + C6
    [523.25, 659.25, 783.99].forEach((freq, i) => {
      const osc = ctx.createOscillator();
      const g = ctx.createGain();
      osc.type = 'sine';
      osc.frequency.value = freq;
      g.gain.setValueAtTime(0, now + i * 0.25);
      g.gain.linearRampToValueAtTime(0.4, now + i * 0.25 + 0.05);
      g.gain.exponentialRampToValueAtTime(0.001, now + i * 0.25 + 0.6);
      osc.connect(g); g.connect(ctx.destination);
      osc.start(now + i * 0.25); osc.stop(now + i * 0.25 + 0.7);
    });
    const osc2 = ctx.createOscillator();
    const g2 = ctx.createGain();
    osc2.type = 'sine'; osc2.frequency.value = 1046.5;
    g2.gain.setValueAtTime(0, now + 0.75);
    g2.gain.linearRampToValueAtTime(0.3, now + 0.85);
    g2.gain.exponentialRampToValueAtTime(0.001, now + 2.0);
    osc2.connect(g2); g2.connect(ctx.destination);
    osc2.start(now + 0.75); osc2.stop(now + 2.1);
  } else {
    // Two beeps for break done
    [880, 1108.73].forEach((freq, i) => {
      const osc = ctx.createOscillator();
      const g = ctx.createGain();
      osc.type = 'triangle'; osc.frequency.value = freq;
      g.gain.setValueAtTime(0, now + i * 0.2);
      g.gain.linearRampToValueAtTime(0.5, now + i * 0.2 + 0.03);
      g.gain.exponentialRampToValueAtTime(0.001, now + i * 0.2 + 0.3);
      osc.connect(g); g.connect(ctx.destination);
      osc.start(now + i * 0.2); osc.stop(now + i * 0.2 + 0.35);
    });
  }
  setTimeout(() => ctx.close(), 3000);
}

function showNotif(msg, type) {
  playSound(type);
  const el = document.createElement('div');
  el.className = 'pomo-notif ' + (type === 'work_done' ? 'notif-success' : 'notif-primary');
  el.textContent = msg;
  document.body.appendChild(el);
  setTimeout(() => { el.style.opacity = '0'; setTimeout(() => el.remove(), 500); }, 4000);

  if ('Notification' in window && Notification.permission === 'granted') {
    new Notification('🍅 Pomodoro', { body: msg });
  } else if ('Notification' in window && Notification.permission !== 'denied') {
    Notification.requestPermission();
  }
}

// ============ TIMER CONTROLS ============
function pomoToggle() {
  if (pomo.running) {
    clearInterval(pomo.intervalId);
    pomo.intervalId = null;
    pomo.running = false;
    updateDisplay();
  } else {
    // Read duration from Shiny input
    const durSel = document.getElementById('pomo_duration');
    if (durSel && !pomo.running && pomo.mode === 'work' && pomo.timeLeft === pomo.workMin * 60) {
      pomo.workMin = parseInt(durSel.value) || 25;
      pomo.timeLeft = pomo.workMin * 60;
    }
    pomo.running = true;
    updateDisplay();
    pomo.intervalId = setInterval(() => {
      pomo.timeLeft--;
      if (pomo.timeLeft <= 0) {
        clearInterval(pomo.intervalId);
        pomo.intervalId = null;
        pomo.running = false;
        if (pomo.mode === 'work') {
          pomo.sessions++;
          // Tell Shiny about completed session
          if (window.Shiny) Shiny.setInputValue('pomo_session_done', pomo.sessions, {priority: 'event'});
          const isLong = pomo.sessions % 4 === 0;
          pomo.mode = isLong ? 'long' : 'short';
          pomo.timeLeft = isLong ? pomo.longMin * 60 : pomo.shortMin * 60;
          updateDisplay();
          showNotif(isLong ?
            '🌴 Descanso largo (' + pomo.longMin + ' min)! Sesiones: ' + pomo.sessions :
            '☕ Descanso corto (' + pomo.shortMin + ' min)!', 'work_done');
        } else {
          pomo.mode = 'work';
          pomo.timeLeft = pomo.workMin * 60;
          updateDisplay();
          showNotif('🎯 A estudiar! (' + pomo.workMin + ' min)', 'break_done');
        }
      } else {
        updateDisplay();
      }
    }, 1000);
  }
}

function pomoReset() {
  clearInterval(pomo.intervalId);
  pomo.intervalId = null;
  pomo.running = false;
  pomo.mode = 'work';
  const durSel = document.getElementById('pomo_duration');
  pomo.workMin = durSel ? parseInt(durSel.value) || 25 : 25;
  pomo.timeLeft = pomo.workMin * 60;
  updateDisplay();
}

function pomoSkip() {
  clearInterval(pomo.intervalId);
  pomo.intervalId = null;
  pomo.running = false;
  if (pomo.mode === 'work') {
    pomo.sessions++;
    if (window.Shiny) Shiny.setInputValue('pomo_session_done', pomo.sessions, {priority: 'event'});
    pomo.mode = pomo.sessions % 4 === 0 ? 'long' : 'short';
    pomo.timeLeft = pomo.mode === 'long' ? pomo.longMin * 60 : pomo.shortMin * 60;
  } else {
    pomo.mode = 'work';
    pomo.timeLeft = pomo.workMin * 60;
  }
  updateDisplay();
}

function pomoUndo() {
  if (pomo.sessions > 0) {
    pomo.sessions--;
    updateDisplay();
  }
}

// ============ AMBIENT MUSIC ============
function toggleMusic(type) {
  if (music.playing && music.type === type) { stopMusic(); return; }
  stopMusic();
  music.type = type;
  music.playing = true;

  const ctx = new (window.AudioContext || window.webkitAudioContext)();
  music.ctx = ctx;
  const bufSize = 4096;
  const node = ctx.createScriptProcessor(bufSize, 1, 1);
  const gain = ctx.createGain();
  gain.gain.value = music.vol;
  music.gain = gain;
  music.node = node;

  let b1 = 0;
  node.onaudioprocess = function(e) {
    const out = e.outputBuffer.getChannelData(0);
    for (let i = 0; i < bufSize; i++) {
      const w = Math.random() * 2 - 1;
      if (type === 'white') out[i] = w;
      else if (type === 'brown') { b1 += w * 0.02; b1 -= b1 * 0.02; out[i] = b1 * 3.5; }
      else { b1 = 0.99886 * b1 + w * 0.0555179; out[i] = b1 * 0.5; }
      out[i] = Math.max(-1, Math.min(1, out[i]));
    }
  };
  node.connect(gain); gain.connect(ctx.destination);
}

function stopMusic() {
  if (music.node) { music.node.disconnect(); music.node = null; }
  if (music.gain) { music.gain.disconnect(); music.gain = null; }
  if (music.ctx) { music.ctx.close(); music.ctx = null; }
  music.playing = false; music.type = null;
}

// ============ SHINY BINDINGS ============
$(document).on('shiny:connected', function() {
  updateDisplay();

  // Duration change
  $(document).on('change', '#pomo_duration', function() {
    if (!pomo.running && pomo.mode === 'work') {
      pomo.workMin = parseInt(this.value) || 25;
      pomo.timeLeft = pomo.workMin * 60;
      updateDisplay();
    }
  });
});

// Button bindings via Shiny
$(document).on('click', '#pomo_toggle', function(e) { e.preventDefault(); pomoToggle(); });
$(document).on('click', '#pomo_reset', function(e) { e.preventDefault(); pomoReset(); });
$(document).on('click', '#pomo_skip', function(e) { e.preventDefault(); pomoSkip(); });
$(document).on('click', '#pomo_undo', function(e) { e.preventDefault(); pomoUndo(); });

// Music buttons
$(document).on('click', '#music_white', function(e) { e.preventDefault(); toggleMusic('white'); });
$(document).on('click', '#music_brown', function(e) { e.preventDefault(); toggleMusic('brown'); });
$(document).on('click', '#music_pink', function(e) { e.preventDefault(); toggleMusic('pink'); });
$(document).on('click', '#music_stop', function(e) { e.preventDefault(); stopMusic(); });

// Volume slider
$(document).on('input', '#music-volume', function() {
  music.vol = this.value / 100;
  if (music.gain) music.gain.gain.value = music.vol;
});
