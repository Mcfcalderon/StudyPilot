// ============ SERVICE WORKER v2 — StudyPilot PWA (App Shell + Offline) ============
const CACHE_NAME = 'studypilot-v3';

// Install: skip waiting to activate immediately
self.addEventListener('install', function(event) {
  self.skipWaiting();
});

// Activate: claim all clients, clean old caches
self.addEventListener('activate', function(event) {
  event.waitUntil(
    caches.keys().then(function(keys) {
      return Promise.all(
        keys.filter(function(k) { return k !== CACHE_NAME; })
            .map(function(k) { return caches.delete(k); })
      );
    }).then(function() { return self.clients.claim(); })
  );
});

// Fetch: Strategy depends on request type
self.addEventListener('fetch', function(event) {
  var url = new URL(event.request.url);

  // Never intercept WebSocket or POST requests (Shiny comms)
  if (event.request.method !== 'GET') return;
  if (url.pathname.indexOf('websocket') !== -1) return;
  if (url.pathname.indexOf('__sockjs__') !== -1) return;
  if (url.protocol === 'ws:' || url.protocol === 'wss:') return;

  // Static assets (CSS, JS, fonts, images): Stale-While-Revalidate
  // Serve cached immediately, then fetch new version in background
  if (url.pathname.match(/\.(css|js|svg|png|jpg|ico|woff2?|ttf|eot)$/)) {
    event.respondWith(
      caches.open(CACHE_NAME).then(function(cache) {
        return cache.match(event.request).then(function(cached) {
          var fetchPromise = fetch(event.request).then(function(resp) {
            if (resp && resp.status === 200) {
              cache.put(event.request, resp.clone());
            }
            return resp;
          }).catch(function() {
            return cached || new Response('', { status: 503 });
          });
          return cached || fetchPromise;
        });
      })
    );
    return;
  }

  // HTML / Navigation requests: Network-First with App Shell fallback
  if (event.request.mode === 'navigate' ||
      event.request.headers.get('accept').indexOf('text/html') !== -1) {
    event.respondWith(
      fetch(event.request).then(function(resp) {
        // Cache the successful HTML response as the App Shell
        if (resp && resp.status === 200) {
          var clone = resp.clone();
          caches.open(CACHE_NAME).then(function(c) { c.put(event.request, clone); });
        }
        return resp;
      }).catch(function() {
        // Offline: serve cached App Shell OR a custom offline page
        return caches.match(event.request).then(function(cached) {
          if (cached) return cached;
          // Fallback: try the root URL cache
          return caches.match('./').then(function(root) {
            if (root) return root;
            // Last resort: inline offline page
            return new Response(
              '<!DOCTYPE html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width">' +
              '<title>StudyPilot - Offline</title>' +
              '<style>body{margin:0;min-height:100vh;display:flex;align-items:center;justify-content:center;' +
              'background:linear-gradient(135deg,#667eea,#764ba2);font-family:Inter,sans-serif;color:#fff;text-align:center}' +
              '.box{background:rgba(255,255,255,.12);border-radius:20px;padding:40px;backdrop-filter:blur(10px)}' +
              'h1{font-size:2rem;margin:0 0 8px}p{opacity:.8;margin:0 0 20px}' +
              'button{background:#fff;color:#6366f1;border:none;padding:12px 28px;border-radius:10px;' +
              'font-weight:700;font-size:1rem;cursor:pointer}</style></head>' +
              '<body><div class="box"><h1>🚀 StudyPilot</h1>' +
              '<p>Sin conexión. Reconectando automáticamente...</p>' +
              '<button onclick="location.reload()">🔄 Reintentar</button></div></body></html>',
              { headers: { 'Content-Type': 'text/html' } }
            );
          });
        });
      })
    );
    return;
  }

  // Shiny internal requests (__api__, session tokens): Network-only
  if (url.pathname.indexOf('__') !== -1) return;

  // Everything else: Network-first with cache fallback
  event.respondWith(
    fetch(event.request).then(function(resp) {
      if (resp && resp.status === 200 && resp.type !== 'opaque') {
        var clone = resp.clone();
        caches.open(CACHE_NAME).then(function(c) { c.put(event.request, clone); });
      }
      return resp;
    }).catch(function() {
      return caches.match(event.request).then(function(cached) {
        return cached || new Response('', { status: 503 });
      });
    })
  );
});
