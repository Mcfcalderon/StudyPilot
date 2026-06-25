# ============ ui_navbar.R — Estructura principal de navegación ============
# Retorna la UI completa: page_navbar + auth overlay + head tags

ui_navbar <- function() {
  # ---- Navbar principal con pestañas ----
  main_navbar <- page_navbar(
    title = tags$span(
      tags$span("🚀", style = "margin-right:6px"),
      "StudyPilot",
      tags$span("", style = "color:#93c5fd;margin-left:6px;font-weight:400")
    ),
    theme = bs_theme(
      version = 5,
      primary = "#2563eb", success = "#16a34a",
      danger = "#dc2626", warning = "#eab308",
      info = "#0891b2", secondary = "#64748b",
      "font-size-base" = "0.88rem",
      "body-bg" = "#f1f5f9",
      "card-border-width" = "0",
      "border-radius" = "0.75rem",
      "nav-link-font-weight" = "500"
    ),
    header = tags$head(
      tags$link(rel = "stylesheet", href = paste0("custom.css?v=", format(Sys.time(), "%Y%m%d%H%M"))),
      tags$script(src = "pomodoro.js"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.min.js"),
      tags$script(HTML("mermaid.initialize({startOnLoad:false, theme:'default', securityLevel:'loose'});"))
    ),
    id = "main_nav",
    bg = "#2563eb",

    # ---- Pestañas (cada una viene de su propio ui/*.R) ----
    ui_dashboard(),
    ui_pomodoro(),
    ui_examen(),
    ui_actividades(),
    ui_notas(),
    ui_calendario(),
    ui_cursos(),
    ui_analytics(),

    nav_spacer(),

    # Botón instalar PWA
    nav_item(
      actionButton("btn_install_app", label = tags$span(
        tags$i(class = "bi bi-download", style = "margin-right:4px"),
        "Instalar App"
      ), class = "btn-sm",
      style = "display:none;background:linear-gradient(135deg,#6366f1,#8b5cf6);color:white;border:none;font-size:0.78rem;font-weight:600;border-radius:8px;padding:4px 12px;")
    ),

    # Logout
    nav_item(
      actionLink("logout_btn", label = tags$span(
        tags$i(class = "bi bi-box-arrow-right", style = "margin-right:4px"),
        "Cerrar sesión"
      ), style = "color:white;opacity:0.9;font-size:0.85rem;text-decoration:none;cursor:pointer;")
    ),


    # Info en navbar
    nav_item(
      tags$span(class = "navbar-text text-white",
        tags$span(class = "d-none d-md-inline", style = "font-size:0.82rem;opacity:0.9",
          paste0("📅 ", format(Sys.Date(), "%A %d %b %Y"), "  ·  📍 Semana ", current_week(), "/", TOTAL_WEEKS)
        )
      )
    )
  )

  # ---- Wrapper con auth overlay + offline banner + PWA meta + inline JS ----
  fluidPage(
    useShinyjs(),
    # Offline banner
    tags$div(id = "offline-banner",
      tags$span("⚠️"), tags$span("Modo Offline — Los botones de IA y sincronización están desactivados.")
    ),
    # PWA meta tags
    tags$head(
      tags$link(rel = "manifest", href = "pwa-manifest.json"),
      tags$meta(name = "theme-color", content = "#1e293b"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, viewport-fit=cover"),
      tags$meta(name = "mobile-web-app-capable", content = "yes"),
      tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
      tags$meta(name = "apple-mobile-web-app-status-bar-style", content = "black-translucent"),
      tags$meta(name = "apple-mobile-web-app-title", content = "StudyPilot"),
      tags$link(rel = "apple-touch-icon", href = "icon-512.svg"),
      tags$link(rel = "icon", type = "image/svg+xml", href = "icon-512.svg"),
      # Inline auto-login + message handlers (immune to SW cache)
      tags$script(HTML("
        var _spReady = false;
        $(document).on('shiny:connected', function() {
          if (!_spReady) {
            _spReady = true;
            Shiny.addCustomMessageHandler('save_session', function(d) {
              try {
                localStorage.setItem('sp_user', d.user);
                localStorage.setItem('sp_token', d.token);
                console.log('[SP] Session SAVED:', d.user);
              } catch(e) { console.error('[SP] save err:', e); }
            });
            Shiny.addCustomMessageHandler('clear_session', function(d) {
              localStorage.removeItem('sp_user');
              localStorage.removeItem('sp_token');
              localStorage.removeItem('sp_active_tab');
              console.log('[SP] Session CLEARED');
            });
            Shiny.addCustomMessageHandler('tab_changed', function(d) {
              try { localStorage.setItem('sp_active_tab', d.tab); } catch(e) {}
            });
            Shiny.addCustomMessageHandler('cache_calendar', function(d) {
              try {
                localStorage.setItem('sp_calendar_events', JSON.stringify(d.events));
                localStorage.setItem('sp_calendar_ts', new Date().toISOString());
              } catch(e) {}
            });
            Shiny.addCustomMessageHandler('execute_logout', function(d) {
              localStorage.removeItem('sp_user');
              localStorage.removeItem('sp_token');
              localStorage.removeItem('sp_active_tab');
              localStorage.removeItem('sp_calendar_events');
              localStorage.removeItem('sp_calendar_ts');
              console.log('[SP] LOGOUT: cleared, reloading...');
              window.location.reload(true);
            });
            console.log('[SP] Handlers registered (inline)');
          }
          var u = localStorage.getItem('sp_user');
          var t = localStorage.getItem('sp_token');
          var tab = localStorage.getItem('sp_active_tab');
          if (u && t) {
            $('#login_form_container').hide();
            setTimeout(function() {
              console.log('[SP] Sending auto_login for:', u);
              Shiny.setInputValue('auto_login', {user:u, token:t, tab:tab||''}, {priority:'event'});
            }, 500);
          } else {
            var fc = document.getElementById('login_form_container');
            if (fc) { fc.style.display = 'block'; fc.style.opacity = '1'; }
            console.log('[SP] No saved session, showing login form');
          }
        });
        setInterval(function() {
          if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket) {
            Shiny.setInputValue('keep_alive', Date.now(), {priority:'event'});
          }
        }, 15000);
      "))
    ),
    # Auth overlay
    ui_login(),
    # Main app (hidden until login)
    div(id = "main_app", style = "display:none;", main_navbar, ui_chat())
  )
}
