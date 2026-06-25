# ============ ui_login.R — Auth overlay (Login + Register) ============
# Anti-flicker: login_form_container hidden by default.
# JS shows it only if no saved session in localStorage.

ui_login <- function() {
  tagList(
    # Auth-specific CSS
    tags$head(
      tags$link(rel = "stylesheet",
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"),
      tags$style(HTML("
        #auth_overlay {
          position: fixed; top: 0; left: 0; width: 100vw; height: 100vh;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          z-index: 9999; display: flex; align-items: center; justify-content: center;
        }
        .auth-card {
          background: white; border-radius: 16px; padding: 40px 36px; width: 400px;
          max-width: 92vw; box-shadow: 0 20px 60px rgba(0,0,0,0.3);
        }
        .auth-card h2 { font-weight: 800; margin-bottom: 4px; }
        .auth-card .subtitle { color: #6b7280; margin-bottom: 24px; }
        .auth-card .form-label { font-weight: 600; font-size: 0.9rem; }
        .auth-card .form-control { border-radius: 8px; padding: 10px 14px; }
        .auth-card .btn-primary {
          width: 100%; padding: 12px; border-radius: 10px; font-weight: 600;
          font-size: 1rem; background: #2563eb; border: none;
        }
        .auth-card .btn-primary:hover { background: #1d4ed8; }
        .auth-toggle { color: #2563eb; cursor: pointer; font-weight: 500; }
        .auth-toggle:hover { text-decoration: underline; }
        .auth-msg { font-size: 0.85rem; margin-top: 8px; padding: 8px 12px; border-radius: 8px; }
        .auth-msg.error { background: #fef2f2; color: #dc2626; border: 1px solid #fecaca; }
        .auth-msg.success { background: #f0fdf4; color: #16a34a; border: 1px solid #bbf7d0; }
      "))
    ),

    # ---- Auth overlay ----
    div(id = "auth_overlay",
      div(class = "auth-card",
        # Login panel
        div(id = "login_panel",
          tags$div(style = "text-align:center",
            tags$h2("\U0001F680 StudyPilot"),
            tags$p(class = "subtitle", "Plataforma de estudio inteligente")
          ),
          # Anti-flicker: hidden by default, JS shows if no saved session
          div(id = "login_form_container", style = "display:none;",
            tags$h5(class = "fw-bold mb-3", "Iniciar sesión"),
            div(class = "mb-3",
              tags$label(class = "form-label", "Usuario:"),
              textInput("login_user", NULL, placeholder = "Tu usuario", width = "100%")
            ),
            div(class = "mb-3",
              tags$label(class = "form-label", "Contraseña:"),
              passwordInput("login_pass", NULL, width = "100%")
            ),
            actionButton("login_btn", "Iniciar sesión", class = "btn btn-primary mt-1"),
            div(id = "login_msg_div"),
            tags$div(class = "text-center mt-3",
              tags$p(class = "small text-muted mb-0", "\u00bfNo tienes cuenta?"),
              tags$span(class = "auth-toggle",
                onclick = "toggleAuthPanel('register')", "Crear cuenta")
            )
          )
        ),

        # Register panel (hidden initially)
        div(id = "register_panel", style = "display:none;",
          tags$div(style = "text-align:center",
            tags$h2("\U0001F680 StudyPilot"),
            tags$p(class = "subtitle", "Crear nueva cuenta")
          ),
          div(class = "mb-2",
            tags$label(class = "form-label", "Nombre completo:"),
            textInput("reg_name", NULL, placeholder = "Ej: María López", width = "100%")
          ),
          div(class = "mb-2",
            tags$label(class = "form-label", "Usuario:"),
            textInput("reg_user", NULL, placeholder = "Ej: maria.lopez", width = "100%")
          ),
          div(class = "mb-2",
            tags$label(class = "form-label", "Contraseña:"),
            passwordInput("reg_pass", NULL, width = "100%")
          ),
          div(class = "mb-3",
            tags$label(class = "form-label", "Confirmar contraseña:"),
            passwordInput("reg_pass2", NULL, width = "100%")
          ),
          actionButton("register_btn", "Crear cuenta", class = "btn btn-primary mt-1"),
          div(id = "register_msg_div"),
          tags$div(class = "text-center mt-3",
            tags$span(class = "auth-toggle",
              onclick = "toggleAuthPanel('login')", "\u2190 Ya tengo cuenta")
          )
        )
      )
    ),

    # JS: toggle login/register panels + Enter key handlers
    tags$script(HTML("
      function toggleAuthPanel(panel) {
        if (panel === 'register') {
          document.getElementById('login_panel').style.display = 'none';
          document.getElementById('register_panel').style.display = 'block';
        } else {
          document.getElementById('register_panel').style.display = 'none';
          document.getElementById('login_panel').style.display = 'block';
          document.getElementById('login_form_container').style.display = 'block';
        }
      }
      document.addEventListener('keydown', function(e) {
        if (e.key !== 'Enter') return;
        var id = (e.target || {}).id || '';
        if (id === 'login_user' || id === 'login_pass') {
          e.preventDefault(); document.getElementById('login_btn').click();
        } else if (id === 'reg_name' || id === 'reg_user' || id === 'reg_pass' || id === 'reg_pass2') {
          e.preventDefault(); document.getElementById('register_btn').click();
        } else if (id === 'chat_input') {
          e.preventDefault(); document.getElementById('chat_send').click();
        }
      });
    "))
  )
}
