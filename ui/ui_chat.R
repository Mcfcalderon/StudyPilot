# ============ ui_chat.R — Chat flotante FAB (position:fixed) ============
# Retorna un tagList, NO un nav_item. Se inyecta FUERA del page_navbar.

ui_chat <- function() {
  tagList(
    # ---- FAB Button (bottom-right, fixed) ----
    tags$div(id = "chat-float-btn",
      class = "chat-float-btn",
      onclick = "toggleChat()",
      icon("robot")
    ),

    # ---- Chat Panel (hidden by default) ----
    tags$div(id = "chat-float-panel",
      class = "chat-float-panel",

      # Header
      tags$div(class = "chat-float-header",
        tags$span(class = "fw-bold", icon("robot"), " Asistente IA"),
        tags$span(class = "chat-float-close", onclick = "toggleChat()", icon("xmark"))
      ),

      # Messages container
      tags$div(class = "chat-float-body",
        uiOutput("chat_display")
      ),

      # Input bar
      tags$div(class = "chat-float-input",
        tags$div(class = "d-flex gap-1",
          tags$div(style = "flex:1",
            textInput("chat_input", NULL, placeholder = "Pregunta algo...", width = "100%")),
          actionButton("chat_send", NULL, icon = icon("paper-plane"), class = "btn-sm btn-primary")
        )
      )
    ),

    # Toggle JS
    tags$script(HTML(
      "function toggleChat(){",
      "var p=document.getElementById('chat-float-panel');",
      "var b=document.getElementById('chat-float-btn');",
      "if(!p||!b)return;",
      "if(p.style.display==='flex'){",
      "  p.style.display='none';b.style.display='flex';",
      "}else{",
      "  p.style.display='flex';b.style.display='none';",
      "}}"
    ))
  )
}

