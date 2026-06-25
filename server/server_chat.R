# ============ server_chat.R — Chat flotante IA ============
# Sourced con local=TRUE.
# rv_chat definido aqui (scope local al server).

rv_chat <- reactiveValues(messages = list())

observeEvent(input$chat_send, {
  q <- trimws(input$chat_input)
  if (nchar(q) < 2) return()

  rv_chat$messages <- c(rv_chat$messages, list(list(role = "user", text = q)))
  updateTextInput(session, "chat_input", value = "")

  # Show thinking indicator
  rv_chat$messages <- c(rv_chat$messages, list(list(role = "ai", text = "Pensando...")))

  tryCatch({
    chat <- get_gemini()
    prompt <- paste0(
      "Eres un tutor academico de apoyo universitario. ",
      "Responde de forma clara, concisa y en espanol. ",
      "Si es un concepto, da definicion + ejemplo practico. ",
      "Pregunta del estudiante: ", q
    )
    response <- chat$chat(prompt)
    message("[StudyPilot] AI chat response length: ", nchar(response))
    msgs <- rv_chat$messages
    msgs[[length(msgs)]] <- list(role = "ai", text = as.character(response))
    rv_chat$messages <- msgs
  }, error = function(e) {
    message("[StudyPilot] AI chat ERROR: ", e$message)
    err_msg <- if (grepl("429", e$message)) {
      "La IA esta temporalmente saturada. Espera 1-2 minutos."
    } else if (grepl("503", e$message)) {
      "El servicio de IA no esta disponible. Intenta de nuevo."
    } else {
      paste0("Error: ", e$message)
    }
    msgs <- rv_chat$messages
    msgs[[length(msgs)]] <- list(role = "ai", text = err_msg)
    rv_chat$messages <- msgs
  })
})

output$chat_display <- renderUI({
  msgs <- rv_chat$messages
  msg_divs <- lapply(msgs, function(m) {
    cls <- if (m$role == "user") "chat-msg chat-user" else "chat-msg chat-ai"
    if (m$role == "ai") {
      tags$div(class = cls, HTML(tryCatch(
        markdown::markdownToHTML(text = m$text, fragment.only = TRUE),
        error = function(e) m$text
      )))
    } else {
      tags$div(class = cls, m$text)
    }
  })
  tags$div(class = "chat-container", id = "chat_messages_inner", msg_divs,
    tags$script(HTML(
      "var c=document.getElementById('chat_messages_inner');if(c)c.scrollTop=c.scrollHeight;"
    )))
})

# Render even when chat panel is hidden (display:none)
outputOptions(output, "chat_display", suspendWhenHidden = FALSE)

