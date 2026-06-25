# ============ server_actividades.R — Actividades CRUD ============
# Sourced con local=TRUE.
# REGLA: Todo CRUD (crear/editar/eliminar) hace rv$refresh <- rv$refresh + 1
# para que Dashboard KPIs y Smart Scheduler se enteren inmediatamente.

# ====================================================================
# ACTIVITIES TABLE (DataTable with filters)
# ====================================================================
output$activities_table <- renderDT({
  a <- acts()
  if (input$act_filter_course != "all") a <- a |> filter(course_id == input$act_filter_course)
  if (input$act_filter_type != "all") a <- a |> filter(type == input$act_filter_type)
  if (input$act_filter_status == "pending") a <- a |> filter(done == 0)
  if (input$act_filter_status == "done") a <- a |> filter(done == 1)
  if (input$act_filter_pri == "high") a <- a |> filter(weight >= 20)
  if (input$act_filter_pri == "medium") a <- a |> filter(weight >= 10, weight < 20)
  if (input$act_filter_pri == "low") a <- a |> filter(weight < 10)

  a <- a |>
    mutate(
      Curso = ifelse(course_id == "_personal", "Personal",
                     ifelse(course_id %in% courses$id,
                            courses$short[match(course_id, courses$id)], course_id)),
      Dias = as.integer(as.Date(date) - Sys.Date()),
      Estado = ifelse(done == 1, "OK", ifelse(Dias < 0, "!", " ")),
      Prioridad = priority_class(weight)
    ) |>
    select(ID = act_id, Estado, Curso, Actividad = name, Tipo = type,
           `Peso%` = weight, Semana = week, Fecha = date, Dias, Prioridad)

  datatable(a,
    options = list(pageLength = 15, order = list(list(8, "asc")), dom = "frtip"),
    rownames = FALSE, selection = "multiple") |>
    formatStyle("Dias",
      color = styleInterval(c(0, 3, 7), c("#dc2626", "#dc2626", "#ca8a04", "#16a34a"))) |>
    formatStyle("Peso%", fontWeight = "bold") |>
    formatStyle("Prioridad",
      backgroundColor = styleEqual(c("high", "medium", "low"), c("#fef2f2", "#fffbeb", "#f0fdf4")))
})

# Helper: get filtered activities (same filters as table)
filtered_acts <- function() {
  a <- acts()
  if (input$act_filter_course != "all") a <- a[a$course_id == input$act_filter_course, ]
  if (input$act_filter_type != "all") a <- a[a$type == input$act_filter_type, ]
  if (input$act_filter_status == "pending") a <- a[a$done == 0, ]
  if (input$act_filter_status == "done") a <- a[a$done == 1, ]
  if (input$act_filter_pri == "high") a <- a[a$weight >= 20, ]
  if (input$act_filter_pri == "medium") a <- a[a$weight >= 10 & a$weight < 20, ]
  if (input$act_filter_pri == "low") a <- a[a$weight < 10, ]
  a[order(a$date), ]
}

# ====================================================================
# NEW ACTIVITY — Modal with is_calificada checkbox
# ====================================================================
observeEvent(input$act_new, {
  course_choices <- c("Personal (sin curso)" = "_personal")
  if (nrow(courses) > 0) course_choices <- c(course_choices, setNames(courses$id, courses$short))
  showModal(modalDialog(
    title = "Nueva Actividad",
    textInput("new_act_name", "Nombre:",
      placeholder = "Ej: Entregar informe, Repaso de capitulos..."),
    selectInput("new_act_course", "Curso (opcional):", choices = course_choices),
    selectInput("new_act_type", "Tipo:",
      choices = c("tarea", "ec", "examen", "proyecto", "quiz", "repaso", "personal")),
    checkboxInput("new_act_calificada",
      "Es evaluacion calificada (cuenta para la nota)", value = TRUE),
    conditionalPanel(
      condition = "input.new_act_calificada == true",
      numericInput("new_act_weight", "Peso (%):", value = 0, min = 0, max = 100)
    ),
    dateInput("new_act_date", "Fecha limite:", value = Sys.Date() + 7),
    uiOutput("new_act_topics_ui"),
    textInput("new_act_notes", "Notas:", placeholder = "Opcional"),
    footer = tagList(
      modalButton("Cancelar"),
      actionButton("new_act_save", "Crear", class = "btn-primary")
    ),
    easyClose = TRUE
  ))
})

# Dynamic topics selector
output$new_act_topics_ui <- renderUI({
  cid <- input$new_act_course
  if (is.null(cid) || cid == "_personal") return(NULL)
  ct <- get0("course_topics", envir = globalenv())
  topics <- if (!is.null(ct) && cid %in% names(ct)) ct[[cid]] else character(0)
  if (length(topics) == 0) return(NULL)
  selectizeInput("new_act_topics", "Temas vinculados:",
    choices = topics, selected = NULL, multiple = TRUE,
    options = list(placeholder = "Selecciona temas (opcional)..."))
})

observeEvent(input$new_act_save, {
  if (is.null(input$new_act_name) || nchar(input$new_act_name) == 0) {
    showNotification("Escribe un nombre para la actividad", type = "warning")
    return()
  }
  act_course <- if (input$new_act_course == "_personal") "_personal" else input$new_act_course
  temas_new <- input$new_act_topics
  is_cal <- isTRUE(input$new_act_calificada)
  act_weight <- if (is_cal) input$new_act_weight else 0
  mg_activity_add(uid(), act_course, input$new_act_type,
    input$new_act_name, as.character(input$new_act_date),
    act_weight, input$new_act_notes, temas = temas_new, is_calificada = is_cal)
  # TRIGGER GLOBAL: Dashboard KPIs + Smart Scheduler se enteran
  rv$refresh <- rv$refresh + 1
  removeModal()
  showNotification(paste0("Actividad \"", input$new_act_name, "\" creada"), type = "message")
})


# ====================================================================
# MARK DONE (multiple selection)
# ====================================================================
observeEvent(input$act_mark_done, {
  sel <- input$activities_table_rows_selected
  if (length(sel) == 0) {
    showNotification("Selecciona actividades primero", type = "warning")
    return()
  }
  af <- filtered_acts()
  count <- 0
  for (s in sel) {
    if (s <= nrow(af)) {
      row <- af[s, ]
      aid <- if ("act_id" %in% names(row)) row$act_id else s
      new_done <- 1L - row$done
      mg_activity_toggle(uid(), aid, new_done)
      count <- count + 1
    }
  }
  rv$refresh <- rv$refresh + 1
  showNotification(paste0(count, " actividad(es) actualizada(s)"), type = "message", duration = 3)
})

# ====================================================================
# EDIT ACTIVITY — preserves is_calificada checkbox
# ====================================================================
observeEvent(input$act_edit, {
  sel <- input$activities_table_rows_selected
  if (length(sel) == 0) {
    showNotification("Selecciona una actividad primero", type = "warning"); return()
  }
  if (length(sel) > 1) {
    showNotification("Selecciona solo una actividad para editar", type = "warning"); return()
  }
  af <- filtered_acts()
  if (sel[1] > nrow(af)) return()
  row <- af[sel[1], ]

  cid_edit <- row$course_id
  ct <- get0("course_topics", envir = globalenv())
  available_topics <- if (!is.null(ct) && cid_edit %in% names(ct)) ct[[cid_edit]] else character(0)
  current_topics <- tryCatch(mg_activity_get_topics(uid(), row$act_id), error = function(e) character(0))

  # Determine if currently calificada
  is_cal_current <- TRUE
  if ("is_calificada" %in% names(row)) is_cal_current <- isTRUE(row$is_calificada)
  else if (row$weight == 0) is_cal_current <- FALSE

  showModal(modalDialog(
    title = paste0("Editar: ", row$name),
    textInput("edit_act_name", "Nombre:", value = row$name),
    selectInput("edit_act_type", "Tipo:",
      choices = c("ec", "examen", "proyecto", "quiz", "tarea", "repaso", "personal"),
      selected = row$type),
    checkboxInput("edit_act_calificada",
      "Es evaluacion calificada (cuenta para la nota)", value = is_cal_current),
    conditionalPanel(
      condition = "input.edit_act_calificada == true",
      numericInput("edit_act_weight", "Peso (%):", value = row$weight, min = 0, max = 100)
    ),
    dateInput("edit_act_date", "Fecha:", value = as.Date(row$date)),
    if (length(available_topics) > 0)
      selectizeInput("edit_act_topics", "Temas vinculados:",
        choices = available_topics, selected = current_topics,
        multiple = TRUE,
        options = list(placeholder = "Selecciona temas que entran en esta evaluacion..."))
    else
      tags$p(class = "text-muted small", "Sin temas disponibles. Sube el silabo del curso."),
    footer = tagList(
      modalButton("Cancelar"),
      actionButton("edit_act_save", "Guardar", class = "btn-primary")
    ),
    easyClose = TRUE
  ))
  rv$editing_act_id <- row$act_id
})

observeEvent(input$edit_act_save, {
  aid <- rv$editing_act_id
  temas_sel <- input$edit_act_topics
  is_cal <- isTRUE(input$edit_act_calificada)
  act_weight <- if (is_cal) input$edit_act_weight else 0
  mg_activity_update(uid(), aid, input$edit_act_name, act_weight,
    as.character(input$edit_act_date), type = input$edit_act_type,
    temas = temas_sel, is_calificada = is_cal)
  # TRIGGER GLOBAL
  rv$refresh <- rv$refresh + 1
  removeModal()
  label <- if (is_cal) "calificada" else "formativa"
  n_temas <- if (!is.null(temas_sel)) length(temas_sel) else 0
  showNotification(paste0("Actividad ", label, " actualizada",
    if (n_temas > 0) paste0(" (", n_temas, " temas)") else ""), type = "message")
})

# ====================================================================
# DELETE ACTIVITIES (multiple)
# ====================================================================
observeEvent(input$act_delete, {
  sel <- input$activities_table_rows_selected
  if (length(sel) == 0) {
    showNotification("Selecciona actividades primero", type = "warning"); return()
  }
  af <- filtered_acts()
  names_list <- sapply(sel, function(s) if (s <= nrow(af)) af$name[s] else "")
  names_list <- names_list[nchar(names_list) > 0]
  showModal(modalDialog(
    title = paste0("Eliminar ", length(names_list), " actividad(es)"),
    tags$ul(lapply(names_list, function(n) tags$li(n))),
    tags$p(class = "text-danger", "Esta accion no se puede deshacer."),
    footer = tagList(
      modalButton("Cancelar"),
      actionButton("delete_act_confirm", "Eliminar", class = "btn-danger")
    ),
    easyClose = TRUE
  ))
  rv$deleting_act_ids <- sapply(sel, function(s) if (s <= nrow(af)) af$act_id[s] else NA)
  rv$deleting_act_ids <- rv$deleting_act_ids[!is.na(rv$deleting_act_ids)]
})

observeEvent(input$delete_act_confirm, {
  for (aid in rv$deleting_act_ids) {
    mg_activity_delete(uid(), aid)
  }
  # TRIGGER GLOBAL
  rv$refresh <- rv$refresh + 1
  removeModal()
  showNotification(paste0(length(rv$deleting_act_ids), " actividad(es) eliminada(s)"), type = "warning")
})
