# ============ ui_actividades.R — Panel de Actividades + Vista Semanal ============

ui_actividades <- function() {
  nav_panel(
    title = "Actividades",

    # ---- Filters ----
    layout_columns(
      col_widths = breakpoints(sm = c(6, 6, 6, 6), lg = c(3, 3, 3, 3)),
      selectInput("act_filter_course", "Curso:",
        choices = c("Todos" = "all", setNames(courses$id, courses$short))),
      selectInput("act_filter_type", "Tipo:",
        choices = c("Todos" = "all", "Examen" = "examen", "Proyecto" = "proyecto",
                    "EC" = "ec", "Quiz/PC" = "quiz")),
      selectInput("act_filter_status", "Estado:",
        choices = c("Pendientes" = "pending", "Todas" = "all", "Completadas" = "done")),
      selectInput("act_filter_pri", "Prioridad:",
        choices = c("Todas" = "all", "Alta >=20%" = "high",
                    "Media" = "medium", "Baja" = "low"))
    ),

    # ---- Action buttons ----
    div(class = "d-flex flex-wrap gap-2 mb-3",
      actionButton("act_new", "+ Nueva Actividad", class = "btn-primary"),
      actionButton("act_mark_done", "Marcar hechas", class = "btn-outline-success btn-sm"),
      actionButton("act_edit", "Editar", class = "btn-outline-secondary btn-sm"),
      actionButton("act_delete", "Eliminar", class = "btn-outline-danger btn-sm")
    ),

    # ---- DataTable ----
    div(style = "overflow-x:auto; -webkit-overflow-scrolling:touch;",
      DTOutput("activities_table")
    ),

    # ---- Vista semanal integrada ----
    div(class = "card mt-4",
      div(class = "card-header py-2 d-flex align-items-center gap-3",
        tags$b("Vista Semanal"),
        actionButton("week_prev", "<", class = "btn-sm btn-outline-primary py-0"),
        uiOutput("week_title"),
        actionButton("week_next", ">", class = "btn-sm btn-outline-primary py-0")
      ),
      div(class = "card-body p-2",
        uiOutput("week_view")
      )
    )
  )
}

