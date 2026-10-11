# ============ R/safe_html.R — Mostrar texto de usuario/IA sin XSS ============
# El texto de la IA (que puede venir de un PDF manipulado) y los apuntes del
# usuario se convertian con markdownToHTML, que deja pasar <script>, onerror=
# y enlaces javascript:. safe_md() escapa el HTML crudo antes de convertir.

esc <- function(x) htmltools::htmlEscape(as.character(x), attribute = TRUE)

# Escapa "<" y "&" fuera de bloques ``` y de `codigo en linea` (alli el
# conversor ya escapa). Asi los diagramas mermaid siguen funcionando.
.md_escape_raw_html <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  in_fence <- FALSE
  for (i in seq_along(lines)) {
    l <- lines[i]
    if (grepl("^\\s*(```|~~~)", l)) { in_fence <- !in_fence; next }
    if (in_fence) next
    parts <- strsplit(l, "`", fixed = TRUE)[[1]]
    if (length(parts) == 0) next
    out_idx <- seq(1, length(parts), by = 2)          # segmentos fuera de `...`
    parts[out_idx] <- gsub("<", "&lt;", gsub("&(?![#A-Za-z0-9]+;)", "&amp;", parts[out_idx], perl = TRUE), fixed = TRUE)
    lines[i] <- paste0(paste(parts, collapse = "`"), if (endsWith(l, "`")) "`" else "")
  }
  paste(lines, collapse = "\n")
}

safe_md <- function(text) {
  if (is.null(text) || length(text) == 0 || is.na(text[1])) return(htmltools::HTML(""))
  html <- tryCatch(
    markdown::markdownToHTML(text = .md_escape_raw_html(as.character(text[1])), fragment.only = TRUE),
    error = function(e) paste0("<pre>", esc(text[1]), "</pre>"))
  # Enlaces/imagenes con esquemas peligrosos
  html <- gsub('(href|src)\\s*=\\s*"\\s*(javascript|vbscript|data):[^"]*"', '\\1="#"',
               html, ignore.case = TRUE, perl = TRUE)
  htmltools::HTML(html)
}
