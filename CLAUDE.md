# StudyPilot — notas para asistentes de código

App de estudio en R Shiny + MongoDB Atlas (mongolite) + Gemini (ellmer). Despliegue en shinyapps.io con `deploy.R`.

## Estructura
- `app.R` — carga variables de entorno, `global.R` y define `server()`; los archivos `server/*.R` se cargan con `source(local=TRUE)` y comparten `auth_user`, `uid`, `rv`, `acts()`, etc.
- `global.R` — librerías, carga `R/` y `ui/`, constantes del ciclo y helpers. Estado por usuario en `session$userData` (`get_courses()`, `get_pass_grade()`, `get_semester_start()`…). **Nunca** guardar datos de usuario en `globalenv()` (se comparte entre sesiones).
- `R/db_mongo.R` — única capa de acceso a datos; toda función filtra por `user_id`. Las consultas se construyen con `R/db_query.R` (jsonlite), **nunca** con `paste0` sobre texto del usuario.
- `R/pure_calc.R` — lógica pura testeada; referencia para una futura migración.
- `ui/`, `www/` — interfaz y PWA.

## Tests
R no está en el PATH. Usar R 4.5.3 (4.6.0 no tiene testthat):
```
"C:/Program Files/R/R-4.5.3/bin/Rscript.exe" tests/run_tests.R
"C:/Program Files/R/R-4.5.3/bin/Rscript.exe" tests/smoke_test.R
```
Correrlos tras cada cambio.

## Reglas
- No leer, mostrar ni copiar `.Renviron` ni `app_env` (secretos).
- Commits pequeños en español.
- Texto de usuario o de IA que se inserte en HTML debe escaparse (`htmltools::htmlEscape`) o pasar por `safe_md()`.
