# StudyPilot

Plataforma de estudio inteligente para gestion academica universitaria, construida con R Shiny.

## Funcionalidades

- **Dashboard** — KPIs de progreso, timeline semestral, countdown de examenes
- **Calendario** — Vista semanal con eventos de Google Calendar + bloques de estudio IA
- **Smart Scheduler** — Genera bloques de estudio automaticamente segun prioridad (Sys.time() implacable)
- **Cursos** — Extraccion automatica de silabos PDF con IA (Gemini)
- **Notas** — Registro de calificaciones con promedio ponderado por creditos
- **Examenes** — Generador de examenes de practica con IA
- **Pomodoro** — Timer de estudio integrado
- **Analytics** — Deuda academica y estado de preparacion por examen
- **Chat IA** — Asistente de estudio con Gemini

## Requisitos

- R >= 4.4
- Cuenta en [MongoDB Atlas](https://cloud.mongodb.com/) (tier gratuito funciona)
- API Key de [Google Gemini](https://aistudio.google.com/apikey)

## Instalacion

```r
# 1. Instalar paquetes
install.packages(c(
  "shiny", "bslib", "dplyr", "lubridate", "DT", "htmltools",
  "markdown", "shinyjs", "sodium", "digest", "mongolite",
  "ellmer", "pdftools"
))

# 2. Clonar el repositorio
# git clone https://github.com/Mcfcalderon/StudyPilot.git

# 3. Configurar variables de entorno
# Copia .Renviron.example como .Renviron y llena tus credenciales:
# cp .Renviron.example .Renviron

# 4. Correr la app
shiny::runApp()
```

## Estructura del proyecto

```
StudyPilot/
├── app.R              # Entry point + shared reactives
├── global.R           # Librerias + carga dinamica de R/ y ui/
├── deploy.R           # Script de deploy a shinyapps.io
├── R/                 # Funciones puras (sin dependencia de Shiny reactives)
│   ├── ai_functions.R
│   ├── db_mongo.R
│   ├── exam_bank.R
│   ├── google_cal.R
│   └── study_guides.R
├── server/            # Modulos server (cargados con local=TRUE)
│   ├── server_auth.R
│   ├── server_dashboard.R
│   ├── server_pomodoro.R
│   ├── server_calendario.R
│   ├── server_smart_scheduler.R
│   ├── server_cursos.R
│   ├── server_notas.R
│   ├── server_examen.R
│   ├── server_actividades.R
│   ├── server_semanal.R
│   ├── server_chat.R
│   └── server_analytics.R
├── ui/                # Modulos de interfaz
│   ├── ui_login.R
│   ├── ui_navbar.R
│   ├── ui_dashboard.R
│   └── ... (11 archivos)
└── www/               # Assets estaticos
    ├── custom.css
    ├── pomodoro.js
    └── ...
```

## Variables de entorno requeridas

| Variable | Descripcion |
|----------|------------|
| `MONGODB_URI` | Connection string de MongoDB Atlas |
| `GEMINI_API_KEY` | API key de Google Gemini (empieza con `AIza`) |
| `STUDYPILOT_ADMIN_USERS` | Usuarios hardcoded (formato: `user:pass:Nombre`) |

Copia `.Renviron.example` como `.Renviron` y llena tus valores.

## Deploy a shinyapps.io

```r
source("deploy.R")
```

## Autor

Marvin Calderon — UTEC 2026
