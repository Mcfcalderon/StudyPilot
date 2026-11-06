# Modelo de datos de MongoDB (app R) — referencia para la migración a v2

> Sacado del **código** (rama `main`, 2026-11-06), no de la base. Los ejemplos son ficticios.
> Lo que el código no garantiza (documentos viejos, campos que faltan) se marca como **rareza**.
> Para ver los datos reales: `tools/export_mongo.R --dry-run` y luego la exportación completa.

## Convenciones generales

- **Una sola base**, la que indique `MONGODB_URI` (cargada desde `app_env` en `app.R`).
- **Aislamiento por usuario:** todas las colecciones salvo `users` y `sessions` llevan `user_id`
  = el **nombre de usuario** (`users.user`), no un ObjectId. Todas las consultas pasan por
  `uf(uid, …)` de `R/db_query.R`.
- **No hay claves foráneas ni índices únicos de negocio.** Solo existen (creados por
  `ensure_indexes()` en `R/db_mongo.R`): `users.user` (único), `sessions.token_hash` (único),
  `sessions.expires_at` (TTL) y `user_id_1` en las demás.
- **`course_id` = código del curso** (`courses.id`, p. ej. `"CS2031"`), no un id interno.
  Las actividades personales usan `course_id = "_personal"`.
- **Fechas guardadas como texto** casi siempre. Hay **cuatro formatos** distintos:

  | Formato | Ejemplo | Dónde |
  |---|---|---|
  | `YYYY-MM-DD` | `2026-05-08` | `activities.date`, `settings.semester_start` |
  | `as.character(Sys.time())` | `2026-05-08 21:14:03` o `2026-05-08 21:14:03.481922` | `activities.done_date`, `study_notes.created_at`, `syllabi.uploaded_at`, `pomo_sessions.completed_at` |
  | ISO sin zona | `2026-05-08T19:00:00` / `2026-05-08T19:00` | `ai_blocks.start/end`, `cal_overrides.orig_start`, `usage_events.ts`, `cal_hidden.key` |
  | Fecha BSON (`$date`) | `ISODate("2026-05-08T21:14:03Z")` | solo `sessions.created_at/expires_at` |

  **Rareza — zona horaria:** hasta el commit `fix(tz)` (2026-11-06) la app **no fijaba `TZ`** y los
  sellos de `Sys.time()` quedaban en la zona del servidor: en shinyapps.io, **UTC**; en local
  (Windows), `America/Lima`. Desde que **se despliega** ese commit, `app.R` fija
  `TZ=America/Lima`. El texto no lleva desfase, así que para migrar: sellos **anteriores a la
  fecha de ese despliegue = UTC**, posteriores = Lima (anotar la fecha exacta al desplegar). Desde R 4.3, `as.character(Sys.time())` agrega
  microsegundos, por eso conviven valores con y sin fracción.

- **Números:** mongolite guarda `integer` como int32 y `numeric` como double. El mismo campo
  puede tener ambos tipos según quién lo escribió (p. ej. `act_id`, `note_id`, `checked`).

---

## `users`
Cuentas. Sin `user_id` (la clave es `user`).

| Campo | Tipo | Notas |
|---|---|---|
| `user` | string | `^[a-zA-Z0-9._]{3,40}$` en cuentas nuevas; índice único. Es el `user_id` del resto |
| `password` | string | hash scrypt de libsodium `"$7$…"` (`sodium::password_store`). **Rareza:** puede quedar texto plano en cuentas que no han entrado desde la migración (se re-hashea en el siguiente login, `server_auth.R`) |
| `name` | string | 2–80 caracteres en cuentas nuevas |
| `admin` | bool | siempre `FALSE` al registrar; no se lee (los admins vienen de `STUDYPILOT_ADMIN_USERS`) |

```json
{ "_id": {"$oid":"…"}, "user": "ana.p", "password": "$7$C6..../....", "name": "Ana Pérez", "admin": false }
```
- Escribe: `mg_user_create`, `mg_user_set_password` (`R/db_mongo.R`) ← `server/server_auth.R`.
- Lee: `mg_user_find`, `mg_user_exists` ← `server/server_auth.R`.
- **No hay email.** Los admins de la variable de entorno **no están en esta colección**, aunque
  pueden tener datos en las demás con su `user` como `user_id`.

## `settings`
Un documento por usuario (upsert por `user_id`). Todos los campos son opcionales.

| Campo | Tipo | Notas |
|---|---|---|
| `user_id` | string | |
| `pass_grade` | double | umbral aprobatorio (default 13 si falta) |
| `semester_start` | string `YYYY-MM-DD` | lunes de la semana 1. Si falta, la app usa **el lunes de la semana actual** (`app.R`), no `SEMESTER_START` |
| `tour_seen` | string `"1"` | visita guiada vista (texto, no bool) |

```json
{ "user_id": "ana.p", "pass_grade": 13, "semester_start": "2026-03-23", "tour_seen": "1" }
```
- Escribe: `mg_settings_set` ← `server_notas.R` (`pass_grade`), `server_calendario_crud.R`
  (`semester_start`), `server_tour.R` (`tour_seen`).
- Lee: `mg_settings_get` ← `server_auth.R` (post-login), `server_tour.R`.
- **Rareza:** las horas de sueño del scheduler (`input$sleep_start/end`) **no se guardan**.

## `courses`
| Campo | Tipo | Notas |
|---|---|---|
| `user_id` | string | |
| `id` | string | **código del curso**; lo usan todas las demás colecciones como `course_id` |
| `name` | string | |
| `short` | string | nombre corto; en cursos del sílabo es `substr(name, 1, 20)` |
| `credits` | int | default 3 si la IA no lo da; puede ser `NA`/null |
| `professor` | string | puede ser `""` |
| `formula` | string | texto libre de la fórmula de notas; `""` en cursos manuales |
| `eval_day` | int | 1 = lunes … 7 = domingo; 5 (viernes) para cursos del sílabo |
| `color` | string | hex `#rrggbb`; en cursos del sílabo se elige al azar de 8 colores |

```json
{ "user_id": "ana.p", "id": "CS2031", "name": "Desarrollo Basado en Plataformas", "short": "Desarrollo Basado e",
  "credits": 4, "professor": "J. Ruiz", "formula": "0.3*PC + 0.3*EP + 0.4*EF", "eval_day": 5, "color": "#7c3aed" }
```
- Escribe: `mg_custom_course_add` (borra y vuelve a insertar), `mg_custom_course_delete`
  ← `server_cursos.R` (manual y `guardar_curso_extraido`).
- Lee: `mg_custom_courses_get` ← `server_auth.R`, `server_cursos.R`, `server_calendario.R`.
- **Ids frágiles:**
  - Sin índice único `(user_id, id)`: puede haber duplicados si dos inserciones se cruzan.
  - Si la IA no devuelve código, el id es `"C" + HHMMSS` (`server_cursos.R`). Puede repetirse
    otro día a la misma hora, y entonces **reemplaza** al curso anterior.
  - **Reimportar el sílabo de un curso existente lo borra en cascada** (`mg_custom_course_delete`
    sobre `grades`, `activities`, `study_notes`, `syllabi`, `exam_checks` y `pomo_sessions`) y lo
    recrea: se pierden las notas y los checks de ese curso.
  - El borrado en cascada **no** limpia `ai_blocks`, `schedules`, `cal_overrides` ni `cal_hidden`
    (no tienen `course_id`): pueden quedar bloques huérfanos que nombran un curso que ya no existe.

## `activities`
Evaluaciones y tareas (en v2: `evaluations`).

| Campo | Tipo | Notas |
|---|---|---|
| `user_id` | string | |
| `act_id` | int/double | `max(act_id del usuario) + 1`: **es único por usuario, no globalmente**, y sin índice único (dos inserciones simultáneas pueden repetirlo) |
| `course_id` | string | `courses.id` o `"_personal"` |
| `type` | string | UI: `tarea, ec, examen, proyecto, quiz, repaso, personal`. Desde el sílabo es lo que diga la IA (default `"ec"`), así que puede haber otros valores |
| `name` | string | |
| `code` | string | **siempre `""`** al crear. Los códigos `E1…En` **no se guardan aquí** (ver más abajo) |
| `date` | string `YYYY-MM-DD` | fecha límite. Desde el sílabo es `week_to_date(semana, 5)` (viernes) y, sin semana, **hoy** |
| `week` | int | semana 1–16. **Rareza:** las actividades creadas a mano guardan `0` hasta que se editan (al editar se recalcula con `date_to_week`) |
| `weight` | double | % de la nota (0–100). Las formativas guardan 0 |
| `done` | int 0/1 | |
| `done_date` | string \| null | `as.character(Sys.time())` (zona: ver "Rareza — zona horaria") o null |
| `notes` | string | desde el sílabo: `"Semana N"` |
| `is_calificada` | bool | **opcional:** falta en documentos viejos |
| `temas_vinculados` | array de string | **opcional.** Son **títulos** de tema (texto), no posiciones: si se reextraen los temas, el enlace se rompe sin aviso |

```json
{ "user_id": "ana.p", "act_id": 7, "course_id": "CS2031", "type": "examen", "name": "Examen Parcial",
  "code": "", "date": "2026-05-15", "week": 8, "weight": 30, "done": 0, "done_date": null,
  "notes": "Semana 8", "is_calificada": true, "temas_vinculados": ["Arquitectura REST", "Autenticación JWT"] }
```
- Escribe: `mg_activity_add` ← `server_actividades.R`, `server_cursos.R`;
  `mg_activity_update`, `mg_activity_toggle`, `mg_activity_delete` ← `server_actividades.R`.
- Lee: `mg_activities_all` (orden `{"date":1}`) ← `app.R` (`acts()`, `calc_avg_fast`),
  `server_smart_scheduler.R`; `mg_activity_get_topics` ← `server_actividades.R`.
- **Rarezas:**
  - `is_calificada` ausente: Notas (`calc_avg_fast`, `server_notas.R`) lo trata como
    **calificada**, pero `calcular_prioridades_estudio` lo trata como **formativa** cuando la
    columna existe para otros documentos del mismo usuario (`isTRUE(NA)` = `FALSE`).
  - Una fecha vaciada en el formulario podía guardarse como `[]`/null, y el peso como null o
    fuera de 0–100. Desde el 2026-11-06 `validar_actividad()` lo impide, pero puede haber
    documentos viejos así.

### Cómo se forman los códigos `E1…En` de las notas (crítico para migrar `grades`)
Lo hace `calc_avg_fast` (`app.R`), y **de forma idéntica** `server_notas.R` (panel y guardado):
1. Se toman las actividades del usuario **en el orden que devuelve Mongo con `sort {"date":1}`**.
2. Se filtran las de `course_id == <curso>`.
3. Se quitan las formativas: se queda si `is_calificada` es `NA`/ausente **o** `TRUE`.
4. Se quitan las de `weight` `NA` o `<= 0`.
5. Se ordena por `date` (texto `YYYY-MM-DD`, ascendente). `order()` de R y `arrange()` de dplyr
   son **estables**: con fechas iguales se conserva el orden en que llegaron de Mongo.
6. A la fila `j` (1-based) cuyo `code` esté vacío se le asigna `"E" + j`.

Consecuencias:
- **Las notas cambian de evaluación** cuando cambia la fecha de una evaluación, cuando se borra
  una, cuando pasa a formativa o a peso 0, o cuando se agrega una con fecha anterior.
- **Empates de fecha:** con el mismo `date`, el orden depende de cómo los devuelva Mongo. No
  está garantizado; en la práctica es el orden de inserción (≈ `act_id` / `_id` creciente). Es
  frecuente, porque el sílabo pone todas las evaluaciones de una semana el mismo viernes. Para
  migrar, desempatar por `act_id` ascendente y **marcar para revisión** los cursos con empates
  entre evaluaciones calificadas que tengan nota.
- Si un documento tuviera `code` no vacío (no lo escribe el código actual), se usa ese código y
  la posición `j` sigue contando.

## `grades`
| Campo | Tipo | Notas |
|---|---|---|
| `user_id` | string | |
| `course_id` | string | |
| `code` | string | `"E1"…"En"` según el orden de arriba |
| `grade` | double | 0–20, redondeado a 2 decimales |

```json
{ "user_id": "ana.p", "course_id": "CS2031", "code": "E2", "grade": 15.5 }
```
- Escribe: `mg_grade_set` (upsert por `user_id+course_id+code`), `mg_grade_delete` ← `server_notas.R`.
- Lee: `mg_grades_all` ← `app.R`, `server_smart_scheduler.R`; `mg_grades_course` (sin uso en server).
- **Huérfanas:** si hoy hay menos evaluaciones calificadas que `n` en `En`, esa nota no se
  muestra pero sigue en la base. `calcular_prioridades_estudio` **sí** la usa (promedio simple
  de todas las notas del curso).

## `study_notes`
Temas del curso y material generado por IA.

| Campo | Tipo | Notas |
|---|---|---|
| `user_id` | string | |
| `note_id` | double | `max + 1` por usuario (como `act_id`) |
| `course_id` | string | |
| `text_content` | string | |
| `source` | string | etiqueta que decide el tipo de nota (ver abajo) |
| `created_at` | string | `as.character(Sys.time())` |

Valores de `source` que el código ha escrito (incluye versiones viejas, según `git log`):

| `source` | Qué es | Se reconoce con |
|---|---|---|
| `Temas extraidos del silabo` / `Temas extraídos del sílabo` | lista de temas | `grepl("Temas", source, ignore.case = TRUE)` |
| `Resumen IA`, `<archivo> [RESUMEN IA]`, `<archivo> [organizado]`, `Guía IA — <curso>` | material de estudio | `grepl("IA\|RESUMEN\|organizado", source, ignore.case = TRUE)` |

### Formato "N. Tema" (nota de temas)
Se escribe en `guardar_curso_extraido` (`server_cursos.R`):
```
Temas del curso Desarrollo Basado en Plataformas:
1. Arquitectura REST
2. Autenticación JWT
3. Despliegue en la nube
```
Se lee en cada login (`server_auth.R`, con `topics_from_notes()` / `parse_topics_note()` de
`R/study_topics.R`; los casos borde están en `tests/testthat/test-study-topics.R`): se parte por `\n`, se quedan las líneas que **empiezan**
con `^\d+\.` y se quita `^\d+\.\s*`. La línea de cabecera se descarta.
- Si un curso tiene **varias** notas de temas, se procesan en orden `created_at` descendente y
  cada una sobrescribe a la anterior: **gana la más antigua**.
- Un tema con salto de línea interno se corta. Una línea que empieza con `"2.5 horas…"` se
  toma como tema `"5 horas…"`.
- Para migrar: la posición del tema es su índice 1-based en la lista ya parseada, y es la que
  usa `exam_checks`.

- Escribe: `mg_note_add` ← `server_cursos.R` (temas), `server_examen.R` (`Resumen IA`);
  `mg_note_delete` ← `server_examen.R`.
- Lee: `mg_notes_all` (orden `created_at` desc) ← `server_auth.R`, `server_examen.R`.

## `syllabi`
| Campo | Tipo | Notas |
|---|---|---|
| `user_id`, `course_id` | string | |
| `filename` | string | nombre del archivo subido o `"Silabo_<short>.pdf"` cuando viene del extractor |
| `content` | string | **texto completo** extraído (no el PDF); puede ser grande |
| `uploaded_at` | string | `as.character(Sys.time())` |

- Escribe: `mg_syllabus_add` ← `server_cursos.R` (subida manual y extractor).
  `mg_syllabus_delete` no tiene llamadas.
- Lee: `mg_syllabi_get` ← `server_cursos.R`.
- Puede haber varios documentos con el mismo `filename` (no hay dedupe).

## `schedules`
Horario semanal de clases extraído con IA del PDF de matrícula. Se reemplaza entero en cada extracción.

| Campo | Tipo | Notas |
|---|---|---|
| `user_id` | string | |
| `dia` | string | `Lunes … Sábado`, con o sin tilde (`Miércoles`/`Miercoles`) |
| `hora_inicio`, `hora_fin` | string `HH:MM` | o `"TBD"` si la IA no lo dio (luego se ignora) |
| `curso` | string | **nombre** del curso (texto de la IA), no `course_id` |
| `codigo` | string | código según la IA, o `"TBD"` |
| `aula` | string | o `"TBD"` |
| *otros* | — | **se guardan todas las columnas que devuelva la IA** (p. ej. `tipo`) |

```json
{ "user_id": "ana.p", "dia": "Lunes", "hora_inicio": "08:00", "hora_fin": "10:00",
  "curso": "Desarrollo Basado en Plataformas", "codigo": "CS2031", "aula": "A-906" }
```
- Escribe: `mg_schedule_set` (borra todo e inserta) ← `server_calendario_crud.R`.
- Lee: `mg_schedule_get` ← `server_auth.R`, `server_calendario.R`, `server_chat.R`,
  `server_dashboard.R`, `server_smart_scheduler.R`.

## `ai_blocks`
Bloques del Smart Scheduler y repasos espaciados. Se reemplazan **todos** en cada generación,
edición o arrastre (`mg_ai_blocks_set`: borra e inserta el data.frame entero).

| Campo | Tipo | Notas |
|---|---|---|
| `user_id` | string | |
| `summary` | string | título. IA: libre (p. ej. `"🤖 Data Analytics: Clustering"`, `"☕ Descanso"`); repaso: `"Repaso <short>: <nombre>"` |
| `start`, `end` | string | `YYYY-MM-DDTHH:MM` o `…:SS` (depende de lo que devolvió la IA y de las ediciones) |
| `location` | string | `""` |
| `color` | string | **nombre** (`purple`, `orange`, `cyan`, `gray`, `teal`) o hex si se editó |
| `is_ai` | bool | `TRUE` |
| `source` | string | `"ai"` |
| `className` | string | `"evento-ia"` |
| `isReadOnly` | bool | `FALSE` |

- Escribe: `mg_ai_blocks_set` ← `app.R` (`save_ai_blocks_mongo`), `server_smart_scheduler.R`.
- Lee: `mg_ai_blocks_get` ← `server_auth.R`.
- **No guarda `course_id`, actividad ni tema:** analytics adivina el curso con `grepl` del
  código, nombre o nombre corto dentro de `summary` (`server_analytics.R`). La prioridad solo se
  deduce del color. Los bloques "Descanso" se reconocen por el título.

## `exam_checks`
Checklist de temas en "Modo examen".

| Campo | Tipo | Notas |
|---|---|---|
| `user_id`, `course_id` | string | |
| `topic_key` | string | `paste0(<activities.code>, "_", i)`, con `i` = posición del tema (1-based) |
| `checked` | double 0/1 | |

- Escribe: `mg_exam_check_set` (upsert) ← `server_examen.R`. Lee: `mg_exam_checks_get` ← `server_examen.R`.
- **Rareza importante:** como `activities.code` siempre es `""`, en la práctica la clave es
  `"_1"`, `"_2"`, … **Los checks se comparten entre todos los exámenes del mismo curso**: no
  apuntan a una evaluación concreta. Para migrar, copiar cada check a **todas** las evaluaciones
  examen/quiz del curso, o solo al próximo examen pendiente (decisión de producto).
- Por el mismo motivo, hasta el 2026-11-06 la detección de final (`grepl("final|EF|EXM2", ex$code)`)
  nunca se activaba: todos los exámenes mostraban la **primera mitad** de los temas. Desde
  entonces `exam_is_final()` (`R/validation.R`) también mira el nombre (`\bfinal\b`, `\bEF\b`,
  `\bEXM2\b`). Las claves con `i` mayor que `ceiling(n/2)` solo aparecen después de esa fecha
  (o en datos viejos con código).

## `pomo_sessions`
| Campo | Tipo | Notas |
|---|---|---|
| `user_id` | string | |
| `course_id` | string | **opcional**: falta si el usuario no tenía cursos (el select queda `NULL`) |
| `duration_min` | int | 1–120 según la UI. Sin validar en el servidor hasta el 2026-11-06 (puede haber `NA`); desde entonces, 1–180 |
| `completed_at` | string | `as.character(Sys.time())` (zona: ver "Rareza — zona horaria") |

- Escribe: `mg_pomo_add` ← `server_pomodoro.R`. Lee: `mg_pomo_get_today` ← `server_pomodoro.R`.
- "Hoy" se calcula con `grepl(Sys.Date(), completed_at)`. Mientras la app corria en UTC, entre
  las 19:00 y las 24:00 de Lima las sesiones contaban para el día siguiente (corregido con TZ).

## `cal_overrides`
Ediciones locales de eventos del horario o de Google Calendar. Se reemplaza la lista entera.

| Campo | Tipo | Notas |
|---|---|---|
| `user_id` | string | |
| `orig_title` | string | título original: **es la clave** |
| `orig_start` | string | `YYYY-MM-DDTHH:MM[:SS]`, o `"any_THH:MM"` cuando viene de arrastrar |
| `new_title` | string | |
| `new_start_time`, `new_end_time` | string `HH:MM` | texto libre del modal hasta el 2026-11-06 (puede haber valores inválidos); desde entonces se valida `HH:MM` y fin > inicio |
| `new_color` | string | hex o `""` |

- Escribe: `mg_cal_overrides_set` ← `server_calendario_crud.R`. Lee: `mg_cal_overrides_get` ← `server_auth.R`.
- **Clave frágil `título + hora`:** dos eventos con el mismo nombre y hora se pisan. Los
  arrastres se agregan **sin quitar duplicados**, así que la lista crece.

## `cal_hidden`
| Campo | Tipo | Notas |
|---|---|---|
| `user_id` | string | |
| `key` | string | `"<título>\|YYYY-MM-DDTHH:MM"` (puede repetirse) |

- Escribe: `mg_cal_hidden_set` (reemplaza todo) ← `server_calendario_crud.R`. Lee: `mg_cal_hidden_get` ← `server_auth.R`.
- Como `\|` separa la clave, un título que contenga `\|` se parte mal: separar por el **último** `\|`.

## `sessions`
Auto-login. **No se migra.**

| Campo | Tipo | Notas |
|---|---|---|
| `token_hash` | string | SHA-256 hex del token de 64 hex (índice único) |
| `user` | string | `users.user` |
| `created_at`, `expires_at` | fecha BSON | 30 días; índice TTL sobre `expires_at` |

- `mg_session_create/user/delete` ← `server_auth.R`.

## `usage_events`
| Campo | Tipo | Notas |
|---|---|---|
| `user_id` | string | |
| `event` | string | `login`, `chat`, `grades_save`, `smart_schedule` |
| `detail` | string | `""` o, en `grades_save`, el número de notas guardadas como texto |
| `ts` | string | `%Y-%m-%dT%H:%M:%S` en la zona del servidor (ver "Rareza — zona horaria") |

- Escribe: `mg_log_event` ← `server_auth.R`, `server_chat.R`, `server_notas.R`,
  `server_smart_scheduler.R`. Lee: `mg_usage_all` ← `server_analytics.R`.

---

## Datos que NO están en Mongo
Viven solo en la sesión y se pierden al recargar: banco de preguntas de examen
(`exam_questions`), historial del chat, resultados de exámenes de práctica y eventos de Google
Calendar (se vuelven a bajar del feed ICS público, cuyo correo tampoco se guarda).

## Colecciones fuera de esta lista
Versiones antiguas usaban `connection_test` (prueba de conexión). `tools/export_mongo.R` lista
todas las colecciones de la base y avisa de cualquiera que no esté documentada aquí.
