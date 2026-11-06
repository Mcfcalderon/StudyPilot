# Canvas → StudyPilot: cómo leer los cursos sin subir archivos

> Para: Marvin y la sesión de v2 (complementa la §5 de `PLAN_v2.md`). Fecha: 2026-11-06.
> Recomendación corta: **hacerlo en v2, no en la app R** (se va a reemplazar). Usar el token
> personal para leer **mucho más que el sílabo** (evaluaciones, pesos, notas y temas) y el
> **feed de calendario** como plan B sin credenciales.

## 1. Lo que cambió en Canvas (verificar en UTEC antes de construir)

- Desde la versión de **septiembre de 2025**, la institución puede activar
  *"Restrict students and observers from creating personal access tokens"*. Si UTEC lo activó,
  el plan basado en token personal **no funciona** para alumnos.
- Los alumnos que sí pueden crear tokens deben ponerles **vencimiento de 120 días o menos**. La
  app tiene que avisar cuando el token esté por vencer y pedir uno nuevo.
- **Prueba rápida (1 minuto):** en `https://utec.instructure.com` → Cuenta → Configuración →
  "Integraciones aprobadas" → **"+ Nuevo token de acceso"**. Si el botón no aparece o da error,
  el token personal está bloqueado.

### Verificado en UTEC (2026-11-06)
El alumno **puede** crear tokens (el formulario "Nuevo token de acceso" está habilitado), pero
**"El plazo máximo de caducidad es de 30 días"**. Consecuencias para el diseño:
- Con token personal, **el alumno tendría que generar y pegar un token nuevo cada mes**. Sirve
  para la importación inicial y para sincronizar las notas a demanda, no para una sincronización
  continua sin fricción.
- Guardar `expires_at` (`GET /api/v1/users/self` no lo da: pedírselo al alumno o probar el token
  y, ante un `401`, marcar la conexión como "vencida"). Avisar 3 días antes.
- Las fechas y entregas, que cambian seguido, conviene leerlas del **feed ICS** (opción A), que
  no vence.
- Con este límite, **pedir la Developer Key a TI de UTEC (opción C) pasa a ser prioritario**: con
  OAuth2 la renovación es automática con `refresh_token`.

## 2. Opciones, de menos a más pasos para el alumno

| Opción | Pasos del alumno | Qué trae | Requiere a UTEC | Riesgo |
|---|---|---|---|---|
| **A. Feed de calendario (ICS)** | copiar 1 enlace (Calendario → "Fuente del calendario") | todas las tareas y exámenes con fecha, de todos los cursos | no | bajo: solo lectura; quien tenga el enlace ve el calendario |
| **B. Token personal (PAT)** — lo que tiene el plan | crear token y pegarlo (**en UTEC: renovar cada ≤ 30 días**) | todo lo que ve el alumno (ver §3) | que no lo haya bloqueado | **alto:** el token puede **actuar como el alumno** (entregar, enviar mensajes). Cifrado y solo lectura en el servidor |
| **C. OAuth2 (Developer Key)** | botón "Conectar con Canvas" + aceptar | igual que B, con permisos acotados (*scopes*) y renovación automática | **sí:** TI de UTEC debe emitir una Developer Key | bajo: el mejor para producción |
| D. App LTI 1.3 dentro de Canvas | ninguno | contexto del curso | sí, integración institucional | — demasiado para este proyecto |
| ~~E. Leer la web de Canvas (scraping / extensión)~~ | — | — | — | frágil y probablemente contra los términos de uso: **descartado** |

**Recomendación (ajustada al límite de 30 días de UTEC):**
1. Fase 3 de v2 con **B para importar y sincronizar notas a demanda** (cursos, evaluaciones,
   pesos, notas y temas de una vez) **más A para las fechas en el día a día**, porque el feed no
   vence. Si el token venció, la app sigue funcionando con el feed y pide un token nuevo solo
   para "Sincronizar notas". La app R ya tiene un parser ICS (`R/google_cal.R`), y v2 ya planea
   uno para Google.
2. **Pedir ya** a TI de UTEC una **Developer Key** para pasar a **C** (conectar una sola vez). La
   interfaz `CanvasAuthProvider` del plan ya lo prevé, así que no habría que reescribir nada.

## 3. Leer más que el sílabo (menos pasos y datos más confiables)

El plan actual usa Canvas solo para conseguir el **texto del sílabo** y pasarlo por la IA. Con
el mismo token se puede leer directamente lo que hoy se adivina:

| Dato en StudyPilot | Hoy (R) | Con la API de Canvas |
|---|---|---|
| Evaluaciones y fechas | IA lee el PDF; sin semana → viernes o **hoy** | `GET /api/v1/courses/:id/assignment_groups?include[]=assignments&include[]=submission`: nombre, `due_at` exacto, `points_possible` |
| Peso de cada evaluación | IA lee la fórmula del sílabo | `group_weight` de cada grupo, **solo** si el curso tiene `apply_assignment_group_weights = true`; si no, usar el sílabo |
| Notas del alumno | se tipean a mano y se enlazan por `E1…En` (frágil) | el mismo endpoint con `include[]=submission` trae su `score`/`grade`; se enlaza por `canvas_assignment_id` |
| Promedio actual | calculado | `GET /api/v1/users/self/enrollments` → `grades.current_score` (para validar el cálculo propio) |
| Temas del curso | IA o `extract_topics_fallback` sobre el PDF | `GET /api/v1/courses/:id/modules?include[]=items` (semanas/unidades) cuando el profesor usa módulos |
| Sílabo | subir PDF | `include[]=syllabus_body`; si no, `files?search_term=silabo` (ya en el plan) |
| Próximas entregas | actividades | `GET /api/v1/planner/items` o el feed ICS |

**Flujo con un solo paso:** conectar → lista de cursos activos (todos marcados) → "Importar" →
por cada curso, en una sola pasada: curso + evaluaciones con fecha y peso + notas + temas +
sílabo → vista previa → guardar. Después, **"Sincronizar"** (o un cron diario) trae fechas
cambiadas y notas nuevas, comparando por `canvas_assignment_id`. Así nunca hay que volver a subir
archivos. Las columnas `canvas_course_id` y `canvas_assignment_id` ya están en el esquema de v2.

**Cuidados:**
- **La fuente de verdad del peso sigue siendo el sílabo** cuando el profesor no configuró
  grupos ponderados en Canvas (es común). Mostrar ambos si no coinciden.
- Las notas de Canvas pueden estar **ocultas o sin publicar**: respetar `workflow_state` y
  `posted_at`. No mostrar lo que Canvas no muestra.
- Escala: Canvas usa puntos o porcentaje y UTEC usa 0–20. Convertir con `points_possible`.
- Límites de la API: ya cubiertos en la §5 del plan (`X-Rate-Limit-Remaining`, backoff).
- Privacidad: notas y token son datos sensibles. Cifrado y RLS como dice el plan, y botón
  "Desconectar y borrar datos de Canvas".

## 4. ¿Y en la app R actual?

No conviene agregar la conexión por token ahí: es una función grande, guarda un secreto sensible
y la app se va a reemplazar. El único atajo pequeño sería la **opción A**: un campo "URL del
calendario de Canvas" que reutilice `parse_ics_events()` (`R/google_cal.R`) para que las
entregas aparezcan en el calendario. Queda pendiente de tu decisión.

## Fuentes
- Restricción de tokens para alumnos: [Canvas Deploy Notes 2025-09-10](https://community.canvaslms.com/t5/Canvas-Releases/Canvas-Deploy-Notes-2025-09-10/ta-p/654577),
  [2025-11-05](https://community.canvaslms.com/t5/Canvas-Releases/Canvas-Deploy-Notes-2025-11-05/ta-p/661038),
  [Strengthening Security in Canvas: User Access Token Management](https://community.canvaslms.com/t5/The-Product-Blog/Strengthening-Security-in-Canvas-Updates-to-User-Access-Token/ba-p/660299)
- Grupos de tareas, pesos e `include[]=submission`: [Instructure Developer Docs — Assignment Groups](https://developerdocs.instructure.com/services/canvas/resources/assignment_groups),
  [Community: Assignment Group API](https://community.canvaslms.com/t5/Canvas-Question-Forum/Assignment-Group-API-shows-inactive-Assignment-Groups/m-p/549108)
- Feed iCal del alumno: [Canvas Student Guide — Calendar iCal feed](https://community.canvaslms.com/t5/Student-Guide/How-do-I-view-the-Calendar-iCal-feed-to-subscribe-to-an-external/ta-p/331)
