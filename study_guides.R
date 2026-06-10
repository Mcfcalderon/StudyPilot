# ============ GUÍAS DE ESTUDIO POR TEMA ============

# ============ ÉTICA — TEMAS POR PC ============
etica_pc1_topics <- c(
  "¿Qué es la ética? Ética vs moral (Rachels)",
  "Utilitarismo (Mill, Bentham) — mayor felicidad para el mayor número",
  "Deontología (Kant) — imperativo categórico, deber moral",
  "Liberalismo vs Comunitarismo (Sandel) — libertad individual vs bien común"
)

etica_pc2_topics <- c(
  "Principios de Biomédica — Autonomía, Beneficencia, No maleficencia, Justicia (Beauchamp & Childress)",
  "Género y desigualdad social — Interseccionalidad (Crenshaw)",
  "Sostenibilidad y medio ambiente — Ecología profunda (Capra)",
  "Tecnología y Sociedad — Neutralidad de la ciencia y la tecnología (Olivé)",
  "Infocracia y control de la información (Byung-Chul Han)",
  "Ética de las Profesiones — Principios profesionales (Hortal)",
  "Ética profesional aplicada — Códigos deontológicos"
)

# ============ ÉTICA — LECTURAS POR SEMANA ============
etica_readings <- tibble::tibble(
  week = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L),
  reading = c(
    "Rachels — ¿Qué es la moralidad?",
    "Sandel — Utilitarismo (El mayor bien para el mayor número)",
    "Sandel — Libertarismo / Kant (Deontología)",
    "Sandel — Liberalismo vs Comunitarismo",
    "Beauchamp & Childress — Principios de Biomédica / Millán — Beneficencia",
    "Crenshaw — Interseccionalidad, género y desigualdad",
    "Capra — La trama de la vida / Sostenibilidad",
    "Evaluación continua (EC)",
    "Olivé — ¿Es éticamente neutral la ciencia y la tecnología?",
    "Han — Infocracia / Control de la información",
    "Hortal — Ética de las profesiones (Repaso / PC2)",
    "Hortal — Ética profesional (continuación)",
    "Hortal — Ética profesional (aplicación)",
    "Entrega de ensayo final",
    "Defensa oral de ensayo",
    "Defensa oral de ensayo (cierre)"
  ),
  tema = c(
    "Ética vs moral", "Utilitarismo", "Deontología / Libertarismo",
    "Liberalismo vs Comunitarismo", "Bioética", "Interseccionalidad",
    "Sostenibilidad", "EC", "Ciencia y tecnología", "Infocracia",
    "Ética profesional", "Ética profesional", "Ética profesional",
    "Ensayo final", "Defensa oral", "Defensa oral"
  )
)

etica_pc2_guides <- list(

list(
  title = "Principios de Biomédica",
  reading = "Millán - Beneficencia y no maleficencia / Mayorga - El mal en la bioética",
  week = 5,
  summary = "Beauchamp y Childress establecen 4 principios fundamentales de la bioética que guían la toma de decisiones en contextos de salud y, por extensión, en cualquier actividad que impacte a las personas.",
  concepts = list(
    list(term = "Autonomía", def = "Derecho de las personas a tomar decisiones informadas sobre su propia vida. Requiere: información completa, comprensión y ausencia de coerción."),
    list(term = "Beneficencia", def = "Obligación de hacer el bien, promover el bienestar del otro. No basta con no hacer daño; hay que actuar positivamente."),
    list(term = "No maleficencia", def = "'Primum non nocere' — lo primero es no causar daño. Principio más básico y restrictivo que la beneficencia."),
    list(term = "Justicia", def = "Distribución equitativa de recursos, beneficios y cargas. ¿Quién recibe qué?"),
    list(term = "Consentimiento informado", def = "Herramienta que operacionaliza la autonomía: el sujeto debe comprender y aceptar libremente."),
    list(term = "Conflicto entre principios", def = "Los 4 principios pueden entrar en conflicto. Ej: beneficencia (operar) vs autonomía (paciente rechaza).")
  ),
  diagram = list(
    list(main_title = "🏥 BIOÉTICA — 4 Principios", title = "🧠 AUTONOMÍA", color = "#2563eb", icon = "🧠",
      items = list(
        list(label = "Decisión del individuo", icon = "👤", children = c("Consentimiento informado", "Información + Comprensión + Libertad"))
      )
    ),
    list(title = "💚 BENEFICENCIA", color = "#16a34a", icon = "💚",
      items = list(
        list(label = "Hacer el bien", icon = "✅", children = c("Obligación POSITIVA", "Promover bienestar activamente"))
      )
    ),
    list(title = "🚫 NO MALEFICENCIA", color = "#dc2626", icon = "🚫",
      items = list(
        list(label = "No causar daño", icon = "⛔", children = c("Obligación NEGATIVA", "Más básica que beneficencia", "Primum non nocere"))
      )
    ),
    list(title = "⚖️ JUSTICIA", color = "#7c3aed", icon = "⚖️",
      items = list(
        list(label = "Equidad", icon = "📊", children = c("Distribución de recursos", "Criterios: necesidad, mérito, igualdad"))
      )
    )
  ),
  key_questions = c(
    "¿Cuáles son los 4 principios de Beauchamp y Childress?",
    "¿Cuál es la diferencia entre beneficencia y no maleficencia?",
    "¿Qué es el consentimiento informado y qué requiere?",
    "¿Qué pasa cuando dos principios entran en conflicto?"
  )
),

list(
  title = "Género y desigualdad social",
  reading = "Crenshaw (1991) - Mapping the Margins: Intersectionality",
  week = 6,
  summary = "Crenshaw introduce el concepto de interseccionalidad: las formas de opresión (género, raza, clase) no son independientes sino que se cruzan, creando experiencias únicas de discriminación.",
  concepts = list(
    list(term = "Interseccionalidad", def = "Las identidades sociales se superponen creando formas únicas de discriminación que no se entienden sumando cada eje."),
    list(term = "Discriminación interseccional", def = "Una mujer negra sufre una forma específica de discriminación que ni mujeres blancas ni hombres negros experimentan."),
    list(term = "Tres niveles", def = "Estructural (leyes), Político (movimientos), Representacional (medios).")
  ),
  diagram = list(
    list(main_title = "🔀 INTERSECCIONALIDAD — Crenshaw", title = "❌ No es SUMA", color = "#dc2626", icon = "❌",
      items = list("Género + Raza ≠ Experiencia real", "Cada eje por separado NO captura la realidad")
    ),
    list(title = "✅ Es CRUCE", color = "#16a34a", icon = "✅",
      items = list("Género × Raza = Experiencia ÚNICA", "Opresiones son SIMULTÁNEAS e INTERDEPENDIENTES")
    ),
    list(title = "📊 Tres Niveles", color = "#7c3aed", icon = "📊",
      items = list(
        list(label = "Estructural", icon = "🏛", children = c("Leyes e instituciones")),
        list(label = "Político", icon = "✊", children = c("Movimientos, advocacy")),
        list(label = "Representacional", icon = "🎭", children = c("Medios, cultura"))
      )
    ),
    list(title = "💡 Ejemplo", color = "#ea580c", icon = "💡",
      items = list("Mujer indígena trabajadora:", "Sexismo + racismo + clasismo", "De forma SIMULTÁNEA e INTERDEPENDIENTE")
    )
  ),
  key_questions = c(
    "¿Qué es la interseccionalidad según Crenshaw?",
    "¿Por qué no basta con 'sumar' opresiones?",
    "¿Cuáles son los 3 niveles de interseccionalidad?",
    "Da un ejemplo concreto de discriminación interseccional"
  )
),

list(
  title = "Sostenibilidad y medio ambiente",
  reading = "Capra (1996) - La trama de la vida. P.25-70",
  week = 7,
  summary = "Capra critica la visión mecanicista cartesiana y propone una visión sistémica: la vida es una red de relaciones interdependientes.",
  concepts = list(
    list(term = "Visión mecanicista", def = "Descartes/Newton: la naturaleza es una máquina. Reduccionismo."),
    list(term = "Visión sistémica", def = "Las propiedades del todo no existen en las partes. Las relaciones importan más."),
    list(term = "Ecología profunda", def = "Naess: valor intrínseco de todos los seres vivos."),
    list(term = "Autopoiesis", def = "Maturana y Varela: los sistemas vivos se auto-producen y auto-organizan.")
  ),
  diagram = list(
    list(main_title = "🌿 CAPRA — Dos Visiones del Mundo", title = "🔬 MECANICISTA (Descartes)", color = "#dc2626", icon = "🔬",
      items = list("Partes → Todo", "Reducción y análisis", "Lineal y predecible", "Naturaleza = máquina")
    ),
    list(title = "🌍 SISTÉMICA (Capra)", color = "#16a34a", icon = "🌍",
      items = list("Todo → Partes", "Relaciones e interconexiones", "Red / No lineal", "Naturaleza = red viva")
    ),
    list(title = "🌱 Ecología Profunda", color = "#0891b2", icon = "🌱",
      items = list(
        list(label = "Superficial", icon = "📉", children = c("Conservar para humanos")),
        list(label = "Profunda", icon = "📈", children = c("Valor intrínseco de TODA vida"))
      )
    ),
    list(title = "🔄 Autopoiesis", color = "#7c3aed", icon = "🔄",
      items = list("La vida se auto-produce", "Maturana y Varela", "Sistemas vivos se auto-organizan")
    )
  ),
  key_questions = c(
    "¿Qué critica Capra de la visión mecanicista?",
    "¿Qué es la visión sistémica de la vida?",
    "¿Diferencia entre ecología superficial y profunda?",
    "¿Qué es la autopoiesis?"
  )
),

list(
  title = "Ética y Tecnología Contemporánea",
  reading = "Transición de ética clásica a ética aplicada a la tecnología",
  week = 8,
  summary = "La tecnología no es neutra: incorpora valores en su diseño y plantea dilemas éticos nuevos.",
  concepts = list(
    list(term = "Determinismo tecnológico", def = "La tecnología determina el cambio social. Critica: ignora decisiones humanas."),
    list(term = "Instrumentalismo", def = "La tecnología es neutra. Critica: el diseño incorpora valores."),
    list(term = "Constructivismo social", def = "La tecnología es moldeada por factores sociales y políticos."),
    list(term = "Principio de precaución", def = "Ante incertidumbre, actuar con cautela antes de daños irreversibles.")
  ),
  diagram = list(
    list(main_title = "❓ ¿La tecnología es NEUTRA?", title = "✅ SÍ — Instrumentalismo", color = "#16a34a", icon = "✅",
      items = list("Solo es una herramienta", "⚠️ PROBLEMA: el diseño YA incorpora valores")
    ),
    list(title = "❌ NO — Constructivismo", color = "#dc2626", icon = "❌",
      items = list("Refleja intereses y valores", "Decisiones de diseño = decisiones éticas")
    ),
    list(title = "👥 ¿Quién es RESPONSABLE?", color = "#7c3aed", icon = "👥",
      items = list(
        list(label = "Diseñador", icon = "🔧", children = c("¿Anticipó consecuencias?")),
        list(label = "Empresa", icon = "🏢", children = c("¿Priorizó ganancias vs seguridad?")),
        list(label = "Usuario", icon = "👤", children = c("¿Uso responsable?")),
        list(label = "Estado", icon = "🏛", children = c("¿Regulación adecuada?"))
      )
    )
  ),
  key_questions = c(
    "¿Es la tecnología éticamente neutra?",
    "¿Diferencia entre determinismo e instrumentalismo?",
    "¿Qué es el principio de precaución?",
    "¿Quién es responsable de los efectos de una tecnología?"
  )
),

list(
  title = "Tecnología y Sociedad (Olivé)",
  reading = "Olivé (2000) - ¿Son éticamente neutrales la ciencia y la tecnología?",
  week = 9,
  summary = "Olivé: ni la ciencia ni la tecnología son neutrales. En cada nivel hay decisiones de valor.",
  concepts = list(
    list(term = "Tesis de neutralidad", def = "La ciencia produce conocimiento 'puro'. Olivé la rechaza."),
    list(term = "Sistemas tecnocientíficos", def = "Ciencia y tecnología incluyen agentes, instituciones, valores y normas."),
    list(term = "Participación pública", def = "Las decisiones sobre tecnología no deben ser solo de expertos.")
  ),
  diagram = list(
    list(main_title = "🔬 OLIVÉ — Ciencia NO es neutral", title = "Nivel 1: ¿Qué se investiga?", color = "#0891b2", icon = "🔍",
      items = list("Intereses económicos, militares, políticos")
    ),
    list(title = "Nivel 2: ¿Cómo se investiga?", color = "#2563eb", icon = "📋",
      items = list("Métodos, sujetos, ética de investigación")
    ),
    list(title = "Nivel 3: ¿Cómo se aplica?", color = "#7c3aed", icon = "🎯",
      items = list("¿Para quién? ¿Con qué consecuencias?")
    ),
    list(title = "Nivel 4: ¿Quién decide?", color = "#ea580c", icon = "🏛",
      items = list("Expertos vs Participación democrática")
    ),
    list(title = "💡 CONCLUSIÓN", color = "#dc2626", icon = "💡",
      items = list("Cada nivel implica DECISIONES DE VALOR", "La neutralidad es un MITO")
    )
  ),
  key_questions = c(
    "¿Por qué Olivé dice que la ciencia no es neutra?",
    "¿En qué niveles entran los valores?",
    "¿Qué son los sistemas tecnocientíficos?",
    "¿Por qué importa la participación pública?"
  )
),

list(
  title = "Byung-Chul Han - Infocracia",
  reading = "Byung-Chul Han - Infocracia (Selección)",
  week = 10,
  summary = "Han: el régimen de información digital crea una nueva forma de dominación. La sobreinformación erosiona la democracia.",
  concepts = list(
    list(term = "Infocracia", def = "Régimen de dominación basado en información, datos y algoritmos."),
    list(term = "Crisis de la verdad", def = "La sobreinformación diluye la verdad. Burbujas informativas."),
    list(term = "Dataísmo", def = "Ideología que cree que los datos son la respuesta a todo. Reduce la complejidad humana."),
    list(term = "Psicopolítica", def = "El poder controla MENTES (no cuerpos) a través de seducción digital.")
  ),
  diagram = list(
    list(main_title = "📱 INFOCRACIA — Byung-Chul Han", title = "🏛 ANTES: Democracia", color = "#16a34a", icon = "🏛",
      items = list("Debate racional", "Hechos compartidos", "Ciudadanos informados")
    ),
    list(title = "📊 AHORA: Infocracia", color = "#dc2626", icon = "📊",
      items = list("Sobreinformación → desinformación", "Burbujas informativas", "Emoción > Razón", "Algoritmos > Debate")
    ),
    list(title = "🔒 Mecanismos de Control", color = "#7c3aed", icon = "🔒",
      items = list(
        list(label = "Datos", icon = "📈", children = c("Vigilancia")),
        list(label = "Transparencia", icon = "👁", children = c("Control total")),
        list(label = "Algoritmos", icon = "🤖", children = c("Manipulación")),
        list(label = "Likes", icon = "👍", children = c("Economía de atención"))
      )
    ),
    list(title = "⚠️ RESULTADO", color = "#dc2626", icon = "⚠️",
      items = list("Erosión de la democracia y la libertad")
    )
  ),
  key_questions = c(
    "¿Qué es la infocracia según Han?",
    "¿Cómo la sobreinformación afecta a la democracia?",
    "¿Qué es el dataísmo y por qué lo critica Han?",
    "¿Diferencia entre biopolítica y psicopolítica?"
  )
),

list(
  title = "Ética de las Profesiones (Hortal)",
  reading = "Hortal (2002) - Ética de las profesiones. Partes 1-3",
  week = "12-14",
  summary = "Hortal aplica los principios bioéticos al ejercicio profesional. El profesional tiene responsabilidad moral ante la sociedad.",
  concepts = list(
    list(term = "Profesión vs oficio", def = "Profesión: formación especializada, servicio a la sociedad, autonomía regulada, código ético."),
    list(term = "Beneficencia profesional", def = "Competencia técnica al servicio del cliente y la sociedad."),
    list(term = "No maleficencia profesional", def = "No dañar: formación continua, no actuar fuera de tu competencia."),
    list(term = "Conflicto de interés", def = "Cuando el interés personal compite con el deber. Priorizar el deber."),
    list(term = "Convicción vs responsabilidad", def = "Weber: principios absolutos vs considerar consecuencias. Se necesitan ambas."),
    list(term = "Código deontológico", def = "Normas de conducta profesional. Necesario pero NO suficiente.")
  ),
  diagram = list(
    list(main_title = "⚖️ ÉTICA PROFESIONAL — Hortal", title = "📋 ¿Qué es una PROFESIÓN?", color = "#ea580c", icon = "📋",
      items = list("📚 Formación especializada", "🤝 Servicio a la sociedad", "🔑 Autonomía regulada", "📜 Código ético")
    ),
    list(title = "4 PRINCIPIOS adaptados", color = "#2563eb", icon = "🎯",
      items = list(
        list(label = "Beneficencia", icon = "💚", children = c("Hacer bien el trabajo")),
        list(label = "No maleficencia", icon = "🚫", children = c("No dañar con tu ejercicio")),
        list(label = "Autonomía", icon = "🧠", children = c("Respetar decisiones del cliente")),
        list(label = "Justicia", icon = "⚖️", children = c("Servir al bien común"))
      )
    ),
    list(title = "⚡ Tensiones", color = "#dc2626", icon = "⚡",
      items = list("Interés personal vs deber profesional", "Convicción vs responsabilidad", "Lealtad al empleador vs bien público")
    ),
    list(title = "📜 Código Deontológico", color = "#ca8a04", icon = "📜",
      items = list("Necesario pero NO suficiente", "La ética va MÁS ALLÁ de cumplir reglas")
    )
  ),
  key_questions = c(
    "¿Qué distingue una profesión de un oficio?",
    "¿Cómo aplica Hortal los 4 principios a las profesiones?",
    "¿Qué es un conflicto de interés profesional?",
    "¿Diferencia entre ética de convicción y de responsabilidad?",
    "¿Por qué un código deontológico no es suficiente?"
  )
)
)
