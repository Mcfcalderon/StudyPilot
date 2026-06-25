# ============ BANCO DE PREGUNTAS POR CURSO ============
# Preguntas se generan con IA basadas en los temas del curso
# El banco hardcodeado está vacío para que cada usuario genere sus propias preguntas
exam_questions <- list()

# Legacy bank (kept as reference, not loaded)
.exam_questions_legacy <- list(
HH5101_legacy = list(
  # TEMA 1: Ética vs moral
  list(topic = "¿Qué es la ética?", type = "mc",
    q = "¿Cuál es la diferencia fundamental entre ética y moral?",
    opts = c("La ética es individual y la moral es colectiva",
             "La moral son las normas vividas; la ética es la reflexión filosófica sobre esas normas",
             "No hay diferencia, son sinónimos",
             "La ética es religiosa y la moral es secular"),
    ans = 2, expl = "La moral se refiere a las costumbres y normas de conducta de un grupo. La ética es la disciplina filosófica que reflexiona críticamente sobre la moral."),

  list(topic = "¿Qué es la ética?", type = "mc",
    q = "Según James Rachels, ¿qué caracteriza al 'agente moral'?",
    opts = c("Alguien que sigue las leyes sin cuestionarlas",
             "Alguien capaz de dar razones para justificar sus acciones",
             "Alguien que actúa por instinto",
             "Alguien que obedece a la autoridad"),
    ans = 2, expl = "Rachels enfatiza que la moralidad requiere la capacidad de razonar y justificar las acciones, no solo seguir reglas."),

  list(topic = "¿Qué es la ética?", type = "open",
    q = "Explica la diferencia entre ética descriptiva, ética normativa y metaética. Da un ejemplo de cada una.",
    ans_guide = "Descriptiva: describe las prácticas morales (ej: 'en Japón se valora la puntualidad'). Normativa: prescribe qué se debe hacer (ej: 'no se debe mentir'). Metaética: analiza el lenguaje y fundamentos morales (ej: '¿qué significa bueno?')."),

  # TEMA 2: Utilitarismo, Deontología, Virtud
  list(topic = "Utilitarismo, Deontología, Ética de la virtud", type = "mc",
    q = "Según el utilitarismo, una acción es moralmente correcta si:",
    opts = c("Cumple con el deber moral universal",
             "Maximiza la felicidad o bienestar para el mayor número de personas",
             "Es consistente con las virtudes del agente",
             "Respeta los derechos individuales sin excepción"),
    ans = 2, expl = "El utilitarismo (Bentham, Mill) juzga las acciones por sus consecuencias: la mayor felicidad para el mayor número."),

  list(topic = "Utilitarismo, Deontología, Ética de la virtud", type = "mc",
    q = "¿Qué dice el imperativo categórico de Kant?",
    opts = c("Actúa de modo que maximices tu felicidad personal",
             "Actúa solo según la máxima que puedas querer que se convierta en ley universal",
             "Actúa según las consecuencias previsibles de tus actos",
             "Actúa como lo haría una persona virtuosa"),
    ans = 2, expl = "Kant propone que una acción es moral si su principio puede universalizarse sin contradicción."),

  list(topic = "Utilitarismo, Deontología, Ética de la virtud", type = "mc",
    q = "La ética de la virtud (Aristóteles) se enfoca principalmente en:",
    opts = c("Las consecuencias de las acciones",
             "Las reglas morales universales",
             "El carácter y las disposiciones del agente moral",
             "Los derechos fundamentales"),
    ans = 3, expl = "Aristóteles centra la ética en el desarrollo del carácter virtuoso (prudencia, justicia, templanza, fortaleza)."),

  list(topic = "Utilitarismo, Deontología, Ética de la virtud", type = "mc",
    q = "Michael Sandel en 'Justicia' presenta el caso del tranvía para ilustrar:",
    opts = c("Que las leyes siempre son justas",
             "El conflicto entre utilitarismo y deontología",
             "Que la virtud es más importante que las reglas",
             "Que la democracia resuelve todos los dilemas morales"),
    ans = 2, expl = "El dilema del tranvía muestra la tensión entre maximizar vidas (utilitarismo) y no usar personas como medios (deontología kantiana)."),

  list(topic = "Utilitarismo, Deontología, Ética de la virtud", type = "open",
    q = "Compara el enfoque utilitarista y el deontológico frente al siguiente caso: una empresa descubre que su producto tiene un defecto menor que causa daño en 1 de cada 100,000 usuarios. Retirar el producto cuesta $10 millones. ¿Qué diría cada enfoque?",
    ans_guide = "Utilitarismo: haría cálculo costo-beneficio (costo del retiro vs daño esperado). Deontología: el deber de no dañar es absoluto, debe retirar el producto independientemente del costo."),

  # TEMA 3: Liberalismo y comunitarismo
  list(topic = "Liberalismo y comunitarismo", type = "mc",
    q = "¿Cuál es la posición central del liberalismo político?",
    opts = c("La comunidad define los valores del individuo",
             "La libertad individual y los derechos son prioritarios sobre el bien común",
             "El Estado debe imponer una visión de la vida buena",
             "La tradición cultural determina la moralidad"),
    ans = 2, expl = "El liberalismo (Rawls, Locke) prioriza la autonomía individual y los derechos fundamentales."),

  list(topic = "Liberalismo y comunitarismo", type = "mc",
    q = "El comunitarismo critica al liberalismo porque:",
    opts = c("Da demasiado poder al Estado",
             "Ignora que las personas se forman dentro de comunidades y tradiciones",
             "Es demasiado religioso",
             "No permite el libre mercado"),
    ans = 2, expl = "Los comunitaristas (MacIntyre, Taylor) argumentan que la identidad moral se forma en comunidades, no en individuos aislados."),

  # TEMA 4: Ética del investigador
  list(topic = "Ética del investigador", type = "mc",
    q = "¿Cuál de los siguientes NO es un principio de la integridad científica?",
    opts = c("Honestidad en la recolección de datos",
             "Transparencia en la metodología",
             "Maximización de resultados positivos para publicar",
             "Respeto al consentimiento informado"),
    ans = 3, expl = "Maximizar resultados positivos (sesgo de publicación) es justamente una violación de la integridad científica."),

  list(topic = "Ética del investigador", type = "mc",
    q = "El plagio académico incluye:",
    opts = c("Solo copiar textos completos sin citar",
             "Copiar textos, parafrasear sin citar, y auto-plagio",
             "Solo copiar de internet",
             "Solo copiar de compañeros"),
    ans = 2, expl = "El plagio incluye copia textual, parafraseo sin atribución, y reutilización de trabajo propio sin declararlo (auto-plagio)."),

  # TEMA 5: Biomédica
  list(topic = "Principios de Biomédica", type = "mc",
    q = "Los cuatro principios de la bioética de Beauchamp y Childress son:",
    opts = c("Libertad, igualdad, fraternidad, justicia",
             "Autonomía, beneficencia, no maleficencia, justicia",
             "Honestidad, lealtad, respeto, solidaridad",
             "Verdad, bondad, belleza, utilidad"),
    ans = 2, expl = "Beauchamp y Childress establecen autonomía, beneficencia, no maleficencia y justicia como los pilares de la ética biomédica."),

  list(topic = "Principios de Biomédica", type = "mc",
    q = "El principio de 'no maleficencia' establece que:",
    opts = c("Se debe hacer el mayor bien posible",
             "Se debe respetar la decisión del paciente",
             "No se debe causar daño innecesario",
             "Se deben distribuir los recursos equitativamente"),
    ans = 3, expl = "No maleficencia = 'primum non nocere' (lo primero es no hacer daño)."),

  # TEMA 6: Género
  list(topic = "Género y desigualdad social", type = "mc",
    q = "El concepto de 'interseccionalidad' de Crenshaw se refiere a:",
    opts = c("La intersección de calles en zonas urbanas marginales",
             "Cómo múltiples formas de discriminación (raza, género, clase) se superponen y amplifican",
             "La colaboración entre disciplinas académicas",
             "La integración de mercados internacionales"),
    ans = 2, expl = "Crenshaw muestra que las experiencias de opresión no se suman linealmente sino que se intersectan creando formas únicas de discriminación."),

  # TEMA 7: Sostenibilidad
  list(topic = "Sostenibilidad y medio ambiente", type = "mc",
    q = "Fritjof Capra en 'La trama de la vida' propone:",
    opts = c("Que la naturaleza es un recurso para explotar eficientemente",
             "Una visión sistémica donde todos los seres vivos están interconectados",
             "Que la tecnología resolverá todos los problemas ambientales",
             "Que el crecimiento económico es compatible con la sostenibilidad"),
    ans = 2, expl = "Capra propone una visión ecológica sistémica: la vida es una red de relaciones interdependientes."),

  # TEMA 8-9: Tecnología y sociedad
  list(topic = "Tecnología y Sociedad", type = "mc",
    q = "Según Olivé, la pregunta sobre si la tecnología es éticamente neutra:",
    opts = c("Sí, la tecnología es solo una herramienta",
             "No, la tecnología incorpora valores y decisiones éticas en su diseño",
             "Depende del país donde se use",
             "Solo es relevante para tecnologías militares"),
    ans = 2, expl = "Olivé argumenta que la tecnología no es neutra: su diseño, desarrollo y uso incorporan valores, intereses y decisiones éticas."),

  list(topic = "Tecnología y Sociedad", type = "mc",
    q = "Byung-Chul Han en 'Infocracia' argumenta que:",
    opts = c("La información digital libera a las personas",
             "El régimen de información digital amenaza la libertad y la democracia",
             "Las redes sociales son éticamente neutrales",
             "La vigilancia digital solo afecta a los criminales"),
    ans = 2, expl = "Han analiza cómo el régimen de información digital crea nuevas formas de control y erosiona la democracia."),

  # TEMA 10: Ética de las profesiones
  list(topic = "Ética de las Profesiones", type = "mc",
    q = "Según Hortal, la ética profesional se fundamenta en:",
    opts = c("Solo en las leyes del país",
             "En principios éticos aplicados al ejercicio de una profesión específica",
             "En la opinión de los clientes",
             "Solo en el código de conducta empresarial"),
    ans = 2, expl = "Hortal establece que la ética profesional aplica principios éticos generales al contexto específico de cada profesión."),

  list(topic = "Ética de las Profesiones", type = "mc",
    q = "Un conflicto de interés en la práctica profesional ocurre cuando:",
    opts = c("El profesional tiene dos trabajos",
             "Los intereses personales del profesional compiten con su deber hacia el cliente o la sociedad",
             "El cliente no paga a tiempo",
             "El profesional no tiene suficiente experiencia"),
    ans = 2, expl = "El conflicto de interés surge cuando el juicio profesional puede verse comprometido por intereses personales o financieros."),

  list(topic = "Ética de las Profesiones", type = "open",
    q = "Como futuro ingeniero industrial, describe un dilema ético que podrías enfrentar en tu práctica profesional. Analízalo desde el utilitarismo y la deontología.",
    ans_guide = "Ejemplo: optimizar costos reduciendo personal vs responsabilidad con los trabajadores. Utilitarismo: evaluar bienestar total. Deontología: deber de no tratar personas como medios."),

  # === PREGUNTAS ADICIONALES PC2 ===
  list(topic = "Principios de Biomédica", type = "mc",
    q = "El principio de 'autonomía' en bioética establece que:",
    opts = c("El médico siempre sabe qué es mejor para el paciente",
             "Las personas tienen derecho a tomar decisiones informadas sobre su propia vida y salud",
             "La sociedad decide sobre el tratamiento del individuo",
             "Solo los profesionales pueden decidir en asuntos de salud"),
    ans = 2, expl = "La autonomía reconoce la capacidad de las personas para tomar decisiones informadas y libres sobre su salud."),

  list(topic = "Principios de Biomédica", type = "open",
    q = "Explica la diferencia entre beneficencia y no maleficencia. ¿Puede haber conflicto entre ambos principios? Da un ejemplo.",
    ans_guide = "Beneficencia: obligación de hacer el bien. No maleficencia: obligación de no causar daño. Conflicto: un tratamiento doloroso (daño) que cura (beneficio). Ej: quimioterapia."),

  list(topic = "Género y desigualdad social", type = "open",
    q = "Explica qué es la interseccionalidad según Crenshaw y por qué es relevante para entender la discriminación. Da un ejemplo concreto.",
    ans_guide = "La interseccionalidad muestra que las opresiones (género, raza, clase) no son independientes sino que se cruzan. Ej: una mujer indígena enfrenta discriminación de género Y racial simultáneamente, creando una experiencia única."),

  list(topic = "Sostenibilidad y medio ambiente", type = "mc",
    q = "La visión sistémica de Capra propone que la vida se organiza como:",
    opts = c("Una cadena lineal de causa-efecto",
             "Una red de relaciones interdependientes",
             "Una jerarquía piramidal",
             "Un sistema mecánico predecible"),
    ans = 2, expl = "Capra argumenta que los sistemas vivos no son máquinas sino redes complejas donde todo está interconectado."),

  list(topic = "Sostenibilidad y medio ambiente", type = "open",
    q = "¿Por qué Capra critica la visión mecanicista de la naturaleza? ¿Qué alternativa propone?",
    ans_guide = "Capra critica el reduccionismo cartesiano que ve la naturaleza como máquina. Propone una ecología profunda: la naturaleza es una red viva donde los componentes son inseparables del todo. La sostenibilidad requiere pensar sistémicamente."),

  list(topic = "Tecnología y Sociedad", type = "open",
    q = "Según Olivé, ¿por qué la ciencia y la tecnología NO son éticamente neutrales? Explica con un ejemplo.",
    ans_guide = "Olivé argumenta que la tecnología incorpora valores desde su diseño: qué problemas se eligen resolver, para quién, con qué efectos. Ej: un algoritmo de crédito puede incorporar sesgos raciales en su diseño, no es 'neutral'."),

  list(topic = "Tecnología y Sociedad", type = "mc",
    q = "El concepto de 'infocracia' de Byung-Chul Han se refiere a:",
    opts = c("Un gobierno que promueve la alfabetización digital",
             "Un régimen de dominación basado en el control de la información y los datos",
             "La democracia participativa a través de internet",
             "Una utopía donde la información es libre para todos"),
    ans = 2, expl = "Han acuña 'infocracia' para describir cómo el régimen informacional actual controla a las personas a través de datos y algoritmos, erosionando la libertad."),

  list(topic = "Tecnología y Sociedad", type = "mc",
    q = "Según Han, ¿qué efecto tiene la sobreinformación digital en la democracia?",
    opts = c("La fortalece porque los ciudadanos están más informados",
             "No tiene ningún efecto relevante",
             "La debilita porque fragmenta el discurso público y dificulta el consenso racional",
             "La mejora porque facilita las votaciones en línea"),
    ans = 3, expl = "Han argumenta que la sobreinformación crea desinformación, polarización y una crisis del discurso racional necesario para la democracia."),

  list(topic = "Ética de las Profesiones", type = "mc",
    q = "Según Hortal, los principios fundamentales de la ética profesional son:",
    opts = c("Productividad, eficiencia y rentabilidad",
             "Beneficencia, no maleficencia, autonomía y justicia aplicados a la profesión",
             "Obediencia al jefe, cumplir horario, y ser puntual",
             "Libertad de mercado, competencia y maximización de ganancias"),
    ans = 2, expl = "Hortal adapta los principios bioéticos de Beauchamp y Childress al ámbito profesional general."),

  list(topic = "Ética de las Profesiones", type = "mc",
    q = "La 'responsabilidad profesional' implica que el ingeniero:",
    opts = c("Solo debe cumplir con lo que le pide su empleador",
             "Debe responder ante la sociedad por las consecuencias de su trabajo, más allá de su empleador",
             "Solo es responsable si firma un contrato legal",
             "No tiene responsabilidad si sigue instrucciones"),
    ans = 2, expl = "La responsabilidad profesional va más allá del contrato laboral: el profesional responde ante la sociedad por el impacto de su trabajo."),

  list(topic = "Ética de las Profesiones", type = "open",
    q = "Hortal habla de la tensión entre la 'ética de la convicción' y la 'ética de la responsabilidad' en el ejercicio profesional. Explica esta tensión con un ejemplo de ingeniería.",
    ans_guide = "Ética de convicción: actuar según principios absolutos (ej: nunca comprometer seguridad). Ética de responsabilidad: considerar las consecuencias prácticas (ej: el costo de un estándar de seguridad excesivo puede hacer inviable un proyecto que beneficiaría a muchos). Tensión: ¿hasta dónde flexibilizar principios por resultados?")
)
)

# ============ FUNCIÓN: ORGANIZAR TEXTO POR TEMAS ============
organize_text_by_topics <- function(text, course_id) {
  topics <- course_topics[[course_id]]
  if (is.null(topics)) return(list())

  # Keywords per topic (lowercase)
  keywords <- list(
    HH5101 = list(
      c("ética", "moral", "diferencia", "rachels", "singer", "qué es"),
      c("utilitarismo", "deontología", "virtud", "kant", "mill", "bentham", "aristóteles", "sandel", "justicia", "imperativo", "categórico", "consecuencias"),
      c("liberalismo", "comunitarismo", "rawls", "macintyre", "taylor", "individuo", "comunidad", "derechos"),
      c("investigador", "integridad", "científica", "plagio", "datos", "consentimiento", "informado"),
      c("biomédica", "beauchamp", "childress", "autonomía", "beneficencia", "maleficencia", "principios"),
      c("género", "interseccionalidad", "crenshaw", "mujer", "discriminación", "desigualdad"),
      c("sostenibilidad", "medio ambiente", "capra", "trama", "ecología", "sistémica"),
      c("tecnología", "sociedad", "olivé", "neutralidad", "ciencia"),
      c("algoritmo", "sesgo", "inteligencia artificial", "vigilancia", "han", "infocracia"),
      c("profesión", "profesional", "hortal", "deontología profesional", "código"),
      c("deber", "norma", "código profesional", "responsabilidad")
    )
  )

  course_kw <- keywords[[course_id]]
  if (is.null(course_kw)) {
    # Generic: use topic words themselves as keywords
    course_kw <- lapply(topics, function(t) {
      words <- tolower(strsplit(t, "[ ,.:;]+")[[1]])
      words[nchar(words) > 3]
    })
  }

  # Split text into paragraphs
  paragraphs <- strsplit(text, "\n{2,}")[[1]]
  paragraphs <- trimws(paragraphs)
  paragraphs <- paragraphs[nchar(paragraphs) > 20]

  # Score each paragraph against each topic
  results <- lapply(seq_along(topics), function(i) {
    kw <- tolower(course_kw[[min(i, length(course_kw))]])
    matched <- sapply(paragraphs, function(p) {
      p_lower <- tolower(p)
      sum(sapply(kw, function(k) grepl(k, p_lower, fixed = TRUE)))
    })
    list(
      topic = topics[i],
      paragraphs = paragraphs[matched > 0],
      scores = matched[matched > 0]
    )
  })

  # Also collect unmatched paragraphs
  all_matched <- unique(unlist(lapply(results, function(r) r$paragraphs)))
  unmatched <- setdiff(paragraphs, all_matched)

  list(by_topic = results, unmatched = unmatched)
}

# ============ FUNCIÓN: GENERAR EXAMEN DE PRÁCTICA ============
generate_practice_exam <- function(course_id, n_questions = 10, type = "all") {
  message("[StudyPilot] Generating exam for: ", course_id, " (", n_questions, " questions, type=", type, ")")
  questions <- exam_questions[[course_id]]

  # If no hardcoded questions, generate with AI using course topics
  if (is.null(questions) || length(questions) == 0) {
    topics <- get0("course_topics", envir = globalenv())
    course_topics_vec <- if (!is.null(topics)) topics[[course_id]] else NULL
    message("[StudyPilot] Topics for ", course_id, ": ", length(course_topics_vec %||% character(0)))
    if (is.null(course_topics_vec) || length(course_topics_vec) == 0) {
      message("[StudyPilot] No topics found for ", course_id)
      return(NULL)
    }

    c_all <- get0("courses", envir = globalenv())
    cname <- if (!is.null(c_all) && course_id %in% c_all$id) c_all$name[c_all$id == course_id] else course_id
    topics_text <- paste(course_topics_vec, collapse = "\n- ")
    ai_n <- n_questions

    questions <- tryCatch({
      message("[StudyPilot] Calling AI for ", ai_n, " questions...")
      result <- ai_generate_questions(topics_text, cname, ai_n)
      message("[StudyPilot] AI returned ", length(result), " questions")
      result
    }, error = function(e) {
      message("[StudyPilot] AI quiz error: ", e$message)
      NULL
    })
    if (is.null(questions) || length(questions) == 0) return(NULL)
  }

  if (type == "mc") questions <- Filter(function(q) q$type == "mc", questions)
  if (type == "open") questions <- Filter(function(q) q$type == "open", questions)

  n <- min(n_questions, length(questions))
  sample(questions, n)
}

# ============ FUNCIÓN: CALIFICAR EXAMEN ============
grade_exam <- function(answers, exam) {
  correct <- 0
  total_mc <- 0
  results <- list()

  for (i in seq_along(exam)) {
    q <- exam[[i]]
    user_ans <- answers[[i]]

    if (q$type == "mc") {
      total_mc <- total_mc + 1
      is_correct <- !is.null(user_ans) && as.integer(user_ans) == q$ans
      if (is_correct) correct <- correct + 1
      results[[i]] <- list(
        question = q$q,
        correct = is_correct,
        user_answer = if(!is.null(user_ans)) q$opts[as.integer(user_ans)] else "Sin respuesta",
        right_answer = q$opts[q$ans],
        explanation = q$expl
      )
    } else {
      results[[i]] <- list(
        question = q$q,
        correct = NA,
        user_answer = user_ans %||% "",
        right_answer = q$ans_guide,
        explanation = "Pregunta abierta — revisa tu respuesta contra la guía."
      )
    }
  }

  list(
    score = correct,
    total = total_mc,
    pct = if(total_mc > 0) round(correct / total_mc * 100) else NA,
    results = results
  )
}
