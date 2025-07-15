
# =================================================================
# APLICACIÓN SHINY: CHATBOT DE COMERCIO EXTERIOR CON IA Y GRÁFICOS
# Versión 4.8 (ValueBox con totales por pestaña)
# =================================================================

# 1. CARGA DE PAQUETES
# -----------------------------------------------------------------
library(shiny)
library(shinythemes)
library(dplyr)
library(readxl)
library(stringr)
library(DT)
library(openai)
library(jsonlite)
library(plotly)
library(tidyr)

# 2. CARGA Y LIMPIEZA DE DATOS
# -----------------------------------------------------------------
tryCatch({
  df_bmx <- read_xlsx("df_comercioT.xlsx")
  message("Datos de 'df_comercioT.xlsx' cargados exitosamente.")
}, error = function(e) {
  message("Archivo 'df_comercioT.xlsx' no encontrado. Usando datos de ejemplo.")
  df_bmx <<- data.frame(
    pais = rep(c("ESTADOS UNIDOS", "GUATEMALA", "HONDURAS", "MEXICO", "Total"), each = 32),
    codigo_arancelario = rep(c("0713330000", "0901110000", "0407210000"), length.out = 160),
    descripcion = rep(c("- - Frijoles", "- - Café sin tostar", "- - Huevos frescos"), length.out = 160),
    medida = rep(c("valor", "Kilogramos"), each = 80),
    anio = rep(2022:2023, 80),
    cantidad = runif(160, 5000, 1000000),
    Elemento = rep(c("Importacion", "Exportacion"), each = 40, times = 2)
  )
})

paises_a_excluir <- c("Total", "MUNDO")
df_bmx <- df_bmx %>%
  filter(!pais %in% paises_a_excluir)

# --- LISTAS DE REFERENCIA Y VALORES GLOBALES ---
productos_disponibles <- df_bmx %>%
  distinct(descripcion, codigo_arancelario) %>%
  arrange(descripcion)
paises_disponibles <- sort(unique(df_bmx$pais))
max_anio_global <- max(df_bmx$anio, na.rm = TRUE)

# --- FUNCIÓN DE IA ---
interpretar_pregunta_con_ia <- function(pregunta_usuario) {
  if (Sys.getenv("OPENAI_API_KEY") == "") { stop("La variable de entorno OPENAI_API_KEY no está configurada.") }
  prompt_sistema <- "Eres un asistente experto en análisis de datos de comercio exterior. Tu tarea es interpretar la pregunta del usuario y extraer parámetros en formato JSON. No incluyas explicaciones.
  Parámetros a extraer:
  - producto: El producto o código arancelario.
  - paises: Lista de países socios.
  - anio: El año específico. Si el usuario pide 'evolución' o 'tendencia', el valor de 'anio' DEBE SER null.
  - elemento: 'Importacion' o 'Exportacion'.
  - intencion: 'comparativa' para rankings/top; 'especifica' para todo lo demás.
  - agregacion: Debe ser 'total_anual' si el usuario pide un 'total general' o 'consolidado'. Por defecto, debe ser 'por_pais'."
  
  tryCatch({
    respuesta_ia <- create_chat_completion(
      model = "gpt-4.1-nano", #"gpt-4o",
      messages = list(list("role" = "system", "content" = prompt_sistema), list("role" = "user", "content" = pregunta_usuario)),
      temperature = 0.7, max_tokens = 1200 #300 a 0 temp
    )
    json_extraido <- str_extract(respuesta_ia$choices$message.content, "(?s)\\{.*\\}")
    if (is.na(json_extraido)) { message("La IA no devolvió un bloque JSON."); return(NULL) }
    fromJSON(json_extraido, flatten = TRUE)
  }, error = function(e) { message("Error procesando la respuesta de OpenAI: ", e$message); return(NULL) })
}

# 3. INTERFAZ DE USUARIO (UI)
# -----------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  # --- MEJORA: CSS para las cajas de valor ---
  tags$head(tags$style(HTML("
    .catalog-box { max-height: 250px; overflow-y: auto; border: 1px solid #ccc; padding: 5px; border-radius: 4px; }
    .value-box { 
      background-color: #f8f9fa; 
      border: 1px solid #dee2e6; 
      border-left: 5px solid #007bff; 
      padding: 15px; 
      margin-bottom: 20px;
      text-align: center;
      border-radius: 5px;
    }
    .value-box-value { font-size: 28px; font-weight: bold; margin: 0; color: #004085; }
    .value-box-label { font-size: 14px; color: #6c757d; margin: 0; text-transform: uppercase; }
  "))),
  
  h1("| Dashboard_IA::Comercio Exterior de El Salvador |", align = "center", style = "margin-bottom: 0;"),
  h4("Consulta Dinámica de Importaciones y Exportaciones", align = "center"), hr(),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("pregunta", "Haz tu pregunta en lenguaje natural (emplea el código de (1) producto del catalogo para mejores resultados):",
                    placeholder = "Evolución de la importación de 0403202000", rows = 3),
      actionButton("enviar", "Consultar con IA", class = "btn-primary", icon = icon("robot")),
      actionButton("limpiar", "Limpiar", class = "btn-secondary", icon = icon("broom")), hr(),
      tags$b("Ideas para preguntar:"),
      tags$ul(
        
        tags$li("Para frijoles: Evolución de importación de 0708200000"),
        tags$li("Para papaya: Evolución de la importación de 0807200000"),
        tags$li("Para aguacate:Importación de 0804400000 en el año 2021"),
        tags$li("Para café:Exportación de 0901113000"),
        tags$li("Para Maíz blanco: Evolución de la importación de 1005903000 por país"),
        #tags$li("Importación total de café"),
        #tags$li("Top países a los que exportamos en 2022")
      ), hr(),
      tags$b("Referencias de la Base de Datos:"),
      textInput("filtro_producto", "Buscar en catálogo de productos:", placeholder = "Ej: café, frijol..."),
      uiOutput("catalogo_productos_ui"),
      tags$p(style="font-size:11px; color: #666; margin-top:10px;", paste("Mostrando", length(paises_disponibles), "países únicos.")),
      tags$details(
        tags$summary("Ver Países disponibles", style = "cursor: pointer;"),
        tags$div(class = "catalog-box", tags$ul(lapply(paises_disponibles, tags$li)))
      ), hr(),
      div(style = "text-align: center; padding-top: 20px;",
          tags$img(src = "https://i.postimg.cc/G36BghFf/Logo.png", height = "80px"), hr(),
          h5("Fuente: Elaborado por DICA con información de BCR (hasta mayo 2025) / Impulsada por IA.", style = "margin-top: 5px; color: #666;")
      ), width = 3
    ),
    mainPanel(uiOutput("resultados_ui"))
  )
)

# 4. LÓGICA DEL SERVIDOR (SERVER)
# -----------------------------------------------------------------
server <- function(input, output, session) {
  
  datos_valor <- reactiveVal(NULL)
  datos_kilos <- reactiveVal(NULL)
  
  observeEvent(input$enviar, {
    req(input$pregunta)
    showModal(modalDialog("Procesando...", footer=NULL, easyClose = FALSE))
    parametros <- interpretar_pregunta_con_ia(input$pregunta)
    removeModal()
    
    if (is.null(parametros)) {
      showModal(modalDialog(title = "Error", "No se pudo procesar la solicitud.", footer = modalButton("Cerrar")))
      return()
    }
    
    datos_filtrados <- df_bmx
    if (!is.null(parametros$elemento)) { datos_filtrados <- datos_filtrados %>% filter(Elemento == parametros$elemento) }
    if (!is.null(parametros$producto)) {
      if (str_detect(parametros$producto, "^[0-9]{8,10}$")) {
        datos_filtrados <- datos_filtrados %>% filter(codigo_arancelario == parametros$producto)
      } else {
        datos_filtrados <- datos_filtrados %>% filter(str_detect(tolower(descripcion), tolower(parametros$producto)))
      }
    }
    if (!is.null(parametros$anio)) { datos_filtrados <- datos_filtrados %>% filter(anio == as.integer(parametros$anio)) }
    if (length(parametros$paises) > 0) {
      paises_matches <- unique(unlist(sapply(parametros$paises, function(p) grep(p, unique(df_bmx$pais), ignore.case = TRUE, value = TRUE))))
      if(length(paises_matches) > 0) { datos_filtrados <- datos_filtrados %>% filter(pais %in% paises_matches) }
    }
    
    resultado_agregado <- summarise(datos_filtrados,
                                    Total_Comerciado = sum(cantidad, na.rm = TRUE),
                                    .by = c(anio, pais, Elemento, medida, descripcion, codigo_arancelario))
    
    if (!is.null(parametros$agregacion) && parametros$agregacion == "total_anual") {
      resultado_final <- summarise(resultado_agregado,
                                   Total_Comerciado = sum(Total_Comerciado, na.rm = TRUE),
                                   .by = c(anio, Elemento, medida)) %>%
        mutate(pais = "Total General")
    } else {
      resultado_final <- resultado_agregado
    }
    
    if (is.null(parametros$intencion) || parametros$intencion != "comparativa") {
      resultado_final <- resultado_final %>% arrange(desc(Total_Comerciado))
    } else {
      resultado_final <- resultado_final %>% arrange(desc(Total_Comerciado)) %>% head(10)
    }
    
    resultado_final_filtrado <- resultado_final %>%
      filter(!is.na(Total_Comerciado) & Total_Comerciado > 0)
    
    res_valor <- resultado_final_filtrado %>% filter(grepl("valor|val|usd", medida, ignore.case = TRUE))
    res_kilos <- resultado_final_filtrado %>% filter(grepl("kilo|kg", medida, ignore.case = TRUE))
    
    datos_valor(if(nrow(res_valor) > 0) res_valor else NULL)
    datos_kilos(if(nrow(res_kilos) > 0) res_kilos else NULL)
  })
  
  observeEvent(input$limpiar, {
    updateTextAreaInput(session, "pregunta", value = "")
    updateTextInput(session, "filtro_producto", value = "")
    datos_valor(NULL)
    datos_kilos(NULL)
  })
  
  output$catalogo_productos_ui <- renderUI({
    productos_filtrados <- productos_disponibles
    termino_busqueda <- input$filtro_producto
    if (!is.null(termino_busqueda) && nchar(termino_busqueda) > 0) {
      termino_busqueda_lower <- tolower(termino_busqueda)
      productos_filtrados <- productos_disponibles %>%
        filter(
          str_detect(tolower(descripcion), termino_busqueda_lower) |
            str_detect(codigo_arancelario, termino_busqueda_lower)
        )
    }
    tagList(
      tags$p(style="font-size:11px; color: #666;", paste("Mostrando", nrow(productos_filtrados), "de", format(nrow(productos_disponibles), big.mark=","), "productos.")),
      tags$details(
        open = if (!is.null(termino_busqueda) && nchar(termino_busqueda) > 0) TRUE else NULL,
        tags$summary("Ver Productos/Códigos disponibles", style = "cursor: pointer;"),
        tags$div(class = "catalog-box",
                 if (nrow(productos_filtrados) > 0) {
                   tags$ul(lapply(1:nrow(productos_filtrados), function(i) {
                     tags$li(paste(productos_filtrados$descripcion[i], "-", productos_filtrados$codigo_arancelario[i]))
                   }))
                 } else {
                   tags$p("No se encontraron productos.")
                 }
        )
      )
    )
  })
  
  generar_grafico_plotly <- function(df_plot, unidad_medida) {
    req(df_plot)
    titulo_elemento <- first(df_plot$Elemento)
    df_plot <- df_plot %>% arrange(anio)
    if (length(unique(df_plot$anio)) > 1) {
      es_total <- (first(df_plot$pais) == "Total General")
      titulo_grafico <- if(es_total) paste("Evolución Total de", titulo_elemento) else paste("Evolución de", titulo_elemento, "por País")
      p <- plot_ly(df_plot, x = ~anio, y = ~Total_Comerciado, color = ~pais, type = 'bar',
                   text = ~paste(if(!es_total) paste("País:", pais, "<br>") else "", "Año:", anio, "<br>Total:", format(round(Total_Comerciado), big.mark=",", scientific = FALSE)),
                   hoverinfo = 'text')
      p %>% layout(title = list(text = titulo_grafico, x = 0.5),
                   xaxis = list(title = "Año", tickformat = "d"),
                   yaxis = list(title = paste("Total en", unidad_medida), rangemode = "tozero"),
                   hovermode = "x unified", barmode = 'stack', showlegend = !es_total,
                   legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.2))
    } else {
      titulo_producto_especifico <- if (n_distinct(df_plot$descripcion) == 1) first(df_plot$descripcion) else "producto seleccionado"
      plot_ly(df_plot, x = ~pais, y = ~Total_Comerciado, color = ~pais, type = 'bar',
              text = ~paste("Total:", format(round(Total_Comerciado), big.mark=",", scientific = FALSE)), hoverinfo = 'text') %>%
        layout(title = list(text = paste(titulo_elemento, "de", str_trunc(titulo_producto_especifico, 30), "en", first(df_plot$anio)), x = 0.5),
               xaxis = list(title = "País Socio", categoryorder = "total descending"),
               yaxis = list(title = paste("Total en", unidad_medida), rangemode = "tozero"),
               showlegend = FALSE)
    }
  }
  
  # Tablas
  output$tabla_valor <- renderDT({
    df_val <- datos_valor(); req(df_val)
    datatable(df_val, caption = "Resultados en Valor (USD).", filter = 'top',
              options = list(pageLength = 5, scrollX = TRUE, language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json')), 
              rownames = FALSE) %>%
      formatCurrency('Total_Comerciado', currency = 'US$ ', interval = 3, mark = ',', digits = 0)
  })
  output$tabla_kilos <- renderDT({
    df_kg <- datos_kilos(); req(df_kg)
    df_kg_fmt <- df_kg %>%
      mutate(Total_Formateado = paste0(format(Total_Comerciado, big.mark = ",", scientific = FALSE), " Kg")) %>%
      select(-Total_Comerciado) %>%
      rename(Total_Comerciado = Total_Formateado)
    datatable(df_kg_fmt, caption = "Resultados en Kilogramos.", filter = 'top',
              options = list(pageLength = 5, scrollX = TRUE, language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json')), 
              rownames = FALSE)
  })
  
  # Gráficos
  output$grafico_valor <- renderPlotly({ generar_grafico_plotly(datos_valor(), "USD") })
  output$grafico_kilos <- renderPlotly({ generar_grafico_plotly(datos_kilos(), "Kg") })
  
  # --- MEJORA: Renderizado de las Cajas de Valor ---
  output$valor_total_box <- renderUI({
    df <- datos_valor(); req(df)
    total <- sum(df$Total_Comerciado, na.rm = TRUE)
    tags$div(class = "value-box",
             tags$p(class = "value-box-value", paste0("US$ ", format(round(total), big.mark = ",", scientific = FALSE))),
             tags$p(class = "value-box-label", "Valor Total de la Consulta")
    )
  })
  output$kilos_total_box <- renderUI({
    df <- datos_kilos(); req(df)
    total <- sum(df$Total_Comerciado, na.rm = TRUE)
    tags$div(class = "value-box",
             tags$p(class = "value-box-value", paste0(format(round(total), big.mark = ",", scientific = FALSE), " Kg")),
             tags$p(class = "value-box-label", "Volumen Total de la Consulta")
    )
  })
  
  # UI de Resultados con Pestañas
  output$resultados_ui <- renderUI({
    if (is.null(datos_valor()) && is.null(datos_kilos())) {
      if (input$enviar == 0) return(div(class="text-center", style="padding-top: 50px;", h4("A la espera de tu consulta...")))
      return(div(class="text-center", style="padding-top: 50px;", icon("info-circle", "fa-3x"), h4("No se encontraron resultados."), p("Intenta reformular tu pregunta.")))
    }
    tabs <- list()
    if (!is.null(datos_valor())) {
      tabs <- append(tabs, list(tabPanel("Resultados en Valor (USD)", 
                                         # --- MEJORA: Se añade el uiOutput para la caja de valor ---
                                         uiOutput("valor_total_box"),
                                         DTOutput("tabla_valor"), hr(),
                                         plotlyOutput("grafico_valor")
      )))
    }
    if (!is.null(datos_kilos())) {
      tabs <- append(tabs, list(tabPanel("Resultados en Volumen (KG)",
                                         # --- MEJORA: Se añade el uiOutput para la caja de valor ---
                                         uiOutput("kilos_total_box"),
                                         DTOutput("tabla_kilos"), hr(),
                                         plotlyOutput("grafico_kilos")
      )))
    }
    do.call(tabsetPanel, c(id = "pestas_resultados", tabs))
  })
}

# 5. LANZAMIENTO DE LA APLICACIÓN
# -----------------------------------------------------------------
shinyApp(ui, server)