# ---- Instalar y cargar librerías necesarias ----
if (!require("dashboardthemes")) install.packages("dashboardthemes")
if (!require("shinyWidgets"))   install.packages("shinyWidgets")
if (!require("writexl"))        install.packages("writexl")

library(shiny)
library(shinydashboard)
library(dashboardthemes)  # para temas predefinidos
library(shinyWidgets)     # pickerInput, progressBar
library(DT)
library(tidyverse)        # dplyr, ggplot2, stringr…
library(writexl)

# ========================================
# Funciones de utilidad
# ========================================
limpiar_datos <- function(archivo, nombre_profesor) {
  lineas <- readLines(archivo, encoding = "UTF-8")
  df <- data.frame(Nombre=character(), Comentario=character(), stringsAsFactors=FALSE)
  comentario_temporal <- ""
  nombre_temporal    <- NA
  for (i in seq_along(lineas)) {
    if (grepl("^\\d{2}:\\d{2}:\\d{2}\\.\\d{3} -->", lineas[i])) next
    else if (grepl("^<v", lineas[i])) {
      if (!is.na(nombre_temporal) && nzchar(comentario_temporal)) {
        df <- rbind(df,
                    data.frame(Nombre=nombre_temporal,
                               Comentario=comentario_temporal,
                               stringsAsFactors=FALSE))
      }
      nombre_temporal    <- str_extract(lineas[i], "(?<=<v ).*(?=>)")
      comentario_temporal <- ""
    } else {
      comentario_temporal <- paste(comentario_temporal, lineas[i], sep=" ")
    }
  }
  if (!is.na(nombre_temporal) && nzchar(comentario_temporal)) {
    df <- rbind(df,
                data.frame(Nombre=nombre_temporal,
                           Comentario=comentario_temporal,
                           stringsAsFactors=FALSE))
  }
  df %>%
    mutate(
      Nombre     = str_squish(Nombre),
      Comentario = str_remove_all(Comentario, "<.*?>"),
      Comentario = str_squish(Comentario)
    ) %>%
    filter(Nombre != nombre_profesor) %>%
    mutate(id = row_number())
}

calcular_metricas <- function(datos, palabras_clave, columna) {
  datos %>%
    mutate(!!sym(columna) := ifelse(Comentario %in% palabras_clave, 1, 0)) %>%
    group_by(Clase, Nombre) %>%
    summarise(!!sym(columna) := sum(!!sym(columna), na.rm=TRUE),
              .groups="drop")
}

# ========================================
# UI
# ========================================
ui <- dashboardPage(
  dashboardHeader(
    title = tagList(icon("chalkboard"), "S5-Blackboard")
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Resumen Detallado",    tabName="resumen",    icon=icon("table")),
      menuItem("Resumen Acumulativo",  tabName="acumulativo",icon=icon("list")),
      menuItem("Gráfico de Barras",    tabName="grafico",    icon=icon("chart-bar")),
      menuItem("Normas",               tabName="normas",     icon=icon("edit")),
      menuItem("Acerca del autor",     tabName="autor",      icon=icon("user"))
    ),
    hr(),
    fileInput("txt_files", "📂 Subir sesiones (.txt)",
              multiple=TRUE, accept=".txt",
              buttonLabel="Buscar…", placeholder="Ningún archivo seleccionado"),
    textInput("profesor_nombre",
              tagList(icon("user-tie"), "Profesor a excluir:"),
              value="JOSE LUIS VENTURA LEON"),
    hr(),
    sliderInput("peso_participaciones", "⚖️ Peso Participaciones",
                min=0, max=2, value=0.5, step=0.1),
    sliderInput("peso_microfono",      "🎤 Peso Micrófono",
                min=0, max=5, value=2.5, step=0.1),
    sliderInput("peso_webcam",         "📷 Peso Webcam",
                min=0, max=10, value=5, step=0.1)
  ),
  dashboardBody(
    shinyDashboardThemes(theme="onenote"),
    tabItems(
      # ---- Resumen Detallado ----
      tabItem(tabName="resumen",
              fluidRow(
                box(title="Resumen Detallado", status="primary", solidHeader=TRUE, collapsible=TRUE, width=12,
                    DTOutput("resumen_df"), br(),
                    downloadButton("download_resumen", "⬇️ Descargar")
                )
              )
      ),
      # ---- Resumen Acumulativo ----
      tabItem(tabName="acumulativo",
              fluidRow(
                infoBoxOutput("n_sesiones",    width=6),
                infoBoxOutput("n_estudiantes", width=6)
              ),
              fluidRow(
                box(title="Resumen Acumulativo", status="success", solidHeader=TRUE, collapsible=TRUE, width=12,
                    DTOutput("acumulativo_df"), br(),
                    downloadButton("download_acumulativo", "⬇️ Descargar")
                )
              )
      ),
      # ---- Gráfico de Barras ----
      tabItem(tabName="grafico",
              fluidRow(
                box(title="Controles Gráfico", status="warning", solidHeader=TRUE, collapsible=TRUE, width=3,
                    pickerInput("clase_seleccionada", "🔎 Sesión:", choices=NULL, options=list(`live-search`=TRUE), width="100%"),
                    pickerInput("metrica_seleccionada", "📊 Métrica:",
                                choices=c("Chat"="Participaciones","Longitud"="longitud","Micrófono"="Microfono","Webcam"="Webcam"),
                                width="100%"),
                    br(),
                    progressBar(id="prog_plot", value=0, total=100, display_pct=TRUE)
                ),
                box(title="Gráfico de Barras", status="warning", solidHeader=TRUE, collapsible=TRUE, width=9,
                    plotOutput("grafico_barras", height="600px"), br(),
                    downloadButton("download_plot", "⬇️ Descargar")
                )
              )
      ),
      # ---- Normas ----
      tabItem(tabName="normas",
              fluidRow(
                box(title="Normas (editable)", status="info", solidHeader=TRUE, collapsible=TRUE, width=6,
                    DTOutput("tabla_normas")
                ),
                infoBoxOutput("valor_promedio", width=6)
              ),
              fluidRow(
                box(title="Resultados", status="primary", solidHeader=TRUE, collapsible=TRUE, width=12,
                    DTOutput("tabla_calificaciones"), br(),
                    downloadButton("download_norms", "⬇️ Descargar")
                )
              ),
              fluidRow(
                box(title="Distribución de Calificaciones", status="success", solidHeader=TRUE, collapsible=TRUE, width=12,
                    plotOutput("grafico_normas", height="300px")
                )
              )
      ),
      # ---- Acerca del autor ----
      tabItem(tabName="autor",
              fluidRow(
                box(title="Acerca del autor", status="primary", solidHeader=TRUE, width=12,
                    p("José Ventura-León es Doctor en Psicología y Magister en Psicología Educativa. Actualmente es Docente Investigador a tiempo completo en la UPN."),
                    tags$b("Más información en:"), " ",
                    tags$a(href="https://joseventuraleon.com", "joseventuraleon.com", target="_blank"), br(),
                    tags$b("Para consultas o reportar errores, escriba a:"), " info@joseventuraleon.com"
                )
              )
      )
    )
  )
)

# ========================================
# Servidor
# ========================================
server <- function(input, output, session) {
  # Reactivos iniciales
  datos_unidos   <- reactiveVal(data.frame())
  normas_default <- tibble(Calification=1:10, Points_gained=seq(7.5,7.5*10,by=7.5))
  normas_unidas  <- reactiveVal(normas_default)
  
  # Procesar .txt y poblar datos_unidos()
  observeEvent(input$txt_files, {
    req(input$txt_files, input$profesor_nombre)
    archivos_info <- as_tibble(input$txt_files) %>%
      mutate(num=as.numeric(str_extract(name,"\\d+"))) %>%
      arrange(num)
    rutas  <- archivos_info$datapath
    clases <- str_remove(archivos_info$name,"\\.txt$")
    datos <- map2_df(rutas, clases, ~limpiar_datos(.x,input$profesor_nombre)%>%
                       select(-id)%>%mutate(Clase=.y))
    df_base <- datos %>%
      mutate(len=str_count(Comentario,"\\S+")) %>%
      group_by(Clase,Nombre)%>%
      summarise(Participaciones=n(),longitud=round(mean(len),0),.groups="drop")
    uso_mic <- calcular_metricas(datos,
                                 c("MICROFONO","MICROFONO 🙂","MICROFONO (me olvidé de ponerlo cuando pregunté)"),
                                 "Microfono")
    uso_web <- calcular_metricas(datos,
                                 c("WEBCAM","WEBCAM 🙂","WEBCAM (me olvidé de ponerlo cuando pregunté)"),
                                 "Webcam")
    df_int <- df_base %>% full_join(uso_mic,by=c("Clase","Nombre")) %>% full_join(uso_web,by=c("Clase","Nombre"))
    datos_unidos(df_int)
    updatePickerInput(session,"clase_seleccionada",choices=clases,selected=clases[1])
  })
  
  # Reactivos de datos
  datos_originales <- reactive({ req(nrow(datos_unidos())>0); datos_unidos() })
  datos_con_pesos  <- reactive({
    df <- datos_originales()
    df %>% mutate(
      Chat=round(Participaciones*input$peso_participaciones,2),
      Microphone=round(Microfono*input$peso_microfono,2),
      Webcam=round(Webcam*input$peso_webcam,2),
      Points_gained=Chat+Microphone+Webcam
    )
  })
  
  # Resumen Acumulativo
  output$n_sesiones <- renderInfoBox({
    infoBox("Sesiones",length(unique(datos_originales()$Clase)),icon=icon("calendar"),color="purple")
  })
  output$n_estudiantes <- renderInfoBox({
    infoBox("Estudiantes",length(unique(datos_originales()$Nombre)),icon=icon("users"),color="purple")
  })
  acumulativo <- reactive({
    datos_originales() %>%
      group_by(Nombre) %>%
      summarise(
        Total_Chat      = sum(Participaciones,na.rm=TRUE),
        Total_Longitud  = sum(longitud,na.rm=TRUE),
        Total_Microfono = sum(Microfono,na.rm=TRUE),
        Total_Webcam    = sum(Webcam,na.rm=TRUE),
        .groups="drop"
      ) %>% arrange(desc(Total_Chat))
  })
  output$acumulativo_df <- renderDT(datatable(acumulativo(),options=list(pageLength=10,searchHighlight=TRUE)))
  output$download_acumulativo <- downloadHandler(
    filename=function()paste0("Resumen_Acumulativo_",Sys.Date(),".xlsx"),
    content=function(file) write_xlsx(acumulativo(),file)
  )
  
  # Resumen Detallado
  output$resumen_df <- renderDT({
    df <- datos_originales() %>% mutate(
      Chat=round(Participaciones*input$peso_participaciones,2),
      Microphone=round(Microfono*input$peso_microfono,2),
      Webcam=round(Webcam*input$peso_webcam,2),
      Points_gained=Chat+Microphone+Webcam
    ) %>% select(Clase,Nombre,Chat,Microphone,Webcam,Points_gained)
    datatable(df,options=list(pageLength=10,searchHighlight=TRUE))
  })
  output$download_resumen <- downloadHandler(
    filename=function()paste0("Resumen_Detallado_",Sys.Date(),".xlsx"),
    content=function(file) write_xlsx(datos_originales(),file)
  )
  
  # Gráfico de Barras
  output$grafico_barras <- renderPlot({
    req(input$clase_seleccionada,input$metrica_seleccionada)
    for(i in seq(0,100,by=25)){updateProgressBar(session,"prog_plot",value=i);Sys.sleep(0.01)}
    df <- datos_originales()%>%filter(Clase==input$clase_seleccionada)
    ggplot(df,aes(x=reorder(Nombre,!!sym(input$metrica_seleccionada)),y=!!sym(input$metrica_seleccionada)))+
      geom_text(aes(label = !!sym(input$metrica_seleccionada)),
                hjust = -0.1,                                  # tira el texto un poco a la derecha
                size  = 5) +                                   # tamaño de la etiqueta
      geom_col(fill="#713f8e")+coord_flip()+theme_minimal()+
      theme(
        axis.text.x = element_text(size = 14),  # tamaño texto eje X
        axis.text.y = element_text(size = 14),  # tamaño texto eje Y
        axis.title  = element_text(size = 16)   # tamaño títulos de ejes si los tuvieras
      ) +
      labs(x=NULL,y=NULL)
  })
  output$download_plot <- downloadHandler(
    filename=function()paste0("Grafico_Barras_",input$clase_seleccionada,"_",Sys.Date(),".jpg"),
    content=function(file) ggsave(file,plot=last_plot(),device="jpg",width=8,height=6)
  )
  
  # Normas editable
  output$tabla_normas <- renderDT(datatable(normas_unidas(),editable=list(target="cell",columns=1),
                                            options=list(dom="t",paging=FALSE),rownames=FALSE))
  proxy_normas <- dataTableProxy("tabla_normas")
  observeEvent(input$tabla_normas_cell_edit,{
    info <- input$tabla_normas_cell_edit; df <- normas_unidas()
    df[info$row,info$col+1] <- as.numeric(info$value)
    normas_unidas(df); replaceData(proxy_normas,df,resetPaging=FALSE,rownames=FALSE)
  })
  
  # Calificaciones
  tabla_calificada <- reactive({
    datos_con_pesos()%>%replace_na(list(Microfono=0,Webcam=0))%>%
      group_by(Nombre)%>%
      summarise(Chat=sum(Participaciones,na.rm=TRUE),
                Microfono=sum(Microfono,na.rm=TRUE),
                Webcam=sum(Webcam,na.rm=TRUE),
                Points_gained=Chat+Microfono+Webcam,
                .groups="drop")%>%
      rowwise()%>%
      mutate(Calification={vals<-normas_unidas()$Calification[normas_unidas()$Points_gained<=Points_gained];
      if(length(vals)==0)0 else max(vals,na.rm=TRUE)})
  })
  output$tabla_calificaciones <- renderDT(datatable(tabla_calificada(),options=list(pageLength=10,searchHighlight=TRUE)))
  output$download_norms <- downloadHandler(
    filename=function()paste0("Calificaciones_",Sys.Date(),".xlsx"),
    content=function(file) write_xlsx(tabla_calificada(),file)
  )
  output$valor_promedio <- renderInfoBox({
    req(tabla_calificada())
    infoBox("Promedio",round(mean(tabla_calificada()$Calification,na.rm=TRUE),2),
            icon=icon("star"),color="blue")
  })
  output$grafico_normas <- renderPlot({
    ggplot(tabla_calificada(),aes(x=factor(Calification)))+
      geom_bar(fill="#3c8dbc")+labs(x="Calificación",y="Estudiantes")+theme_minimal()
  })
}

# ---- Ejecutar la aplicación ----
shinyApp(ui, server)
