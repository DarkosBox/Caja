shinyUI(fluidPage(
    title = "Análisis de Medicamentos",
    tags$head(includeCSS(file.path("www", "app.css"))),
    tags$b(
        href="https://www.colsubsidioenlinea.com/",
        tags$img(style="position: absolute; top: 0; right:0; border: 0;",
                 src="droguerias.png")
    ),
    div(id = "header",
        div(id = "title",
            "Análisis Transacciones Medicamentos "
        ),
        div(id = "subsubtitle",
            "Inteligencia de Negocios",
            HTML("&bull;"),
            "Subdirección Comercial")
    ),
    
    sidebarLayout(
        sidebarPanel(width = 3,
                     dateRangeInput("Fecha", h4("Periodo de Análisis"),start=min(BD$Fecha),end=max(BD$Fecha),
                                    format ="dd/mm/yyyy",separator = "a", language = "es", weekstart = 1,
                                    min=min(BD$Fecha), max=max(BD$Fecha)),
                     selectizeInput("Categoria", h4("Categoría"),choices = c(unique(BD$Categoria), "TOTAL"), selected="TOTAL"),
                     selectizeInput("Patologia", h4("Patología"),choices = c(unique(BD$Patologia), "TOTAL"), selected="TOTAL"),
                     selectizeInput("Principio", h4("Principio Activo"),choices = c(unique(as.character(BD$principioconcatenado)), "TOTAL"), selected="TOTAL"),
                     selectizeInput("Producto", h4("Producto"),choices = c(unique(as.character(BD$prod_nombre)), "TOTAL"), selected="TOTAL"),
                     selectizeInput("AgrudoCro", h4("Agudo / Crónico"),choices = c(unique(BD$tipodetratamiento), "TOTAL"), selected="TOTAL"),
                     selectizeInput("Proveedor", h4("Proveedor"),choices = c(unique(BD$prod_proveedor_nombre), "TOTAL"), selected="TOTAL"),
                     selectizeInput("Contactable", h4("Contactable"),choices = c(unique(as.character(BD$autorizacion)), "TOTAL"), selected="TOTAL"),
                     br(),
                     actionButton("go", "Aplicar Filtros", icon = icon("refresh"))
        ),
        mainPanel(navbarPage(h5("Menu"),
                             tabPanel(h5("Resumen RFM"),
                                      h4("Distribución de Pacientes por Scores RFM"),
                                      plotOutput("ResumenRFM",height = "600px"),
                                      br(),
                                      h4("Valores Scores RFM"),
                                      fluidRow(
                                          column(4,h4("Frecuencia"),
                                                 tableOutput('TablaFrecuencia')
                                          ),
                                          column(4,h4("Recencia"),
                                                 tableOutput('TablaRecencia')
                                          ),
                                          column(4,h4("Monto"),
                                                 tableOutput('TablaMonto')
                                          ))),
                             tabPanel(h5("Segmentación"),
                                      # h4("Resumen de Cluster"),
                                      # br(),
                                      # dataTableOutput("resumenCluster"),
                                      br(),
                                      h4("Resumen de Cluster"),
                                      br(),
                                      plotOutput("CompR", height = 200),
                                      br(),
                                      plotOutput("CompF", height = 200),
                                      br(),
                                      plotOutput("CompM", height = 200),
                                      br(),
                                      h4("Clústers en las Dos Primeras Dimensiones"),
                                      plotOutput("biplot", height = "600px"),
                                      br(),
                                      h4("Valores Scores RFM"),
                                      fluidRow(
                                          column(4,h4("Frecuencia"),
                                                 tableOutput('TablaFrecuencia_s')
                                          ),
                                          column(4,h4("Recencia"),
                                                 tableOutput('TablaRecencia_s')
                                          ),
                                          column(4,h4("Monto"),
                                                 tableOutput('TablaMonto_s')
                                          ))),
                             tabPanel(h5("Priorización"),
                                      h3("Patologías de Mayor Venta"),
                                      dataTableOutput("TablaPato1"),
                                      br(),
                                      fluidRow(
                                          column(6,h4("Priorización de Recencia"),
                                                 br(),
                                                 dataTableOutput("TablaPato2")
                                          ),
                                          column(6,h4("Priorización de Frecuencia"),
                                                 br(),
                                                 dataTableOutput("TablaPato3")
                                          )
                                      ),
                                      br(),
                                      h4("Valores Scores RFM"),
                                      fluidRow(
                                          column(4,h4("Frecuencia"),
                                                 tableOutput('TablaFrecuencia_p')
                                          ),
                                          column(4,h4("Recencia"),
                                                 tableOutput('TablaRecencia_p')
                                          ),
                                          column(4,h4("Monto"),
                                                 tableOutput('TablaMonto_p')
                                          )
                                      )),
                             tabPanel(h5("Estimación de Impactos"),
                                      h4("Estimación de Ganancia en Monto por incremento de Score"),
                                      br(),
                                      br(),
                                      h4("Ganancia de Monto por cambio de score de recencia"),
                                      br(),
                                      tableOutput('Modelo_R'),
                                      br(),
                                      h4("Ganancia de Monto por cambio de score de frecuencia"),
                                      br(),
                                      tableOutput('Modelo_F'),
                                      br(),
                                      h4("Valores Scores RFM"),
                                      fluidRow(
                                          column(4,h4("Frecuencia"),
                                                 tableOutput('TablaFrecuencia_m')
                                                 ),
                                          column(4,h4("Recencia"),
                                                 tableOutput('TablaRecencia_m')
                                                 ),
                                          column(4,h4("Monto"),
                                                 tableOutput('TablaMonto_m')
                                                 )
                                          )),
                             tabPanel(h5("Tops Interactivos"),
                                      # verbatimTextOutput("default"),
                                      fluidRow(
                                          column(4,
                                                 selectInput("VariableTop", label = h4("Seleccione la Variable del TOP"),
                                                             choices = list("Patología"="Patologia",
                                                                            "Categoría"="Categoria",
                                                                            "Sucursal"="nombre_Sucursal",
                                                                            "Producto" = "prod_nombre",
                                                                            "Proveedor"="prod_proveedor_nombre",
                                                                            "Jerarquía 1"="jerarquia1",
                                                                            "Jerarquía 2"="jerarquia2",
                                                                            "Principio Activo"="principioconcatenado",
                                                                            "Paciente"="Id_Cliente"))
                                                 ),
                                          column(3,
                                                 numericInput("top", label=h4("Número de filas a Mostrar"), 
                                                              10, min = 1, max = 100)
                                          ),
                                          column(5, 
                                                 selectInput("VariableOrden", label=h4("Seleccione la Variable de Orden"),
                                                             choices=list(
                                                                 "Pacientes"="Pacientes",
                                                                 "Venta Bruta"="VentaBruta",
                                                                 "Venta Neta"="VentaNeta",
                                                                 "Frecuencia de Compra"="Frec",
                                                                 "Unidades"="Cantidad",
                                                                 "Uso Promedio"="UsoPromedio",
                                                                 "Venta Bruta Promedio"="VentaBPromedio",
                                                                 "Venta Neta Promedio"="VentaNPromedio"))
                                                 )
                                      ),
                                      dataTableOutput("TopDInamico")
                                      ),
                             tabPanel(h5("Descargas"),
                                      fluidRow(
                                          column(4,h4("Frecuencia"),
                                                 tableOutput('TablaFrecuencia_d')
                                          ),
                                          column(4,h4("Recencia"),
                                                 tableOutput('TablaRecencia_d')
                                          ),
                                          column(4,h4("Monto"),
                                                 tableOutput('TablaMonto_d')
                                          )
                                      ),
                                      br(),
                                      fluidRow(
                                          column(3,h4("Score de Recencia"),
                                                 selectInput("Recencia", " ",choices=c("1","2","3","4","5"), 
                                                             multiple = T, selected=c("1","2","3","4","5"))
                                          ),
                                          column(3,h4("Score de Frecuencia"),
                                                 selectInput("Frecuencia", " ",choices=c("1","2","3","4","5"), 
                                                             multiple = T, selected=c("1","2","3","4","5"))
                                          ),
                                          column(3,h4("Score de Monto"),
                                                 selectInput("Monto", " ",choices=c("1","2","3","4","5"), 
                                                             multiple = T, selected=c("1","2","3","4","5"))
                                          ),
                                          column(3,h4("Segmento"),
                                                 selectInput("Cluster", " ",choices=c("1","2","3","4","5"), 
                                                             multiple = T, selected=c("1","2","3","4","5")))
                                      ),
                                      br(),
                                      h3("Listado de Pacientes según puntaje RFM (Previsualizacón)"),
                                      checkboxGroupInput("Vars_Prod", label = h3("Variables a Incluir"), 
                                                         choices = list("Principio Activo" = "principioconcatenado", 
                                                                        "Producto" = "prod_nombre"), inline=T),
                                      dataTableOutput("TablaDesc"),
                                      br(),
                                      downloadButton("downloadData", "Descargar")
                                      )
                             # navbarMenu(h5("Descripciones"),
                             #            tabPanel("Referenciación Geográfica"),
                             #            tabPanel("Descripción Demográfica")
                             #            )
                             )
                  )
        )
    )
)