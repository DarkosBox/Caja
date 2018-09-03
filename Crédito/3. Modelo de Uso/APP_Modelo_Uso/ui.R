<<<<<<< HEAD
options(shiny.maxRequestSize = 30*1024^2)

shinyUI(
  fluidPage(
    title = "Análisis de Medicamentos",
    tags$head(includeCSS(file.path("www", "app.css"))),
    tags$b(
      href="https://www.colsubsidioenlinea.com/",
      tags$img(style="position: absolute; top: 0; right:0; border: 0;",src="Logo.png"),
      tags$img(style="position: absolute; top: 0; left:0; border: 0;",src="logo_colsubsidio.png")
      ),
    div(id = "header",
        div(id = "title",
            "Modelo de Incentivo de Uso TMS"
            ),
        div(id = "subsubtitle",
            "Inteligencia de Negocios",
            HTML("&bull;"),
            "Subdirección Comercial")
        ),
    navbarPage("", 
               tabPanel("Instructivo",
                        h4("El presente archivo permite determinar, para ciertos clientes que no han usado la 
                           tarjeta multiservicios, el establecimiento en el cual es más probable que puedan 
                           usarla. El modelo permite distinguir si la compra más probable de un cliente será en 
                           un establecimiento de Colsubsidio o en un Convenio. En el primer caso se informara la UES de mayor 
                           probabilidad, en el segundo caso se informará la categoría del convenio seleccionado",
                           style='text-align: justify'),
                        br(),
                        h4("El archivo se divide en dos formas de estimacion:"),
                        tags$li(h4("Calificación Masiva")),
                        tags$li(h4("Calificación individual")),
                        br(),
                        h3("Calificación Masiva"),
                        br(),
                        h4("Para la calificación masiva es necesario cargar un archivo en extensión csv con las siguientes variables:"),
                        br(),
                        img(src='Tabla1.png', align = "center"),
                        br(),
                        h4("La actividad econcopmica debe tener alguno de los siguiente valores: PRESTACION DE SERVICIOS (EDUCACION, BANCA, COMUNICACIONES, SEGUROS, SALUD, ARTE Y 
                           CULTURA);  INDUSTRIA,COMERCIO AL POR MAYOR Y AL POR MENOR; OTROS; CONSTRUCCION, 
                           DEMOLICIONES, TERRENOS, VIAS.; PUBLICO; TRANSPORTE Y ALMACENAMIENTO, TEMPORALES; 
                           AGRICULTURA, CAZA, SILVICULTURA Y PESCA, ELECTRICIDAD, GAS, AGUA Y EXPLOTACION DE 
                           MINAS."),
                        br(),
                        h4("La zona de residencia debe ser alguno de los siguientes valores: MUNICIPIOS ;ZONA CENTRO; 
                           ZONA CHAPINERO; ZONA NORTE; ZONA SUR o AUSENTE"),
                        br(),
                        h4("Una vez se califique la base de datos, es posible descargar en formato csv los resultados del modelo."),
                        br(),
                        h3("Calificación Individual"),
                        br(),
                        h4("Se deben ingresar las características de un cliente en cada una de las variables del menú superior. Por último, el archivo informará cúal es la mejor asignación de campaña a cada cliente"),
                        br()
                        ),
                       tabPanel("Evaluación Masiva",
                                column(h2("Cargue del archivo plano"),width = 3,
                                       style = "background-color:#E6E6E6;",
                                       fileInput("file1", h5("Seleccione el archivo (.csv)"),
                                                 buttonLabel = "Cargar",
                                                 placeholder = "Sin archivo seleccionado",
                                                 multiple = F,
                                                 accept = c("text/csv",
                                                            "text/comma-separated-values,text/plain",
                                                            ".csv")),
                                       tags$hr(),
                                       checkboxInput("header", "Encabezado", TRUE),
                                       radioButtons("sep", "Separador de Columnas",
                                                    choices = c("Coma" = ",",
                                                                "Punto y Coma"= ";",
                                                                "Tabulación" = "\t"),
                                                    selected = ";"),
                                       radioButtons("dates", "Formato de Fecha",
                                                    choices = c("dd/mm/aaaa" = "%d/%m/%Y",
                                                                "mm/dd/aaaa" = "%m/%d/%Y",
                                                                "ddmmaaaa" = "%d%m%Y",
                                                                "mmddaaaa" = "%m%d%Y"),
                                                    selected = "%d/%m/%Y"),
                                       tags$hr(),
                                       dateInput("date", "Fecha de Evaluación:", value = floor_date(Sys.Date(), "month") - days(1), 
                                                 format = "dd/mm/yyyy",  language = "es")
                                       ),
                                column(width = 9,
                                       h3("Previsualización de Datos"),
                                       br(),
                                       dataTableOutput("Preview"),
                                       br(),
                                       textOutput("Nota"),
                                       br(),
                                       h3("Base de datos Calificada"),
                                       br(),
                                       dataTableOutput("Calificada"),
                                       br(),
                                       downloadButton("downloadData", "Descargar base Calificada"),
                                       br()
                                       )
                                ),
                       tabPanel("Evaluación Individual",
                                fluidPage(
                                  fluidRow(style = "background-color:#E6E6E6;",
                                           column(width = 2, style='border-right: 2px solid Ivory',
                                                  radioButtons("Genero", h5("Género"),
                                                               choices = c("Mujer" = "F",
                                                                           "Hombre"= "M"),
                                                               selected = "F")),
                                           column(width = 2, style='border-right: 2px solid Ivory',
                                                  dateInput("FechaNacimiento", h5("Fecha de Nacimiento:"), 
                                                            format = "dd/mm/yyyy",  language = "es", value="1990-01-01")),
                                           column(width = 2, style='border-right: 2px solid Ivory',
                                                  radioButtons("Ciudad", h5("Ciudad de Residencia"),
                                                               choices = c("Bogotá" = "BOGOTA",
                                                                           "Otros"= "Otros"),
                                                               selected = "BOGOTA")),
                                           column(width = 2, style='border-right: 2px solid Ivory',
                                                  selectInput("Segmento", h5("Segmento Individual"),
                                                              c("Básico" = "Básico",
                                                                "Medio" = "Medio",
                                                                "Joven" = "Joven",
                                                                "Alto"="Alto"))),
                                           column(width = 2, style='border-right: 2px solid Ivory',
                                                  numericInput("Salario", h5("Salario"), 1000000)),
                                           column(width = 2,
                                                  numericInput("CIIU", h5("Código CIIU"), 50000))
                                           ),
                                  br(),
                                  fluidRow(style = "background-color:#E6E6E6;",
                                           column(width = 4, style='border-right: 2px solid Ivory',
                                                  selectInput("Actividad", h5("Actividad Económica"),width='100%',
                                                              c("Prestacion de Servicios (Educacion, Banca, Comunicaciones, Seguros, Salud, Arte y Cultura)",
                                                                "Industria","Comercio al por Mayor y al por Menor","Otros","Construccion, demoliciones, terrenos, Vias.",
                                                                "Publico","Transporte y Almacenamiento"="Tansporte y Almacenamiento","Temporales",
                                                                "Agricultura, Caza, Silvicultura y pesca","Electricidad, Gas, Agua y explotacion de minas"),
                                                              selected = "Industria")),
                                           column(width = 2, style='border-right: 2px solid Ivory',
                                                  selectInput("Zona", h5("Zona Geográfica"),
                                                              c("Municipios"="1",
                                                                "Zona Centro"="2",
                                                                "Zona Chapinero"="3",
                                                                "Zona Norte"="4",
                                                                "Zona Sur"="5",
                                                                "Ausente"="6"), selected = "2")),
                                           column(width = 2, style='border-right: 2px solid Ivory',
                                                  dateInput("FechaApertura", h5("Fecha de Apertura"), 
                                                            format = "dd/mm/yyyy",  language = "es")),
                                           column(width = 2, style='border-right: 2px solid Ivory',
                                                  numericInput("Cupo", h5("Límite de Cupo"), 500000)),
                                           column(width = 2, style='border-right: 2px solid Ivory',
                                                  radioButtons("Amparada", h5("Amparada"),
                                                               choices = c("Si" = "2",
                                                                           "No"= "1"),
                                                               selected = "1"))
                                           ),
                                  br(),
                                  fluidRow(
                                    column(width= 1),
                                    column(width = 10,
                                           textOutput("Pred")
                                           ),
                                    column(width= 1)
                                    )
                                  )
                                )
                       )
            )
  )
=======
options(shiny.maxRequestSize = 30*1024^2)

shinyUI(
  fluidPage(
    title = "Análisis de Medicamentos",
    tags$head(includeCSS(file.path("www", "app.css"))),
    tags$b(
      href="https://www.colsubsidioenlinea.com/",
      tags$img(style="position: absolute; top: 0; right:0; border: 0;",src="Logo.png"),
      tags$img(style="position: absolute; top: 0; left:0; border: 0;",src="logo_colsubsidio.png")
      ),
    div(id = "header",
        div(id = "title",
            "Modelo de Incentivo de Uso TMS"
            ),
        div(id = "subsubtitle",
            "Inteligencia de Negocios",
            HTML("&bull;"),
            "Subdirección Comercial")
        ),
    navbarPage("", 
               tabPanel("Instructivo",
                        h4("El presente archivo permite determinar, para ciertos clientes que no han usado la 
                           tarjeta multiservicios, el establecimiento en el cual es más probable que puedan 
                           usarla. El modelo permite distinguir si la compra más probable de un cliente será en 
                           un establecimiento de Colsubsidio o en un Convenio. En el primer caso se informara la UES de mayor 
                           probabilidad, en el segundo caso se informará la categoría del convenio seleccionado",
                           style='text-align: justify'),
                        br(),
                        h4("El archivo se divide en dos formas de estimacion:"),
                        tags$li(h4("Calificación Masiva")),
                        tags$li(h4("Calificación individual")),
                        br(),
                        h3("Calificación Masiva"),
                        br(),
                        h4("Para la calificación masiva es necesario cargar un archivo en extensión csv con las siguientes variables:"),
                        br(),
                        img(src='Tabla1.png', align = "center"),
                        br(),
                        h4("La actividad econcopmica debe tener alguno de los siguiente valores: PRESTACION DE SERVICIOS (EDUCACION, BANCA, COMUNICACIONES, SEGUROS, SALUD, ARTE Y 
                           CULTURA);  INDUSTRIA,COMERCIO AL POR MAYOR Y AL POR MENOR; OTROS; CONSTRUCCION, 
                           DEMOLICIONES, TERRENOS, VIAS.; PUBLICO; TRANSPORTE Y ALMACENAMIENTO, TEMPORALES; 
                           AGRICULTURA, CAZA, SILVICULTURA Y PESCA, ELECTRICIDAD, GAS, AGUA Y EXPLOTACION DE 
                           MINAS."),
                        br(),
                        h4("La zona de residencia debe ser alguno de los siguientes valores: MUNICIPIOS ;ZONA CENTRO; 
                           ZONA CHAPINERO; ZONA NORTE; ZONA SUR o AUSENTE"),
                        br(),
                        h4("Una vez se califique la base de datos, es posible descargar en formato csv los resultados del modelo."),
                        br(),
                        h3("Calificación Individual"),
                        br(),
                        h4("Se deben ingresar las características de un cliente en cada una de las variables del menú superior. Por último, el archivo informará cúal es la mejor asignación de campaña a cada cliente"),
                        br()
                        ),
                       tabPanel("Evaluación Masiva",
                                column(h2("Cargue del archivo plano"),width = 3,
                                       style = "background-color:#E6E6E6;",
                                       fileInput("file1", h5("Seleccione el archivo (.csv)"),
                                                 buttonLabel = "Cargar",
                                                 placeholder = "Sin archivo seleccionado",
                                                 multiple = F,
                                                 accept = c("text/csv",
                                                            "text/comma-separated-values,text/plain",
                                                            ".csv")),
                                       tags$hr(),
                                       checkboxInput("header", "Encabezado", TRUE),
                                       radioButtons("sep", "Separador de Columnas",
                                                    choices = c("Coma" = ",",
                                                                "Punto y Coma"= ";",
                                                                "Tabulación" = "\t"),
                                                    selected = ";"),
                                       radioButtons("dates", "Formato de Fecha",
                                                    choices = c("dd/mm/aaaa" = "%d/%m/%Y",
                                                                "mm/dd/aaaa" = "%m/%d/%Y",
                                                                "ddmmaaaa" = "%d%m%Y",
                                                                "mmddaaaa" = "%m%d%Y"),
                                                    selected = "%d/%m/%Y"),
                                       tags$hr(),
                                       dateInput("date", "Fecha de Evaluación:", value = floor_date(Sys.Date(), "month") - days(1), 
                                                 format = "dd/mm/yyyy",  language = "es")
                                       ),
                                column(width = 9,
                                       h3("Previsualización de Datos"),
                                       br(),
                                       dataTableOutput("Preview"),
                                       br(),
                                       textOutput("Nota"),
                                       br(),
                                       h3("Base de datos Calificada"),
                                       br(),
                                       dataTableOutput("Calificada"),
                                       br(),
                                       downloadButton("downloadData", "Descargar base Calificada"),
                                       br()
                                       )
                                ),
                       tabPanel("Evaluación Individual",
                                fluidPage(
                                  fluidRow(style = "background-color:#E6E6E6;",
                                           column(width = 2, style='border-right: 2px solid Ivory',
                                                  radioButtons("Genero", h5("Género"),
                                                               choices = c("Mujer" = "F",
                                                                           "Hombre"= "M"),
                                                               selected = "F")),
                                           column(width = 2, style='border-right: 2px solid Ivory',
                                                  dateInput("FechaNacimiento", h5("Fecha de Nacimiento:"), 
                                                            format = "dd/mm/yyyy",  language = "es", value="1990-01-01")),
                                           column(width = 2, style='border-right: 2px solid Ivory',
                                                  radioButtons("Ciudad", h5("Ciudad de Residencia"),
                                                               choices = c("Bogotá" = "BOGOTA",
                                                                           "Otros"= "Otros"),
                                                               selected = "BOGOTA")),
                                           column(width = 2, style='border-right: 2px solid Ivory',
                                                  selectInput("Segmento", h5("Segmento Individual"),
                                                              c("Básico" = "Básico",
                                                                "Medio" = "Medio",
                                                                "Joven" = "Joven",
                                                                "Alto"="Alto"))),
                                           column(width = 2, style='border-right: 2px solid Ivory',
                                                  numericInput("Salario", h5("Salario"), 1000000)),
                                           column(width = 2,
                                                  numericInput("CIIU", h5("Código CIIU"), 50000))
                                           ),
                                  br(),
                                  fluidRow(style = "background-color:#E6E6E6;",
                                           column(width = 4, style='border-right: 2px solid Ivory',
                                                  selectInput("Actividad", h5("Actividad Económica"),width='100%',
                                                              c("Prestacion de Servicios (Educacion, Banca, Comunicaciones, Seguros, Salud, Arte y Cultura)",
                                                                "Industria","Comercio al por Mayor y al por Menor","Otros","Construccion, demoliciones, terrenos, Vias.",
                                                                "Publico","Transporte y Almacenamiento"="Tansporte y Almacenamiento","Temporales",
                                                                "Agricultura, Caza, Silvicultura y pesca","Electricidad, Gas, Agua y explotacion de minas"),
                                                              selected = "Industria")),
                                           column(width = 2, style='border-right: 2px solid Ivory',
                                                  selectInput("Zona", h5("Zona Geográfica"),
                                                              c("Municipios"="1",
                                                                "Zona Centro"="2",
                                                                "Zona Chapinero"="3",
                                                                "Zona Norte"="4",
                                                                "Zona Sur"="5",
                                                                "Ausente"="6"), selected = "2")),
                                           column(width = 2, style='border-right: 2px solid Ivory',
                                                  dateInput("FechaApertura", h5("Fecha de Apertura"), 
                                                            format = "dd/mm/yyyy",  language = "es")),
                                           column(width = 2, style='border-right: 2px solid Ivory',
                                                  numericInput("Cupo", h5("Límite de Cupo"), 500000)),
                                           column(width = 2, style='border-right: 2px solid Ivory',
                                                  radioButtons("Amparada", h5("Amparada"),
                                                               choices = c("Si" = "2",
                                                                           "No"= "1"),
                                                               selected = "1"))
                                           ),
                                  br(),
                                  fluidRow(
                                    column(width= 1),
                                    column(width = 10,
                                           textOutput("Pred")
                                           ),
                                    column(width= 1)
                                    )
                                  )
                                )
                       )
            )
  )
>>>>>>> 5308310d9fd680f7f63a4eff58a3d19ee6c67bee
  