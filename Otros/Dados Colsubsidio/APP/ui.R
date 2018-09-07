sidebar <- dashboardSidebar(width="200px",
  sidebarMenu(disable = TRUE, br(),
              tags$img(src = "logo.png", height=40, width=200, align="center"),
              br(),br(),
              menuItem("Estadísticas de Resumen", tabName = "Resumen", icon = icon("area-chart")),
              menuItem("Ficha Técnica", tabName = "Ficha", icon = icon("desktop"))
  )
)

body <- dashboardBody(
  tabItems(
  #   tabItem(tabName = "Residencia",
  #           h2("Mapa de localización de Afiliados según lugar de residencia"),
  #           br(),
  #           leafletOutput("MapaVivien", width='100%', height='600px'),
  #           h2("Mapa de propensión a uso servicios: Dados "),
  #           br(),
  #           leafletOutput("Propension", width='100%', height='600px'),
  #           br(),
  #           h5("La propensión de membresia está calculada como el promedio ponderado según importancia de cada servicio.")
  #   ),
    # tabItem(tabName = "Trabajo",
    #         h2("Mapa de localización de Afiliados según lugar de trabajo")
    # ),
    tabItem(tabName = "Resumen",
            h2("Resumen de propensión por punto propuesto"),
            fluidRow(
              column(width=4,
                     selectInput("punto", label=h3("Seleccione un Punto"), choices = unique(puntos$Punto))
                     ),
              column(width=4,
                     sliderInput("Distancia", label = h3("Distancia a Punto (km)"), min = 0,max = 10, step = 0.1, value=2)
                     )
              ),
            br(),
            fluidRow(
              box(leafletOutput("MapaPunto"), width=8),
              valueBoxOutput("Afiliados", width = 2),
              valueBoxOutput("Empresas", width = 2)
              ),
            fluidRow(
              box(dataTableOutput("Futbol"), title="Fútbol", width=4, status = "primary", solidHeader = F, collapsible = TRUE),
              box(dataTableOutput("Juegos"), title="Juegos", width=4, status = "primary", solidHeader = F, collapsible = TRUE),
              box(dataTableOutput("Fisico"), title="Acondicionamiento Físico", width=4, status = "primary", solidHeader = F, collapsible = TRUE)
            ),
            fluidRow(
              box(dataTableOutput("Yoga"), title="Yoga", width=4, status = "primary", solidHeader = F, collapsible = TRUE),
              box(dataTableOutput("Infantiles"), title="Infantiles", width=4, status = "primary", solidHeader = F, collapsible = TRUE),
              box(dataTableOutput("Membresia"), title="Membresia *", width=4, status = "primary", solidHeader = F, collapsible = TRUE)
            ),
            h5("* La propensión de membresia está calculada como el promedio ponderado según importancia de cada servicio."),
            fluidRow(
              box(dataTableOutput("Resumen"), title="Resumen de Propensión por Empresa" , width=12, collapsible = TRUE)
            ),
            h5("* La propensión de membresia está calculada como el promedio ponderado según importancia de cada servicio.")
            ),
    tabItem(tabName = "Ficha", h1("Ficha Técnica"),br(),
            fluidRow(
              box(tags$img(src = "prep.png", height=450, width=630), title = "Fuentes de Infomación", width = 6),
              box(tags$img(src = "ficha.png", height=450, width=630), title = "Ficha Técnica del Modelo", width = 6)
            )
            )
    )
  )

dashboardPage(
  dashboardHeader(title = "Modelo de Propensión de Uso de Servicios en los Dados Colsubsidio", titleWidth = 600),
  sidebar,
  body,
  tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }
      ')))
)

