sidebar <- dashboardSidebar(width="450px",
  sidebarMenu(
    # menuItem("Mapas según lugar de residencia", tabName = "Residencia", icon = icon("globe")),
    # menuItem("Mapas según lugar de trabajo", tabName = "Trabajo", icon = icon("globe")),
    menuItem("Estadísticas de Resumen", tabName = "Resumen", icon = icon("area-chart"))
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
    
    tabItem(tabName = "Trabajo",
            h2("Mapa de localización de Afiliados según lugar de trabajo")
    ),
    tabItem(tabName = "Resumen",
            h2("Resumen de propensión por punto propuesto"),
            fluidRow(
              column(width=4,
                     selectInput("punto", label=h3("Seleccione un Punto"), choices = unique(puntos$Punto))
                     ),
              column(width=4,
                     sliderInput("Distancia", label = h3("Distancia a Punto"), min = 0,max = 10, step = 0.1, value=2)
                     )
              ),
            br(),
            fluidRow(
              box(leafletOutput("MapaPunto"), width=8),
              valueBoxOutput("Afiliados", width = 2),
              valueBoxOutput("Empresas", width = 2)
              ),
            fluidRow(
              box(dataTableOutput("Futbol"), title="Fútbol", width=4),
              box(dataTableOutput("Juegos"), title="Juegos", width=4),
              box(dataTableOutput("Fisico"), title="Acondicionamiento Físico", width=4)
            ),
            fluidRow(
              box(dataTableOutput("Yoga"), title="Yoga", width=4),
              box(dataTableOutput("Infantiles"), title="Infantiles", width=4),
              box(dataTableOutput("Membresia"), title="Membresia", width=4)
            ),
            fluidRow(
              box(dataTableOutput("Resumen"), title="Resumen de Propensión por Empresa" , width=12)
            )
            )
    )
  )

dashboardPage(
  dashboardHeader(title = "Propensión de Uso de Servicios en los Dados Colsubsidio", titleWidth = 450),
  sidebar,
  body
)

