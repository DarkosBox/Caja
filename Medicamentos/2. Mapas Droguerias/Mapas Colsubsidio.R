source("C:/Users/hernyatt/Documents/CamiloYate/Funciones.R")
pkgs <- c("data.table", "tidyverse", "leaflet.extras", "RColorBrewer")
Loadpkg(pkgs)

### Datos ----

afiliados<-fread("~/CamiloYate/2018/Otros/5. Innovacion/1. Proyecto Dados/BD/Persona_3Ago18.csv") %>% 
  select(id_persona, CX, CY) %>% 
  filter(!is.na(CX))

infra<-fread("~/CamiloYate/InfraestructuraColsubsidio.csv") %>% 
  filter(COD_UES=="FARMACIA") %>% 
  select(NOMBRE, CX, CY) %>% 
  mutate_at(c("CX", "CY"), funs(as.numeric(.)))

str(infra)

# Icono
Farmacias <- makeIcon(
  iconUrl = "C:/Users/hernyatt/Pictures/Imagenes/COLSUBSIDIO/IconosNuevos/Farmacias.png",
  iconWidth = 20, iconHeight = 25)

#### Mapas ----

leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 10, maxZoom = 13)) %>% 
  addProviderTiles(providers$Stamen.Toner) %>% 
  setView(lat=4.6477908, lng = -74.108419, zoom = 11) %>% 
  addHeatmap(data = afiliados, lat =~CY , lng =~CX, radius = 15, max=0.5, blur=27, group = "Afilaidos") %>% 
  addMarkers(data= infra, lat =~CY , lng =~CX, icon = Farmacias, popup=~NOMBRE, group = "Droguerias") %>% 
  addLayersControl(overlayGroups  = c("Afilaidos", "Droguerias"), options = layersControlOptions(collapsed = FALSE))
