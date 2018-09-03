rm(list=ls(all=T))
source("C:/Users/hernyatt/Documents/CamiloYate/Funciones.R")
pkgs <- c("data.table", "tidyverse", "leaflet.extras", "geosphere", "GGally")
Loadpkg(pkgs)

BD_calif<-fread("BD/Depuradas/BD_Calif.csv")
puntos<-fread("BD/Puntos.csv")


# Localizacion ausente 
persona<-fread("BD/Persona.csv", stringsAsFactors = T) %>% 
  select(id_persona, CX, CY) %>% 
  filter(is.na(CX))
names(persona)<-Limpiar.Cadenas(names(persona))

# Nombre de Empresa - Localización

geo_emp<-fread("BD/Geoempresas.csv")
empresa_geo<-fread("BD/Persona.csv") %>% 
  select(1,2) %>% 
  left_join(geo_emp, by=c("id_empresa"="IDEMP")) %>% 
  rename(cx_emp=CX, cy_emp=CY)
  
db<-BD_calif %>% select(id_afiliado, cx, cy, Futbol=`0`, Juegos=`1`, Fisico=`2`, Yoga=`3`, Infantil=`4`) %>% 
  mutate_at(c("cx", "cy"), funs(ifelse(id_afiliado %in% persona$id_persona, NA, .))) %>% 
  mutate(Membresia=((Futbol*1)+(Juegos*4)+(Fisico*5)+(Yoga*3)+(Infantil*2))/15) %>% 
  left_join(empresa_geo, by=c("id_afiliado"="id_persona"))

saveRDS(db, "Protocolo/Dados/Data/bd.rds")

#### Mapas Individuales ----

Recreacion <- makeIcon(
  iconUrl = "Recreacion.png",
  iconWidth = 30, iconHeight = 40)

Imprimir.Mapa<-function(var){
  var1=db[,var]
  leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 11, maxZoom = 13)) %>% 
    addProviderTiles(providers$Stamen.Toner) %>% 
    setView(lat=4.6477908, lng = -74.108419, zoom = 11) %>% 
    addHeatmap(data = db, lat = ~cy, lng = ~cx, intensity = var1, radius = 7, max=0.1, blur=20, gradient="Greys") %>% 
    addMarkers(data = puntos, lng = ~CY, lat = ~CX, popup=~Punto, icon = Recreacion)
}

Imprimir.Mapa("Futbol")
Imprimir.Mapa("Juegos")
Imprimir.Mapa("Fisico")
Imprimir.Mapa("Yoga")
Imprimir.Mapa("Infantil")

#### Gráficas
ggpairs(BD_calif[,32:36])
names(BD_calif)
  