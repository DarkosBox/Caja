function(input, output) {

### Bases de Datos Reactivas ----
  
  bd_mapa<-reactive({
    aux1 <- puntos %>% 
      filter (Punto == input$punto) %>% 
      select (id=Punto, long=CY, lat=CX)
  })
  
  bd_v<-reactive({
    aux1<- db %>% filter(!is.na(cx))
    aux1$Dist_V<-round(distHaversine(aux1[,2:3], bd_mapa()[,c(2,3)])/1000,1)
    aux1<-aux1 %>% filter(Dist_V<=input$Distancia)
  })
  
  bd_t<-reactive({
    aux1<- db %>% filter(!is.na(cx_emp))
    aux1$Dist_V<-round(distHaversine(aux1[,14:15], bd_mapa()[,c(2,3)])/1000,1)
    aux1<-aux1 %>% filter(Dist_V<=input$Distancia)
  })
  
# ### Mapas de Residencia ----
#   
#   output$MapaVivien <- renderLeaflet({
#     leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 10, maxZoom = 14)) %>%
#       addProviderTiles(providers$Stamen.Toner) %>%
#       setView(lat=4.6477908, lng = -74.108419, zoom = 11) %>%
#       addHeatmap(data = db, lat = ~cy, lng = ~cx, radius=9, max=0.3)
#     })
#   
#   output$Propension <- renderLeaflet({
# 
#     Recreacion <- makeIcon(
#       iconUrl = "Recreacion.png",
#       iconWidth = 30, iconHeight = 40)
# 
#     leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 11, maxZoom = 13)) %>%
#       addProviderTiles(providers$Stamen.Toner) %>%
#       setView(lat=4.6477908, lng = -74.108419, zoom = 11) %>%
#       addHeatmap(data = db, lat = ~cy, lng = ~cx, intensity = ~Futbol, radius = 7, max=0.1, blur=20, gradient="Greens", group = "Futbol")%>%
#       addHeatmap(data = db, lat = ~cy, lng = ~cx, intensity = ~Juegos, radius = 7, max=0.1, blur=20, gradient="Blues", group = "Juegos") %>%
#       addHeatmap(data = db, lat = ~cy, lng = ~cx, intensity = ~Fisico, radius = 7, max=0.1, blur=20, gradient="Oranges", group = "Físico") %>%
#       addHeatmap(data = db, lat = ~cy, lng = ~cx, intensity = ~Yoga, radius = 7, max=0.1, blur=20, gradient="Reds", group = "Yoga") %>%
#       addHeatmap(data = db, lat = ~cy, lng = ~cx, intensity = ~Infantil, radius = 7, max=0.1, blur=20, gradient="Purples", group = "Infantil") %>%
#       addHeatmap(data = db, lat = ~cy, lng = ~cx, intensity = ~Membresia, radius = 7, max=0.1, blur=20, gradient="Greys", group = "Membresia") %>%
#       addMarkers(data = puntos, lng = ~CY, lat = ~CX, label=~Punto, icon = Recreacion) %>%
#       addLayersControl(baseGroups = c("Futbol", "Juegos", "Físico", "Yoga", "Infantil", "Membresia"),
#                        options = layersControlOptions(collapsed = FALSE))
#   })
#   
# ### Mapas de Trabajo ----

### Hoja de Resumen -----

  output$MapaPunto <- renderLeaflet({
    Recreacion <- makeIcon(
      iconUrl = "Recreacion.png",
      iconWidth = 30, iconHeight = 40)
    
    aux_punto<-bd_mapa()
    
    leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 12, maxZoom = 18)) %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
      setView(lat=aux_punto$lat, lng = aux_punto$long, zoom = 13) %>%
      addMarkers(data = aux_punto, lng = ~long, lat = ~lat, label=~id, icon = Recreacion) %>% 
      addCircles(data = aux_punto, lng = ~long, lat = ~lat, color = "steelblue", radius = input$Distancia*1000)
  })
  output$Afiliados <- renderValueBox({
    aux1<-bd_v()
    valueBox(
      value = formatC(length(unique(aux1$id_afiliado)), digits = 0, format = "d", big.mark=","),
      subtitle = paste0("Afiliados que viven a una distancia menor a ", input$Distancia, " kilometros del punto ", input$punto),
      icon = icon("users"),
      color = "blue"
    )
  })
  output$Empresas <- renderValueBox({
    aux1<-bd_t()
    valueBox(
             value = formatC(length(unique(aux1$id_afiliado)), digits = 0, format = "d", big.mark=","),
             subtitle = paste0("Afiliados que trabajan a una distancia menor a ", input$Distancia, " kilometros del punto ", input$punto),
             icon = icon("building"),
             color = "aqua"
    )
  })
  output$Futbol <- renderDataTable({
    t1<-bd_v() %>%
      select(id_afiliado, var=Futbol) %>% 
      mutate(Rango=ifelse(var<=.2,"1. Menor al 20%",
                          ifelse(var<=.4,"2. Entre 20% y 40%",
                                 ifelse(var<=.6,"3. Entre 40% y 60%",
                                        ifelse(var<=.8,"4. Entre 60% y 80%","5. Más de 80%"))))
      ) %>% 
      group_by(Rango) %>% 
      summarise(Viven=n_distinct(id_afiliado))
    
    t2<-bd_t() %>%
      select(id_afiliado, var=Futbol) %>% 
      mutate(Rango=ifelse(var<=.2,"1. Menor al 20%",
                          ifelse(var<=.4,"2. Entre 20% y 40%",
                                 ifelse(var<=.6,"3. Entre 40% y 60%",
                                        ifelse(var<=.8,"4. Entre 60% y 80%","5. Más de 80%"))))
      ) %>% 
      group_by(Rango) %>% 
      summarise(Trabajan=n_distinct(id_afiliado))
    
    t3<-t1 %>% full_join(t2, by=c("Rango"="Rango"))
    
    datatable(t3, options=list(pageLength = 20,dom = 't',searching= FALSE), rownames=F, 
              colnames = c("Rango", "Viven", "Trabajan")) %>%
      formatRound(c("Viven","Trabajan"), digits = 0)
  })
  output$Juegos <- renderDataTable({
    t1<-bd_v() %>%
      select(id_afiliado, var=Juegos) %>% 
      mutate(Rango=ifelse(var<=.2,"1. Menor al 20%",
                          ifelse(var<=.4,"2. Entre 20% y 40%",
                                 ifelse(var<=.6,"3. Entre 40% y 60%",
                                        ifelse(var<=.8,"4. Entre 60% y 80%","5. Más de 80%"))))
      ) %>% 
      group_by(Rango) %>% 
      summarise(Viven=n_distinct(id_afiliado))
    
    t2<-bd_t() %>%
      select(id_afiliado, var=Juegos) %>% 
      mutate(Rango=ifelse(var<=.2,"1. Menor al 20%",
                          ifelse(var<=.4,"2. Entre 20% y 40%",
                                 ifelse(var<=.6,"3. Entre 40% y 60%",
                                        ifelse(var<=.8,"4. Entre 60% y 80%","5. Más de 80%"))))
      ) %>% 
      group_by(Rango) %>% 
      summarise(Trabajan=n_distinct(id_afiliado))
    
    t3<-t1 %>% full_join(t2, by=c("Rango"="Rango"))
    
    datatable(t3, options=list(pageLength = 20,dom = 't',searching= FALSE), rownames=F, 
              colnames = c("Rango", "Viven", "Trabajan")) %>%
      formatRound(c("Viven","Trabajan"), digits = 0)
  })
  output$Fisico <- renderDataTable({
    t1<-bd_v() %>%
      select(id_afiliado, var=Fisico) %>% 
      mutate(Rango=ifelse(var<=.2,"1. Menor al 20%",
                          ifelse(var<=.4,"2. Entre 20% y 40%",
                                 ifelse(var<=.6,"3. Entre 40% y 60%",
                                        ifelse(var<=.8,"4. Entre 60% y 80%","5. Más de 80%"))))
      ) %>% 
      group_by(Rango) %>% 
      summarise(Viven=n_distinct(id_afiliado))
    
    t2<-bd_t() %>%
      select(id_afiliado, var=Fisico) %>% 
      mutate(Rango=ifelse(var<=.2,"1. Menor al 20%",
                          ifelse(var<=.4,"2. Entre 20% y 40%",
                                 ifelse(var<=.6,"3. Entre 40% y 60%",
                                        ifelse(var<=.8,"4. Entre 60% y 80%","5. Más de 80%"))))
      ) %>% 
      group_by(Rango) %>% 
      summarise(Trabajan=n_distinct(id_afiliado))
    
    t3<-t1 %>% full_join(t2, by=c("Rango"="Rango"))
    
    datatable(t3, options=list(pageLength = 20,dom = 't',searching= FALSE), rownames=F, 
              colnames = c("Rango", "Viven", "Trabajan")) %>%
      formatRound(c("Viven","Trabajan"), digits = 0)
  })
  output$Yoga <- renderDataTable({
    t1<-bd_v() %>%
      select(id_afiliado, var=Yoga) %>% 
      mutate(Rango=ifelse(var<=.2,"1. Menor al 20%",
                          ifelse(var<=.4,"2. Entre 20% y 40%",
                                 ifelse(var<=.6,"3. Entre 40% y 60%",
                                        ifelse(var<=.8,"4. Entre 60% y 80%","5. Más de 80%"))))
      ) %>% 
      group_by(Rango) %>% 
      summarise(Viven=n_distinct(id_afiliado))
    
    t2<-bd_t() %>%
      select(id_afiliado, var=Yoga) %>% 
      mutate(Rango=ifelse(var<=.2,"1. Menor al 20%",
                          ifelse(var<=.4,"2. Entre 20% y 40%",
                                 ifelse(var<=.6,"3. Entre 40% y 60%",
                                        ifelse(var<=.8,"4. Entre 60% y 80%","5. Más de 80%"))))
      ) %>% 
      group_by(Rango) %>% 
      summarise(Trabajan=n_distinct(id_afiliado))
    
    t3<-t1 %>% full_join(t2, by=c("Rango"="Rango"))
    
    datatable(t3, options=list(pageLength = 20,dom = 't',searching= FALSE), rownames=F, 
              colnames = c("Rango", "Viven", "Trabajan")) %>%
      formatRound(c("Viven","Trabajan"), digits = 0)
  })
  output$Infantiles <- renderDataTable({
    t1<-bd_v() %>%
      select(id_afiliado, var=Infantil) %>% 
      mutate(Rango=ifelse(var<=.2,"1. Menor al 20%",
                          ifelse(var<=.4,"2. Entre 20% y 40%",
                                 ifelse(var<=.6,"3. Entre 40% y 60%",
                                        ifelse(var<=.8,"4. Entre 60% y 80%","5. Más de 80%"))))
      ) %>% 
      group_by(Rango) %>% 
      summarise(Viven=n_distinct(id_afiliado))
    
    t2<-bd_t() %>%
      select(id_afiliado, var=Infantil) %>% 
      mutate(Rango=ifelse(var<=.2,"1. Menor al 20%",
                          ifelse(var<=.4,"2. Entre 20% y 40%",
                                 ifelse(var<=.6,"3. Entre 40% y 60%",
                                        ifelse(var<=.8,"4. Entre 60% y 80%","5. Más de 80%"))))
      ) %>% 
      group_by(Rango) %>% 
      summarise(Trabajan=n_distinct(id_afiliado))
    
    t3<-t1 %>% full_join(t2, by=c("Rango"="Rango"))
    
    datatable(t3, options=list(pageLength = 20,dom = 't',searching= FALSE), rownames=F, 
              colnames = c("Rango", "Viven", "Trabajan")) %>%
      formatRound(c("Viven","Trabajan"), digits = 0)
  })
  output$Membresia <- renderDataTable({
    t1<-bd_v() %>%
      select(id_afiliado, var=Membresia) %>% 
      mutate(Rango=ifelse(var<=.2,"1. Menor al 20%",
                          ifelse(var<=.4,"2. Entre 20% y 40%",
                                 ifelse(var<=.6,"3. Entre 40% y 60%",
                                        ifelse(var<=.8,"4. Entre 60% y 80%","5. Más de 80%"))))
      ) %>% 
      group_by(Rango) %>% 
      summarise(Viven=n_distinct(id_afiliado))
    
    t2<-bd_t() %>%
      select(id_afiliado, var=Membresia) %>% 
      mutate(Rango=ifelse(var<=.2,"1. Menor al 20%",
                          ifelse(var<=.4,"2. Entre 20% y 40%",
                                 ifelse(var<=.6,"3. Entre 40% y 60%",
                                        ifelse(var<=.8,"4. Entre 60% y 80%","5. Más de 80%"))))
      ) %>% 
      group_by(Rango) %>% 
      summarise(Trabajan=n_distinct(id_afiliado))
    
    t3<-t1 %>% full_join(t2, by=c("Rango"="Rango"))
    
    datatable(t3, options=list(pageLength = 20,dom = 't',searching= FALSE), rownames=F, 
              colnames = c("Rango", "Viven", "Trabajan")) %>%
      formatRound(c("Viven","Trabajan"), digits = 0)
  })
  output$Resumen <- renderDataTable({
    aux1<-bd_t() %>% 
      group_by(`Piramide 1`, `Piramide 2`, `RAZON SOCIAL`) %>% 
      summarise(Afiliados=n_distinct(id_afiliado),
                Futbol=mean(Futbol),
                Juegos=mean(Juegos),
                Fisico=mean(Fisico),
                Yoga=mean(Yoga),
                Infantiles=mean(Infantil),
                Membresia=mean(Membresia)
                )
    
    datatable(aux1, options=list(pageLength = 10,dom = 'ltl',searching= FALSE), rownames=F) %>%
      formatRound(c("Afiliados"), digits = 0) %>% 
      formatRound(c("Futbol","Juegos","Fisico","Yoga","Infantiles", "Membresia"), digits=4)
  })
  
### Ficha Técnica ----
  
  
  
}
