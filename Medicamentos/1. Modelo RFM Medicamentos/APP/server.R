function(input, output) {
    
    ### Bases de Datos reactivas ----
    
    # Filtros: En esta base se aplican los filtros seleccionados por el usuario
    data_f<-eventReactive(input$go,{
        data_f <- BD %>%
            filter(
                Fecha >= input$Fecha[1] & Fecha <=input$Fecha[2],
                if (input$Patologia!="TOTAL") Patologia==input$Patologia else TRUE,
                if (input$Categoria!="TOTAL") Categoria==input$Categoria else TRUE,
                if (input$Principio!="TOTAL") principioconcatenado==input$Principio else TRUE,
                if (input$Producto!="TOTAL") prod_nombre==input$Producto else TRUE,
                if (input$AgrudoCro!="TOTAL") tipodetratamiento==input$AgrudoCro else TRUE,
                if (input$Proveedor!="TOTAL") prod_proveedor_nombre==input$Proveedor else TRUE,
                if (input$Contactable!="TOTAL") autorizacion==input$Contactable else TRUE
                )
        return(data_f)
    })
    
    # RFM: Cálculo del Score RFM a partir de la base filtrada
    bd_rfm<-reactive({
        aux1<-data_f() %>% 
            select(Id_Cliente, Fecha,Venta_Bruta) %>%
            mutate(Recencia=as.numeric(difftime(as.Date(input$Fecha[2]),Fecha,units="days"))) %>%
            group_by(Id_Cliente) %>%
            summarise(Recencia = min(Recencia),
                      Frecuencia=n_distinct(Fecha),
                      Monto=sum(Venta_Bruta, na.rm = T)/Frecuencia
                      ) %>%
            mutate(R_Score=cut2(Recencia, g = 5),
                   F_Score=cut2(Frecuencia, g = 5),
                   M_Score=cut2(Monto, g = 5))
        
        levels(aux1$R_Score)<-seq(5,1, by = -1)
        levels(aux1$F_Score)<-seq(1,5)
        levels(aux1$M_Score)<-seq(1,5)
        return(aux1)
    })
    
    # Cluster: Cálculo de cluster a partir de los puntajes RFM
    clustering<-reactive({
        aux1<-bd_rfm() %>% 
            mutate_at(c("Recencia","Recencia","Monto"), funs(as.numeric))
  
        pca <- prcomp(aux1[,2:4],  scale = T, center = T)
        km_clusters <- kmeans(pca$x,5)
        aux1$Cluster=as.factor(km_clusters$cluster)
        
        aux1<-aux1[,c(1,5:8)]
        return(aux1)
    })
    ?princomp
    
    # Base Final: Consolidacion de la base 
    Data_total<-reactive({
        aux1<-data_f() %>%
            left_join(clustering(), by=c("Id_Cliente"="Id_Cliente"))
        return(aux1)
    })
    
    # Columnas descargas
    cols<-reactive({
        
        cols=c("Id_Cliente", "autorizacion", "Categoria_Afiliado", input$Vars_Prod, "R_Score", "F_Score", "M_Score", "Cluster",
               "Venta_Bruta","Venta_Neta")

        return(cols)
    })
    
    # Base de Descargas
    Descarga1<-reactive({
        
        aux1<-Data_total() %>%
            filter(R_Score %in% input$Recencia,
                   F_Score %in% input$Frecuencia,
                   M_Score %in% input$Monto,
                   Cluster %in% input$Cluster) %>%
            select(cols()) %>% 
            group_by(.dots=cols()) %>% 
            summarise(PromBruta=mean(Venta_Bruta),
                      PromNeta=mean(Venta_Neta),
                      VentaBruta=sum(Venta_Bruta),
                      VentaNeta=sum(Venta_Neta))
        
        return(aux1)
    })
    
    ### Tab Resumen RFM ----
    
    output$ResumenRFM <- renderPlot({
        
        aux2<-bd_rfm() %>%
            group_by(R_Score, F_Score) %>%
            summarise(MontoPromedio=median(Monto), Freq=n())
        
        p1 <- ggplot(aux2, aes(x=F_Score, y=Freq, fill=MontoPromedio))+
            facet_grid(R_Score~.)+
            geom_bar(stat = "identity")+
            ylim(0,max(aux2$Freq)*1.3)+
            geom_text(aes(label=comma(Freq)), position=position_dodge(width=0.5), vjust=-0.5, size=4.4)+
            labs(title = "Puntajes RFM",y="Número de Pacientes", x="Puntaje Frecuencia")+
            scale_fill_gradient2(high="dodgerblue4",mid="gray84",low="firebrick1", labels = dollar,
                                 name="Monto", midpoint = (max(aux2$MontoPromedio)+min(aux2$MontoPromedio))/2)+
            theme(text = element_text(size=12), plot.title = element_text(hjust = 0.5), legend.text = element_text(size=11),
                  legend.position="bottom", legend.key.width = unit(6.5, "cm"))
        p1
    })
    
    #Frecuencia
    output$TablaFrecuencia <- renderTable({
        t1<-bd_rfm() %>% group_by(`Score`=F_Score) %>%
            summarise(`Mínimo`=comma(round(min(Frecuencia))),
                      `Máximo`=comma(round(max(Frecuencia))),
                      Pacientes=comma(round(n_distinct(Id_Cliente))))
        
        }, spacing = 'xs')
    #Recencia
    output$TablaRecencia <- renderTable({
        t1<-bd_rfm() %>% group_by(`Score`=R_Score) %>%
            summarise(`Mínimo`=comma(round(min(Recencia))),
                      `Máximo`=comma(round(max(Recencia))),
                      Pacientes=comma(round(n_distinct(Id_Cliente)))) %>% 
            arrange(desc(Score))

    }, spacing = 'xs')
    #Monto
    output$TablaMonto <- renderTable({
        t1<-bd_rfm() %>% group_by(`Score`=M_Score) %>%
            summarise(`Mínimo`=dollar(round(min(Monto))),
                      `Máximo`=dollar(round(max(Monto))),
                      Pacientes=comma(round(n_distinct(Id_Cliente))))
    }, spacing = 'xs')
   
    ### Segmentación ----
    
    output$resumenCluster <- renderDataTable({
        aux1<-bd_rfm() %>% 
            mutate(ID=seq(1:nrow(bd_rfm()))) %>% 
            select(-Id_Cliente)
        aux1<-as.data.frame(sapply(aux1[, c(1:7)], as.numeric))
        row.names(aux1)<-aux1$ID
        pca <- prcomp(aux1[,4:6],  scale = T, center = T)
        km_clusters <- kmeans(pca$x,5)
        aux1$Cluster=as.factor(km_clusters$cluster)
        
        t1<- aux1 %>% group_by(Cluster) %>% 
            summarise(Pacientes=n_distinct(ID),
                      Recencia=median(Recencia),
                      Frecuencia=median(Frecuencia),
                      Monto=median(Monto))
        
        datatable(t1, options=list(pageLength = 5,dom = 't',searching= FALSE), rownames=F, 
                  colnames = c("Clúster","Pacientes","Recencia Promedio", "Frecuencia  Promedio", "Monto Promedio")) %>% 
            formatCurrency(c("Monto"), digits = 0) %>% 
            formatRound(c("Pacientes","Recencia", "Frecuencia"), digits = 0)
    })
    
    output$CompR <- renderPlot({
        aux1<-bd_rfm() %>% 
            mutate(ID=seq(1:nrow(bd_rfm()))) %>% 
            select(-Id_Cliente)
        aux1<-as.data.frame(sapply(aux1[, c(1:7)], as.numeric))
        row.names(aux1)<-aux1$ID
        pca <- prcomp(aux1[,4:6],  scale = T, center = T)
        km_clusters <- kmeans(pca$x,5)
        aux1$Cluster=as.factor(km_clusters$cluster)
        
        ggplot(aux1, aes(x=Cluster, y=Recencia, fill=Cluster)) +
            geom_boxplot(outlier.shape = NA)+
            geom_hline(yintercept = median(aux1$Recencia), color="darkgreen", linetype="dashed")+
            coord_flip()+
            theme_minimal()+
            labs(title="")+
            scale_y_continuous(limits = quantile(aux1$Recencia, c(0.1, 0.9)), labels = comma)
    })
    output$CompF <- renderPlot({
        aux1<-bd_rfm() %>% 
            mutate(ID=seq(1:nrow(bd_rfm()))) %>% 
            select(-Id_Cliente)
        aux1<-as.data.frame(sapply(aux1[, c(1:7)], as.numeric))
        row.names(aux1)<-aux1$ID
        pca <- prcomp(aux1[,4:6],  scale = T, center = T)
        km_clusters <- kmeans(pca$x,5)
        aux1$Cluster=as.factor(km_clusters$cluster)
        
        ggplot(aux1, aes(x=Cluster, y=Frecuencia, fill=Cluster)) +
            geom_boxplot(outlier.shape = NA)+
            geom_hline(yintercept = median(aux1$Recencia), color="darkgreen", linetype="dashed")+
            coord_flip()+
            theme_minimal()+
            labs(title="")+
            scale_y_continuous(limits = quantile(aux1$Frecuencia, c(0.1, 0.9)), labels = comma)
    })
    output$CompM <- renderPlot({
        aux1<-bd_rfm() %>% 
            mutate(ID=seq(1:nrow(bd_rfm()))) %>% 
            select(-Id_Cliente)
        aux1<-as.data.frame(sapply(aux1[, c(1:7)], as.numeric))
        row.names(aux1)<-aux1$ID
        pca <- prcomp(aux1[,4:6],  scale = T, center = T)
        km_clusters <- kmeans(pca$x,5)
        aux1$Cluster=as.factor(km_clusters$cluster)
        
        ggplot(aux1, aes(x=Cluster, y=Monto, fill=Cluster)) +
            geom_boxplot(outlier.shape = NA)+
            geom_hline(yintercept = median(aux1$Recencia), color="darkgreen", linetype="dashed")+
            coord_flip()+
            theme_minimal()+
            labs(title="")+
            scale_y_continuous(limits = quantile(aux1$Monto, c(0.1, 0.9)), labels = comma)
    })
    
    output$biplot <- renderPlot({
        aux1<-bd_rfm() %>% 
            mutate(ID=seq(1:nrow(bd_rfm()))) %>% 
            select(-Id_Cliente)
        aux1<-as.data.frame(sapply(aux1[, c(4:7)], as.numeric))
        row.names(aux1)<-aux1$ID
        pca <- prcomp(aux1[-4],  scale = T, center = T)

        km_clusters <- kmeans(pca$x,5)
        fviz_pca_biplot(pca, geom = "point", habillage =  as.factor(km_clusters$cluster), addEllipses=TRUE, ellipse.level=0.95, 
                        title = "", repel = T)
    })
    
    #Frecuencia
    output$TablaFrecuencia_s <- renderTable({
        t1<-bd_rfm() %>% group_by(`Score`=F_Score) %>%
            summarise(`Mínimo`=comma(round(min(Frecuencia))),
                      `Máximo`=comma(round(max(Frecuencia))),
                      Pacientes=comma(round(n_distinct(Id_Cliente))))
        
    }, spacing = 'xs')
    #Recencia
    output$TablaRecencia_s <- renderTable({
        t1<-bd_rfm() %>% group_by(`Score`=R_Score) %>%
            summarise(`Mínimo`=comma(round(min(Recencia))),
                      `Máximo`=comma(round(max(Recencia))),
                      Pacientes=comma(round(n_distinct(Id_Cliente)))) %>% 
            arrange(desc(Score))
        
    }, spacing = 'xs')
    #Monto
    output$TablaMonto_s <- renderTable({
        t1<-bd_rfm() %>% group_by(`Score`=M_Score) %>%
            summarise(`Mínimo`=dollar(round(min(Monto))),
                      `Máximo`=dollar(round(max(Monto))),
                      Pacientes=comma(round(n_distinct(Id_Cliente))))
    }, spacing = 'xs')
    
    ### Priorizacion ----
    
    output$TablaPato1 <- renderDataTable({
        t1<-data_f() %>% group_by(Patologia) %>%
            summarise(Monto=sum(Venta_Bruta, na.rm = T),
                      Pacientes=n_distinct(Id_Cliente),
                      IngresoPromedio=sum(Venta_Bruta, na.rm = T)/n_distinct(Id_Cliente))
        
        datatable(t1, options=list(pageLength = 20,dom = 't',scrollY = '250px',searching= FALSE), rownames=F, 
                  colnames = c("Patología","Monto", "Pacientes", "Promedio por Paciente")) %>% 
            formatCurrency(c("Monto", "IngresoPromedio"), digits = 0) %>% 
            formatRound("Pacientes", digits = 0)
        })
    output$TablaPato2 <- renderDataTable({
        for (i in 1:5){
            aux1<-within(bd_rfm(), R_Score <- relevel(R_Score, ref = i))
            assign(paste0("x",i),as.data.frame(lm(data = aux1, Monto~R_Score)$coefficients))
            d=get(paste0("x",i))
            colnames(d)<-paste0("R_Score",levels(aux1$R_Score)[1])
            assign(paste0("x",i),d)
        }
        
        x1$Score <- rownames(x1)
        x2$Score <- rownames(x2)
        x3$Score <- rownames(x3)
        x4$Score <- rownames(x4)
        x5$Score <- rownames(x5)
        
        aux2<-x1 %>% full_join(x2) %>% full_join(x3) %>% full_join(x4) %>% full_join(x5) %>% 
            filter(Score != "(Intercept)") %>% arrange(Score) %>% 
            gather(-Score, key="Score1", value="Betta") %>% select(c(2,1,3)) %>% 
            mutate(Betta=as.numeric(Betta),
                   Score=as.numeric(gsub("[^0-9]", "",Score)),
                   Score1=as.numeric(gsub("[^0-9]", "",Score1))
            ) %>% 
            filter(Score==Score1+1)
        
        t1<-bd_rfm() %>% group_by(R_Score) %>% 
            summarise(Pacientes=n_distinct(Id_Cliente)) %>% 
            arrange(R_Score) %>% 
            mutate(R_Score=as.numeric(R_Score)) %>% 
            left_join(aux2, by=c("R_Score"="Score1")) %>% 
            select(R_Score, Pacientes, Betta) %>% 
            mutate(Impacto=Pacientes*Betta,
                   Betta=Betta)
        
        datatable(t1, options=list(pageLength = 5,dom = 't',scrollY = '250px',searching= FALSE), rownames=F, 
                  colnames = c("Score Recencia","Pacientes", "Betta", "Impacto")) %>% 
            formatCurrency(c("Betta", "Impacto"), digits = 0) %>% 
            formatRound("Pacientes", digits = 0)
        
    })
    output$TablaPato3 <- renderDataTable({
        for (i in 1:5){
            aux1<-within(bd_rfm(), F_Score <- relevel(F_Score, ref = i))
            assign(paste0("x",i),as.data.frame(lm(data = aux1, Monto~F_Score)$coefficients))
            d=get(paste0("x",i))
            colnames(d)<-paste0("F_Score",levels(aux1$F_Score)[1])
            assign(paste0("x",i),d)
        }
        
        x1$Score <- rownames(x1)
        x2$Score <- rownames(x2)
        x3$Score <- rownames(x3)
        x4$Score <- rownames(x4)
        x5$Score <- rownames(x5)
        
        aux2<-x1 %>% full_join(x2) %>% full_join(x3) %>% full_join(x4) %>% full_join(x5) %>% 
            filter(Score != "(Intercept)") %>% arrange(Score) %>% 
            gather(-Score, key="Score1", value="Betta") %>% select(c(2,1,3)) %>% 
            mutate(Betta=as.numeric(Betta),
                   Score=as.numeric(gsub("[^0-9]", "",Score)),
                   Score1=as.numeric(gsub("[^0-9]", "",Score1))
            ) %>% 
            filter(Score==Score1+1)
        
        t1<-bd_rfm() %>% group_by(F_Score) %>% 
            summarise(Pacientes=n_distinct(Id_Cliente)) %>% 
            arrange(F_Score) %>% 
            mutate(F_Score=as.numeric(F_Score)) %>% 
            left_join(aux2, by=c("F_Score"="Score1")) %>% 
            select(F_Score, Pacientes, Betta) %>% 
            mutate(Impacto=Pacientes*Betta,
                   Betta=Betta)
        
        datatable(t1, options=list(pageLength = 5,dom = 't',scrollY = '250px',searching= FALSE), rownames=F, 
                  colnames = c("Score Frecuencia","Pacientes", "Betta", "Impacto")) %>% 
            formatCurrency(c("Betta", "Impacto"), digits = 0) %>% 
            formatRound("Pacientes", digits = 0)
        
    })
    
    #Frecuencia
    output$TablaFrecuencia_p <- renderTable({
        t1<-bd_rfm() %>% group_by(`Score`=F_Score) %>%
            summarise(`Mínimo`=comma(round(min(Frecuencia))),
                      `Máximo`=comma(round(max(Frecuencia))),
                      Pacientes=comma(round(n_distinct(Id_Cliente))))
        
    }, spacing = 'xs')
    #Recencia
    output$TablaRecencia_p <- renderTable({
        t1<-bd_rfm() %>% group_by(`Score`=R_Score) %>%
            summarise(`Mínimo`=comma(round(min(Recencia))),
                      `Máximo`=comma(round(max(Recencia))),
                      Pacientes=comma(round(n_distinct(Id_Cliente)))) %>% 
            arrange(desc(Score))
        
    }, spacing = 'xs')
    #Monto
    output$TablaMonto_p <- renderTable({
        t1<-bd_rfm() %>% group_by(`Score`=M_Score) %>%
            summarise(`Mínimo`=dollar(round(min(Monto))),
                      `Máximo`=dollar(round(max(Monto))),
                      Pacientes=comma(round(n_distinct(Id_Cliente))))
    }, spacing = 'xs')
    
    ### Estimación de Impactos ----
    
    #Recencia
    output$Modelo_R <- renderTable({
        for (i in 1:5){
            aux1<-within(bd_rfm(), R_Score <- relevel(R_Score, ref = i))
            assign(paste0("x",i),as.data.frame(comma(lm(data = aux1, Monto~R_Score)$coefficients)))
            d=get(paste0("x",i))
            colnames(d)<-paste0("R_Score",levels(aux1$R_Score)[1])
            assign(paste0("x",i),d)
        }
        
        x1$Score <- rownames(x1)
        x2$Score <- rownames(x2)
        x3$Score <- rownames(x3)
        x4$Score <- rownames(x4)
        x5$Score <- rownames(x5)
        
        R_MatCoeff <- Reduce(Merge_All, list(x1,x2,x3,x4,x5)) %>% filter(Score!="(Intercept)")
        R_MatCoeff <- select(R_MatCoeff,noquote(order(colnames(R_MatCoeff))))
        R_MatCoeff <- transpose(R_MatCoeff)
        colnames(R_MatCoeff)<-R_MatCoeff[6,]
        R_MatCoeff <- R_MatCoeff[-6,]
        R_MatCoeff$Score<-c("R_Score1", "R_Score2", "R_Score3", "R_Score4", "R_Score5")
        nvars<-dim(R_MatCoeff)[2]
        R_MatCoeff <- R_MatCoeff[1:nvars-1,c(nvars,1:nvars-1)]
        rm(x1,x2,x3,x4,x5,aux1)
        return(R_MatCoeff)
    }, spacing = 'xs', align = 'c', digits = 0)
    #Frecuencia
    output$Modelo_F <- renderTable({
        for (i in 1:5){
            aux1<-within(bd_rfm(), F_Score <- relevel(F_Score, ref = i))
            assign(paste0("x",i),as.data.frame(comma(lm(data = aux1, Monto~F_Score)$coefficients)))
            d=get(paste0("x",i))
            colnames(d)<-paste0("F_Score",levels(aux1$F_Score)[1])
            assign(paste0("x",i),d)
        }
        
        x1$Score <- rownames(x1)
        x2$Score <- rownames(x2)
        x3$Score <- rownames(x3)
        x4$Score <- rownames(x4)
        x5$Score <- rownames(x5)
        
        R_MatCoeff <- Reduce(Merge_All, list(x1,x2,x3,x4,x5)) %>% filter(Score!="(Intercept)")
        R_MatCoeff <- select(R_MatCoeff,noquote(order(colnames(R_MatCoeff))))
        R_MatCoeff <- transpose(R_MatCoeff)
        colnames(R_MatCoeff)<-R_MatCoeff[6,]
        R_MatCoeff <- R_MatCoeff[-6,]
        R_MatCoeff$Score<-c("F_Score1", "F_Score2", "F_Score3", "F_Score4", "F_Score5")
        nvars<-dim(R_MatCoeff)[2]
        R_MatCoeff <- R_MatCoeff[1:nvars-1,c(nvars,1:nvars-1)]
        rm(x1,x2,x3,x4,x5)
        return(R_MatCoeff)
    }, spacing = 'xs', align = 'c', digits = 0)
    
    #Frecuencia
    output$TablaFrecuencia_m <- renderTable({
        t1<-bd_rfm() %>% group_by(`Score`=F_Score) %>%
            summarise(`Mínimo`=comma(round(min(Frecuencia))),
                      `Máximo`=comma(round(max(Frecuencia))),
                      Pacientes=comma(round(n_distinct(Id_Cliente))))
        
    }, spacing = 'xs')
    #Recencia
    output$TablaRecencia_m <- renderTable({
        t1<-bd_rfm() %>% group_by(`Score`=R_Score) %>%
            summarise(`Mínimo`=comma(round(min(Recencia))),
                      `Máximo`=comma(round(max(Recencia))),
                      Pacientes=comma(round(n_distinct(Id_Cliente)))) %>% 
            arrange(desc(Score))
        
    }, spacing = 'xs')
    #Monto
    output$TablaMonto_m <- renderTable({
        t1<-bd_rfm() %>% group_by(`Score`=M_Score) %>%
            summarise(`Mínimo`=dollar(round(min(Monto))),
                      `Máximo`=dollar(round(max(Monto))),
                      Pacientes=comma(round(n_distinct(Id_Cliente))))
    }, spacing = 'xs')
    
    ### Tops Interactivos ----
    
    output$default <- renderText({
        print(dim(bd_rfm()))
    })
    
    output$TopDInamico <- renderDataTable({
    
    myCol1 <- paste0("desc(", input$VariableOrden,")")
    t1<-data_f() %>% group_by_(input$VariableTop) %>% 
        summarise(Pacientes=n_distinct(Id_Cliente),
                  VentaBruta=sum(Venta_Bruta, na.rm = T),
                  VentaNeta=sum(Venta_Neta, na.rm = T),
                  Frec=n(),
                  Cantidad=sum(Cantidad_Productos, na.rm = T),
                  UsoPromedio=Frec/Pacientes,
                  VentaBPromedio=VentaBruta/Frec,
                  VentaNPromedio=VentaNeta/Frec
                  ) %>% 
        arrange_(.dots = c(myCol1))
    
    if(input$VariableTop =="Patologia") nom="Patología"
    if(input$VariableTop =="Categoria") nom="Categoría"
    if(input$VariableTop =="nombre_Sucursal") nom="Sucursal"
    if(input$VariableTop =="prod_nombre") nom="Producto"
    if(input$VariableTop =="prod_proveedor_nombre") nom="Proveedor"
    if(input$VariableTop =="jerarquia1") nom="Jerarquía 1" 
    if(input$VariableTop =="jerarquia2") nom="Jerarquía 2"
    if(input$VariableTop =="principioconcatenado") nom="Principio Activo"
    if(input$VariableTop =="Id_Cliente") nom="Paciente"
   
    datatable(t1, options=list(pageLength =input$top,dom = 't', searching= FALSE), rownames=F, 
              colnames = c(nom,"Pacientes", "Venta Bruta","Venta Neta","Frecuencia de Compra","Unidades","Uso Promedio",
                           "Venta Bruta Promedio","Venta Neta Promedio")) %>% 
        formatCurrency(c("VentaBruta", "VentaNeta", "VentaBPromedio", "VentaNPromedio"), digits = 0) %>% 
        formatRound(c("Pacientes", "Cantidad", "Frec"), digits = 0) %>% 
        formatRound(c("UsoPromedio"), digits = 2)
    
    })
    
    ### Hoja de descargas ----
    
    #Frecuencia
    output$TablaFrecuencia_d <- renderTable({
        t1<-bd_rfm() %>% group_by(`Score`=F_Score) %>%
            summarise(`Mínimo`=comma(round(min(Frecuencia))),
                      `Máximo`=comma(round(max(Frecuencia))),
                      Pacientes=comma(round(n_distinct(Id_Cliente))))
        
    }, spacing = 'xs')
    #Recencia
    output$TablaRecencia_d <- renderTable({
        t1<-bd_rfm() %>% group_by(`Score`=R_Score) %>%
            summarise(`Mínimo`=comma(round(min(Recencia))),
                      `Máximo`=comma(round(max(Recencia))),
                      Pacientes=comma(round(n_distinct(Id_Cliente)))) %>% 
            arrange(desc(Score))
        
    }, spacing = 'xs')
    #Monto
    output$TablaMonto_d <- renderTable({
        t1<-bd_rfm() %>% group_by(`Score`=M_Score) %>%
            summarise(`Mínimo`=dollar(round(min(Monto))),
                      `Máximo`=dollar(round(max(Monto))),
                      Pacientes=comma(round(n_distinct(Id_Cliente))))
    }, spacing = 'xs')
    
    output$TablaDesc <- renderDataTable({
    
    cols=c("Id_Cliente", "autorizacion", "Categoria_Afiliado", input$Vars_Prod, "R_Score", "F_Score", "M_Score", "Cluster",
               "Venta_Bruta","Venta_Neta")
        
        datatable(Descarga1(),options=list(pageLength =input$top,dom = 't', searching= FALSE, scrollX = TRUE), rownames=F)

    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0("datos-", Sys.Date(), ".csv")
        },
        content = function(file) {
            write.csv(Descarga1(), file, row.names = FALSE, sep=";")
        }
    )
    
    ### Reglas de Asociación ----
    ### Referenciación Geográfica ----
    ### Descripción Demográfica ----
}
