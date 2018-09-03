<<<<<<< HEAD
﻿shinyServer(function(input, output, session) {
  
  # Función para cargar el archivo subido por el usuario.
  data<-reactive({
    req(input$file1)
    df <- fread(input$file1$datapath,
                header = input$header,
                sep = input$sep, 
                na.strings = c("", "NA", "#/NA"))
    return(df)
  })
  # Validacion del archivo
  error<-reactive({
    
    error=character()
    nom_esp<-c('IDCLIENTE','LIMITECUPO','FECHAAPERTURA','AMPARADA','SEGEMENTOPOBLACIONAL','SALARIO',
               'CIUDAD','GENERO','CIIU','ACTIVIDAD','ZONA','FECHANACIMIENTO')
    
    x=data()
    
    # Verificacion de nombres de Columnas
    cond1<-sum(toupper(names(x)) != nom_esp)>0
    if(cond1){  
      error=rbind(error,paste0("ERROR: La(s) variable(s): ",paste(names(x)[toupper(names(x)) != nom_esp],collapse = ","), 
                   " no coincide(n) con el nombre esperado","\n"))
    }
    
    # Verificación de Formatos de Fecha de apertura
    names(x)<-nom_esp
    cond2<-sum(!(IsDate(x$FECHAAPERTURA, date.format = input$dates)))>0
    if (cond2){
      error=rbind(error, paste0("ERROR: La fecha de apertura no coincide con el formato especificado","\n"))
    }
    
    # Verificación de Formatos de Fecha de nacimiento
    cond3<-sum(!(IsDate(x$FECHANACIMIENTO, date.format = input$dates)))>0
    if (cond3){
      error=rbind(error, paste0("ERROR: La fecha de nacimeinto no coincide con el formato especificado","\n"))
    }
    
    # Verificación de ausentes en la identificación
    cond4<-sum(is.na(x$IDCLIENTE))>0
    if (cond4){
      error=rbind(error, paste0("ERROR: Existen valores ausentes en la identificacion del cliente","\n"))
    }
    
    # Verificación de ausentes en el cupo
    cond5<-sum(is.na(x$LIMITECUPO))>0
    if (cond5){
      error=rbind(error, paste0("ERROR: Existen valores ausentes en el cupo","\n"))
    }
    
    # Verificación de amparo
    cond6<-sum(!(toupper(x$AMPARADA) %in% c("SI", "NO")))>0
    if (cond6){
      error=rbind(error, paste0("ERROR: Existen valores del amparo distintos a Si / No","\n"))
    }
    
    # Verificación de segmento
    cond7<-sum(!(toupper(x$SEGEMENTOPOBLACIONAL) %in% c("ALTO","BÁSICO","BASICO","JOVEN","MEDIO", NA)))>0
    if (cond7){
      error=rbind(error, paste0("ERROR: Existen valores del segmento no aceptables","\n"))
    }
    
    # Verificación de salario
    cond8<-sum(is.na(as.numeric(x$SALARIO)))>0
    if (cond8){
      error=rbind(error, paste0("ERROR: Los valores de salario no son interpretables como números o existen valores ausentes","\n"))
    }
    
    # Verificacion de género
    cond9<-sum(!(toupper(x$GENERO) %in% c("F","M","HOMBRE","MUJER", NA)))>0
    if (cond9){
      error=rbind(error, paste0("ERROR: Existen valores del género no aceptables","\n"))
    }
    
    # Verificación de actividad
    cond11<-sum(!(toupper(x$ACTIVIDAD) %in% c("PRESTACION DE SERVICIOS (EDUCACION, BANCA, COMUNICACIONES, SEGUROS, SALUD, ARTE Y CULTURA)"
                                         ,"INDUSTRIA","COMERCIO AL POR MAYOR Y AL POR MENOR","OTROS","CONSTRUCCION, DEMOLICIONES, TERRENOS, VIAS."
                                         ,"PUBLICO","TANSPORTE Y ALMACENAMIENTO","TRANSPORTE Y ALMACENAMIENTO", "TEMPORALES",
                                         "AGRICULTURA, CAZA, SILVICULTURA Y PESCA", "ELECTRICIDAD, GAS, AGUA Y EXPLOTACION DE MINAS", NA)))>0
    if (cond11){
      error=rbind(error, paste("ERROR: Existen valores de la actividad no aceptables","\n"))
    }
    
    # Verificación de zona
    cond11<-sum(!(toupper(x$ZONA) %in% c("MUNICIPIOS","ZONA CENTRO","ZONA CHAPINERO",
                                                           "ZONA NORTE","ZONA SUR", NA)))>0
    if (cond11){
      error=rbind(error, paste0("ERROR: Existen valores de la zona no aceptables","\n"))
    }
    
    if(length(error)==0){
      error=paste0("Calificando la base de datos de ", dim(x)[1], " registros..." )
    }
    
    return(error)
  })
  # Calificacion
  calif<-reactive({
    
    if (error()==paste0("Calificando la base de datos de ", dim(data())[1], " registros..." )){
      
      x=data()
      names(x)<-c('IDCLIENTE','LIMITECUPO','FECHAAPERTURA','AMPARADA','SEGEMENTOPOBLACIONAL','SALARIO',
                  'CIUDAD','GENERO','CIIU','ACTIVIDAD','ZONA','FECHANACIMIENTO')
      
      fecha<-as.Date(input$date)
      
      step1<-x %>%
        mutate(FECHAAPERTURA=as.Date(FECHAAPERTURA, "%d/%m/%Y"),
               FECHANACIMIENTO=as.Date(FECHANACIMIENTO, "%d/%m/%Y"),
               EDAD=floor(difftime(fecha,FECHANACIMIENTO, units = 'days')/360),
               EDADPLASTICO=difftime(fecha,FECHAAPERTURA, units = 'days'),
               CUPO_VI=LIMITECUPO/SALARIO,
               IDCLIENTE=as.character(IDCLIENTE),
               LIMITECUPO=as.numeric(LIMITECUPO),
               CIUDAD=as.factor(ifelse(grepl("BOGOTA",CIUDAD), "BOGOTA","OTROS")),
               AMPARADA=as.factor(ifelse(AMPARADA=="SI",2,1)),
               SEGEMENTOPOBLACIONAL=as.factor(
                 ifelse(SEGEMENTOPOBLACIONAL=="ALTO","Alto",
                        ifelse(SEGEMENTOPOBLACIONAL %in% c("BÁSICO","BASICO"),"Básico",
                               ifelse(SEGEMENTOPOBLACIONAL=="JOVEN","Joven","Medio")))),
               SALARIO=as.numeric(SALARIO),
               GENERO=as.factor(
                 ifelse(GENERO %in% c("F","MUJER"),"F",
                        ifelse(GENERO %in% c("M","HOMBRE"),"M",NA))),
               CIIU=as.factor(CIIU),
               ACTIVIDAD=toupper(ACTIVIDAD),
               ACTIVIDAD=as.factor(
                 ifelse(ACTIVIDAD %in% c("PRESTACION DE SERVICIOS (EDUCACION, BANCA, COMUNICACIONES, SEGUROS, SALUD, ARTE Y CULTURA)"),"Prestacion de Servicios (Educacion, Banca, Comunicaciones, Seguros, Salud, Arte y Cultura)",
                        ifelse(ACTIVIDAD %in% c("INDUSTRIA"),"Industria",
                               ifelse(ACTIVIDAD %in% c("COMERCIO AL POR MAYOR Y AL POR MENOR"),"Comercio al por Mayor y al por Menor",
                                      ifelse(ACTIVIDAD %in% c("OTROS"),"Otros",
                                             ifelse(ACTIVIDAD %in% c("CONSTRUCCION, DEMOLICIONES, TERRENOS, VIAS."),"Construccion, demoliciones, terrenos, Vias.",
                                                    ifelse(ACTIVIDAD %in% c("PUBLICO"),"Publico",
                                                           ifelse(ACTIVIDAD %in% c("TANSPORTE Y ALMACENAMIENTO", "TRANSPORTE Y ALMACENAMIENTO"),"Tansporte y Almacenamiento",
                                                                  ifelse(ACTIVIDAD %in% c("TEMPORALES"),"Temporales",
                                                                         ifelse(ACTIVIDAD %in% c("AGRICULTURA, CAZA, SILVICULTURA Y PESCA"),"Agricultura, Caza, Silvicultura y pesca",
                                                                                ifelse(ACTIVIDAD %in% c("ELECTRICIDAD, GAS, AGUA Y EXPLOTACION DE MINAS"),"Electricidad, Gas, Agua y explotacion de minas",NA))))))))))),
               ZONA=as.factor(ifelse(toupper(ZONA)=="MUNICIPIOS",1,
                                     ifelse(toupper(ZONA)=="ZONA CENTRO",2,
                                            ifelse(toupper(ZONA)=="ZONA CHAPINERO",3,
                                                   ifelse(toupper(ZONA)=="ZONA NORTE",4,
                                                          ifelse(toupper(ZONA)=="ZONA SUR",5,NA)))))),
               EDAD=as.numeric(EDAD),
               EDADPLASTICO=as.numeric(EDADPLASTICO)
        )%>%
        select(IDCLIENTE, LIMITECUPO, CIUDAD, AMPARADA, SEGEMENTOPOBLACIONAL, SALARIO, GENERO, CIIU, ACTIVIDAD, ZONA,
               EDAD, EDADPLASTICO, CUPO_VI)
      names(step1)<-c("id_cliente","limitecupo","ciudad","amparada","segmpoblacion","salario","genero","id_ciiu",
                      "actividad","zona","Edad","EdadPlastico","Cupo_VI")
      
      step2<-woe.binning.deploy(step1, bin)
      step2$p_pred <- predict(modelo1,step2,type="prob")[,2]
      step2$Colsubsidio <- ifelse(step2$p_pred>thresh,1,0)
      
      step3<-step2 %>%
        select(id_cliente:Cupo_VI, Colsubsidio, -id_ciiu, -ciudad) %>%
        mutate(amparada=as.factor(ifelse(is.na(amparada),1,as.numeric(amparada))),
               zona=as.factor(ifelse(zona=="",NA,zona)),
               zona=as.factor(ifelse(is.na(zona),"Ausente",zona)),
               segmpoblacion=as.factor(ifelse(is.na(segmpoblacion),"Básico",as.character(segmpoblacion))),
               genero=as.factor(ifelse(is.na(genero),"M",as.character(genero))),
               actividad=as.factor(ifelse(is.na(actividad),"PRESTACION DE SERVICIOS (EDUCACION, BANCA, COMUNICACIONES, SEGUROS, SALUD, ARTE Y CULTURA)",as.character(actividad))),
               salario=ifelse(is.na(salario),mean(salario, na.rm=T),salario),
               limitecupo=ifelse(is.na(limitecupo),mean(limitecupo, na.rm=T),limitecupo),
               Edad=ifelse(is.na( Edad),mean( Edad, na.rm=T), Edad),
               EdadPlastico=ifelse(is.na(EdadPlastico),mean(EdadPlastico, na.rm=T),EdadPlastico),
               Cupo_VI=ifelse(is.na(Cupo_VI),mean(Cupo_VI, na.rm=T),Cupo_VI)
        )
      
      test<-predict(modelo_2,step3,type="prob")
      test$Cat_2<-colnames(test[1:5])[apply(test[1:5],1,which.max)]
      test$UES<-ifelse(test$SUPERMERCADOS<0.8, test$Cat_2, "SUPERMERCADOS")
      step3$UES <- test$UES
      
      test<-predict(modelo_3,step3,type="prob")
      test$Ues_2<-colnames(test[1:17])[apply(test[1:17],1,which.max)]
      test$Categoria<-ifelse(test$VESTUARIO<0.1, test$Ues_2, "VESTUARIO")
      step3$Categoria <- test$Categoria
      
      step4 <- step3 %>%
        mutate(PrimeraCompra=as.factor(ifelse(Colsubsidio==1, as.character(UES), as.character(Categoria)))) %>%
        select(IdPersona=id_cliente, PrimeraCompra) %>%
        group_by(IdPersona) %>%
        arrange(IdPersona) %>%
        filter(row_number()==1)
      
    return(step4)
    rm(step1, step2, step3, test)
    }
    
  })
  calif2<-reactive({
    
    fecha<-as.Date(input$date)
    
    step1=data.frame(limitecupo=as.numeric(input$Cupo),
                     ciudad=as.factor(input$Ciudad),
                     amparada=as.factor(input$Amparada),
                     segmpoblacion=as.factor(input$Segmento),
                     salario=as.numeric(input$Salario),
                     genero=as.factor(input$Genero),
                     id_ciiu=as.factor(input$CIIU),
                     actividad=as.factor(input$Actividad),
                     zona=as.factor(input$Zona),
                     Edad=as.numeric(floor(difftime(fecha,as.Date(input$FechaNacimiento), units = 'days')/360)),
                     EdadPlastico=as.numeric(difftime(fecha,as.Date(input$FechaApertura), units = 'days')),
                     Cupo_VI=as.numeric(input$Cupo)/as.numeric(input$Salario)
                     )
    
    step2<-woe.binning.deploy(step1, bin)
    step2$p_pred <- predict(modelo1,step2,type="prob")[,2]
    step2$Colsubsidio <- ifelse(step2$p_pred>thresh,1,0)
    
    test<-predict(modelo_2,step2,type="prob")
    test$Cat_2<-colnames(test[1:5])[apply(test[1:5],1,which.max)]
    test$UES<-ifelse(test$SUPERMERCADOS<0.8, test$Cat_2, "SUPERMERCADOS")
    step2$UES <- test$UES
    
    test<-predict(modelo_3,step2,type="prob")
    test$Ues_2<-colnames(test[1:17])[apply(test[1:17],1,which.max)]
    test$Categoria<-ifelse(test$VESTUARIO<0.1, test$Ues_2, "VESTUARIO")
    step2$Categoria <- test$Categoria
    
    step4 <- step2 %>%
      mutate(PrimeraCompra=as.factor(ifelse(Colsubsidio==1, as.character(UES), as.character(Categoria)))) %>%
      select(PrimeraCompra)
    
    return(step4)

    })
  
  # Desición
  output$Nota <- renderText({
    error()
  })
  output$Preview <- renderDataTable({
    datatable(data(), options=list(dom="t",searching=F, scrollX = TRUE), rownames=F)
  })
  
  output$Calificada <- renderDataTable({
      datatable(calif(), options=list(dom="t",searching=F, scrollX = TRUE), rownames=F)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Descarga", ".csv", sep = "")
    },
    content = function(file) {
      fwrite(calif(), file, row.names = FALSE)
    }
  )
  
  # Calificacion Individual
  
  output$Pred <- renderText({ 
    paste0("Según las características del cliente, es más probable que haga su primera compra en: ", calif2()[1,1])
  })
  
     session$onSessionEnded(function() {
     stopApp()
     q("no")

  })
})
=======
﻿shinyServer(function(input, output, session) {
  
  # Función para cargar el archivo subido por el usuario.
  data<-reactive({
    req(input$file1)
    df <- fread(input$file1$datapath,
                header = input$header,
                sep = input$sep, 
                na.strings = c("", "NA", "#/NA"))
    return(df)
  })
  # Validacion del archivo
  error<-reactive({
    
    error=character()
    nom_esp<-c('IDCLIENTE','LIMITECUPO','FECHAAPERTURA','AMPARADA','SEGEMENTOPOBLACIONAL','SALARIO',
               'CIUDAD','GENERO','CIIU','ACTIVIDAD','ZONA','FECHANACIMIENTO')
    
    x=data()
    
    # Verificacion de nombres de Columnas
    cond1<-sum(toupper(names(x)) != nom_esp)>0
    if(cond1){  
      error=rbind(error,paste0("ERROR: La(s) variable(s): ",paste(names(x)[toupper(names(x)) != nom_esp],collapse = ","), 
                   " no coincide(n) con el nombre esperado","\n"))
    }
    
    # Verificación de Formatos de Fecha de apertura
    names(x)<-nom_esp
    cond2<-sum(!(IsDate(x$FECHAAPERTURA, date.format = input$dates)))>0
    if (cond2){
      error=rbind(error, paste0("ERROR: La fecha de apertura no coincide con el formato especificado","\n"))
    }
    
    # Verificación de Formatos de Fecha de nacimiento
    cond3<-sum(!(IsDate(x$FECHANACIMIENTO, date.format = input$dates)))>0
    if (cond3){
      error=rbind(error, paste0("ERROR: La fecha de nacimeinto no coincide con el formato especificado","\n"))
    }
    
    # Verificación de ausentes en la identificación
    cond4<-sum(is.na(x$IDCLIENTE))>0
    if (cond4){
      error=rbind(error, paste0("ERROR: Existen valores ausentes en la identificacion del cliente","\n"))
    }
    
    # Verificación de ausentes en el cupo
    cond5<-sum(is.na(x$LIMITECUPO))>0
    if (cond5){
      error=rbind(error, paste0("ERROR: Existen valores ausentes en el cupo","\n"))
    }
    
    # Verificación de amparo
    cond6<-sum(!(toupper(x$AMPARADA) %in% c("SI", "NO")))>0
    if (cond6){
      error=rbind(error, paste0("ERROR: Existen valores del amparo distintos a Si / No","\n"))
    }
    
    # Verificación de segmento
    cond7<-sum(!(toupper(x$SEGEMENTOPOBLACIONAL) %in% c("ALTO","BÁSICO","BASICO","JOVEN","MEDIO", NA)))>0
    if (cond7){
      error=rbind(error, paste0("ERROR: Existen valores del segmento no aceptables","\n"))
    }
    
    # Verificación de salario
    cond8<-sum(is.na(as.numeric(x$SALARIO)))>0
    if (cond8){
      error=rbind(error, paste0("ERROR: Los valores de salario no son interpretables como números o existen valores ausentes","\n"))
    }
    
    # Verificacion de género
    cond9<-sum(!(toupper(x$GENERO) %in% c("F","M","HOMBRE","MUJER", NA)))>0
    if (cond9){
      error=rbind(error, paste0("ERROR: Existen valores del género no aceptables","\n"))
    }
    
    # Verificación de actividad
    cond11<-sum(!(toupper(x$ACTIVIDAD) %in% c("PRESTACION DE SERVICIOS (EDUCACION, BANCA, COMUNICACIONES, SEGUROS, SALUD, ARTE Y CULTURA)"
                                         ,"INDUSTRIA","COMERCIO AL POR MAYOR Y AL POR MENOR","OTROS","CONSTRUCCION, DEMOLICIONES, TERRENOS, VIAS."
                                         ,"PUBLICO","TANSPORTE Y ALMACENAMIENTO","TRANSPORTE Y ALMACENAMIENTO", "TEMPORALES",
                                         "AGRICULTURA, CAZA, SILVICULTURA Y PESCA", "ELECTRICIDAD, GAS, AGUA Y EXPLOTACION DE MINAS", NA)))>0
    if (cond11){
      error=rbind(error, paste("ERROR: Existen valores de la actividad no aceptables","\n"))
    }
    
    # Verificación de zona
    cond11<-sum(!(toupper(x$ZONA) %in% c("MUNICIPIOS","ZONA CENTRO","ZONA CHAPINERO",
                                                           "ZONA NORTE","ZONA SUR", NA)))>0
    if (cond11){
      error=rbind(error, paste0("ERROR: Existen valores de la zona no aceptables","\n"))
    }
    
    if(length(error)==0){
      error=paste0("Calificando la base de datos de ", dim(x)[1], " registros..." )
    }
    
    return(error)
  })
  # Calificacion
  calif<-reactive({
    
    if (error()==paste0("Calificando la base de datos de ", dim(data())[1], " registros..." )){
      
      x=data()
      names(x)<-c('IDCLIENTE','LIMITECUPO','FECHAAPERTURA','AMPARADA','SEGEMENTOPOBLACIONAL','SALARIO',
                  'CIUDAD','GENERO','CIIU','ACTIVIDAD','ZONA','FECHANACIMIENTO')
      
      fecha<-as.Date(input$date)
      
      step1<-x %>%
        mutate(FECHAAPERTURA=as.Date(FECHAAPERTURA, "%d/%m/%Y"),
               FECHANACIMIENTO=as.Date(FECHANACIMIENTO, "%d/%m/%Y"),
               EDAD=floor(difftime(fecha,FECHANACIMIENTO, units = 'days')/360),
               EDADPLASTICO=difftime(fecha,FECHAAPERTURA, units = 'days'),
               CUPO_VI=LIMITECUPO/SALARIO,
               IDCLIENTE=as.character(IDCLIENTE),
               LIMITECUPO=as.numeric(LIMITECUPO),
               CIUDAD=as.factor(ifelse(grepl("BOGOTA",CIUDAD), "BOGOTA","OTROS")),
               AMPARADA=as.factor(ifelse(AMPARADA=="SI",2,1)),
               SEGEMENTOPOBLACIONAL=as.factor(
                 ifelse(SEGEMENTOPOBLACIONAL=="ALTO","Alto",
                        ifelse(SEGEMENTOPOBLACIONAL %in% c("BÁSICO","BASICO"),"Básico",
                               ifelse(SEGEMENTOPOBLACIONAL=="JOVEN","Joven","Medio")))),
               SALARIO=as.numeric(SALARIO),
               GENERO=as.factor(
                 ifelse(GENERO %in% c("F","MUJER"),"F",
                        ifelse(GENERO %in% c("M","HOMBRE"),"M",NA))),
               CIIU=as.factor(CIIU),
               ACTIVIDAD=toupper(ACTIVIDAD),
               ACTIVIDAD=as.factor(
                 ifelse(ACTIVIDAD %in% c("PRESTACION DE SERVICIOS (EDUCACION, BANCA, COMUNICACIONES, SEGUROS, SALUD, ARTE Y CULTURA)"),"Prestacion de Servicios (Educacion, Banca, Comunicaciones, Seguros, Salud, Arte y Cultura)",
                        ifelse(ACTIVIDAD %in% c("INDUSTRIA"),"Industria",
                               ifelse(ACTIVIDAD %in% c("COMERCIO AL POR MAYOR Y AL POR MENOR"),"Comercio al por Mayor y al por Menor",
                                      ifelse(ACTIVIDAD %in% c("OTROS"),"Otros",
                                             ifelse(ACTIVIDAD %in% c("CONSTRUCCION, DEMOLICIONES, TERRENOS, VIAS."),"Construccion, demoliciones, terrenos, Vias.",
                                                    ifelse(ACTIVIDAD %in% c("PUBLICO"),"Publico",
                                                           ifelse(ACTIVIDAD %in% c("TANSPORTE Y ALMACENAMIENTO", "TRANSPORTE Y ALMACENAMIENTO"),"Tansporte y Almacenamiento",
                                                                  ifelse(ACTIVIDAD %in% c("TEMPORALES"),"Temporales",
                                                                         ifelse(ACTIVIDAD %in% c("AGRICULTURA, CAZA, SILVICULTURA Y PESCA"),"Agricultura, Caza, Silvicultura y pesca",
                                                                                ifelse(ACTIVIDAD %in% c("ELECTRICIDAD, GAS, AGUA Y EXPLOTACION DE MINAS"),"Electricidad, Gas, Agua y explotacion de minas",NA))))))))))),
               ZONA=as.factor(ifelse(toupper(ZONA)=="MUNICIPIOS",1,
                                     ifelse(toupper(ZONA)=="ZONA CENTRO",2,
                                            ifelse(toupper(ZONA)=="ZONA CHAPINERO",3,
                                                   ifelse(toupper(ZONA)=="ZONA NORTE",4,
                                                          ifelse(toupper(ZONA)=="ZONA SUR",5,NA)))))),
               EDAD=as.numeric(EDAD),
               EDADPLASTICO=as.numeric(EDADPLASTICO)
        )%>%
        select(IDCLIENTE, LIMITECUPO, CIUDAD, AMPARADA, SEGEMENTOPOBLACIONAL, SALARIO, GENERO, CIIU, ACTIVIDAD, ZONA,
               EDAD, EDADPLASTICO, CUPO_VI)
      names(step1)<-c("id_cliente","limitecupo","ciudad","amparada","segmpoblacion","salario","genero","id_ciiu",
                      "actividad","zona","Edad","EdadPlastico","Cupo_VI")
      
      step2<-woe.binning.deploy(step1, bin)
      step2$p_pred <- predict(modelo1,step2,type="prob")[,2]
      step2$Colsubsidio <- ifelse(step2$p_pred>thresh,1,0)
      
      step3<-step2 %>%
        select(id_cliente:Cupo_VI, Colsubsidio, -id_ciiu, -ciudad) %>%
        mutate(amparada=as.factor(ifelse(is.na(amparada),1,as.numeric(amparada))),
               zona=as.factor(ifelse(zona=="",NA,zona)),
               zona=as.factor(ifelse(is.na(zona),"Ausente",zona)),
               segmpoblacion=as.factor(ifelse(is.na(segmpoblacion),"Básico",as.character(segmpoblacion))),
               genero=as.factor(ifelse(is.na(genero),"M",as.character(genero))),
               actividad=as.factor(ifelse(is.na(actividad),"PRESTACION DE SERVICIOS (EDUCACION, BANCA, COMUNICACIONES, SEGUROS, SALUD, ARTE Y CULTURA)",as.character(actividad))),
               salario=ifelse(is.na(salario),mean(salario, na.rm=T),salario),
               limitecupo=ifelse(is.na(limitecupo),mean(limitecupo, na.rm=T),limitecupo),
               Edad=ifelse(is.na( Edad),mean( Edad, na.rm=T), Edad),
               EdadPlastico=ifelse(is.na(EdadPlastico),mean(EdadPlastico, na.rm=T),EdadPlastico),
               Cupo_VI=ifelse(is.na(Cupo_VI),mean(Cupo_VI, na.rm=T),Cupo_VI)
        )
      
      test<-predict(modelo_2,step3,type="prob")
      test$Cat_2<-colnames(test[1:5])[apply(test[1:5],1,which.max)]
      test$UES<-ifelse(test$SUPERMERCADOS<0.8, test$Cat_2, "SUPERMERCADOS")
      step3$UES <- test$UES
      
      test<-predict(modelo_3,step3,type="prob")
      test$Ues_2<-colnames(test[1:17])[apply(test[1:17],1,which.max)]
      test$Categoria<-ifelse(test$VESTUARIO<0.1, test$Ues_2, "VESTUARIO")
      step3$Categoria <- test$Categoria
      
      step4 <- step3 %>%
        mutate(PrimeraCompra=as.factor(ifelse(Colsubsidio==1, as.character(UES), as.character(Categoria)))) %>%
        select(IdPersona=id_cliente, PrimeraCompra) %>%
        group_by(IdPersona) %>%
        arrange(IdPersona) %>%
        filter(row_number()==1)
      
    return(step4)
    rm(step1, step2, step3, test)
    }
    
  })
  calif2<-reactive({
    
    fecha<-as.Date(input$date)
    
    step1=data.frame(limitecupo=as.numeric(input$Cupo),
                     ciudad=as.factor(input$Ciudad),
                     amparada=as.factor(input$Amparada),
                     segmpoblacion=as.factor(input$Segmento),
                     salario=as.numeric(input$Salario),
                     genero=as.factor(input$Genero),
                     id_ciiu=as.factor(input$CIIU),
                     actividad=as.factor(input$Actividad),
                     zona=as.factor(input$Zona),
                     Edad=as.numeric(floor(difftime(fecha,as.Date(input$FechaNacimiento), units = 'days')/360)),
                     EdadPlastico=as.numeric(difftime(fecha,as.Date(input$FechaApertura), units = 'days')),
                     Cupo_VI=as.numeric(input$Cupo)/as.numeric(input$Salario)
                     )
    
    step2<-woe.binning.deploy(step1, bin)
    step2$p_pred <- predict(modelo1,step2,type="prob")[,2]
    step2$Colsubsidio <- ifelse(step2$p_pred>thresh,1,0)
    
    test<-predict(modelo_2,step2,type="prob")
    test$Cat_2<-colnames(test[1:5])[apply(test[1:5],1,which.max)]
    test$UES<-ifelse(test$SUPERMERCADOS<0.8, test$Cat_2, "SUPERMERCADOS")
    step2$UES <- test$UES
    
    test<-predict(modelo_3,step2,type="prob")
    test$Ues_2<-colnames(test[1:17])[apply(test[1:17],1,which.max)]
    test$Categoria<-ifelse(test$VESTUARIO<0.1, test$Ues_2, "VESTUARIO")
    step2$Categoria <- test$Categoria
    
    step4 <- step2 %>%
      mutate(PrimeraCompra=as.factor(ifelse(Colsubsidio==1, as.character(UES), as.character(Categoria)))) %>%
      select(PrimeraCompra)
    
    return(step4)

    })
  
  # Desición
  output$Nota <- renderText({
    error()
  })
  output$Preview <- renderDataTable({
    datatable(data(), options=list(dom="t",searching=F, scrollX = TRUE), rownames=F)
  })
  
  output$Calificada <- renderDataTable({
      datatable(calif(), options=list(dom="t",searching=F, scrollX = TRUE), rownames=F)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Descarga", ".csv", sep = "")
    },
    content = function(file) {
      fwrite(calif(), file, row.names = FALSE)
    }
  )
  
  # Calificacion Individual
  
  output$Pred <- renderText({ 
    paste0("Según las características del cliente, es más probable que haga su primera compra en: ", calif2()[1,1])
  })
  
     session$onSessionEnded(function() {
     stopApp()
     q("no")

  })
})
>>>>>>> 5308310d9fd680f7f63a4eff58a3d19ee6c67bee
