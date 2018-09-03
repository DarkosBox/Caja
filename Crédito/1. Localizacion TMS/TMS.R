source("D:/CamiloYate/Funciones.r")

pkgs<-c("data.table", "lubridate", "dplyr", "ggmap", "ggplot2", "scales")
Loadpkg(pkgs)

Consumo<-fread("BD/Consumos.txt", sep=";", dec=",", na.strings = "#N/A")
colnames(Consumo)<-c("Persona","Tarjeta","NIT_Establecimiento","Cod_Establecimiento",
                     "Nom_Establecimiento","Fecha","Valor","Anno")

Convenios<-fread("BD/Convenios.csv") %>% select(-NIT)
str(Convenios)

Geo_Persona<-fread("BD/Persona_Geo.csv", sep=";", dec=",", na.strings = "#N/A") %>% 
  select(-c(Barrio,NivelSocioRes,Cod_Poblado,Nom_Mun,Nom_Dep,IdPersona_Emp_Seg_Empresarial))
names(Geo_Persona)<-c("IdPersona","V_x","V_y")

empresa<-fread("BD/GeoEmpresas.txt", sep=";", dec=",", na.strings = "#N/A") %>% 
    filter(!is.na(CX)) %>% 
    select(IdPersona, CX, CY) %>% 
    rename(EX=CX, EY=CY)

BD_V <- Consumo %>% 
    group_by(NIT_Establecimiento, Persona, Cod_Establecimiento, Nom_Establecimiento) %>% 
    summarise(Consumo=sum(Valor)) %>% 
    left_join(Geo_Persona, by=c("Persona"="IdPersona")) %>% 
    left_join(Convenios, by=c("Cod_Establecimiento"="Cod")) %>% 
    left_join(empresa, by=c("Persona"="IdPersona"))

rm(Consumo, Convenios, Geo_Persona, empresa)

SumPersona<-BD_V %>% ungroup() %>% select(Persona, V_x, V_y) %>% unique()
SumEmpresa<-BD_V %>% ungroup() %>% select(Persona, EX, EY) %>% unique()

SumConvenio<-BD_V %>% ungroup() %>% 
    group_by(NIT_Establecimiento,Cod_Establecimiento,CX,CY) %>% 
    summarise(Consumo=sum(Consumo)) %>% 
    mutate(Colsubsidio=as.factor(ifelse(NIT_Establecimiento==8600073361,"Colsubsidio","Otro")))

SumConvenio$Colsubsidio=ifelse(is.na(SumConvenio$NIT_Establecimiento),"Otro",SumConvenio$Colsubsidio)

TopConv<-BD_V %>%  group_by(Nom_Establecimiento) %>% 
    summarise(Consumo=sum(Consumo),
              'Número de Clientes'=n_distinct(Persona),
              Frecuencia=n(),
              'Consumo Promedio'=sum(Consumo)/n_distinct(Persona)
              ) %>% 
    rename('Convenio'=Nom_Establecimiento) %>% 
    arrange(desc(Consumo))

mapa<-get_map(location = "bogota", maptype = "toner", zoom = 11)

#Donde Viven

ggmap(mapa, maprange=F, extent="device")+
    geom_density2d(data = SumPersona, aes(x=V_x, y=V_y), size=0.5) +
    stat_density2d(data = SumPersona,
                   aes(x = V_x, y = V_y, fill = ..level.., alpha = ..level..), size = 0.5,
                   bins = 16, geom = "polygon")+
    scale_fill_gradient("Densidad Poblacional",low = "green", high = "red") +
    scale_alpha(range = c(0, 0.3), guide = FALSE) +
    geom_point(data=SumConvenio, aes(x=CX, y=CY,colour =Colsubsidio, size=Consumo),alpha=0.4)+
    scale_colour_manual("Convenio",values=c("red", "blue"))+
    scale_size("Consumo", range = c(1, 15), labels=comma)

#Donde Trabajan

mt<-ggmap(mapa, maprange=FALSE ,extent = 'device')+
    geom_density2d(data = SumEmpresa, aes(x=EX, y=EY), size=0.5) +
    stat_density2d(data = SumEmpresa,
                   aes(x = EX, y = EY, fill = ..level.., alpha = ..level..), size = 0.5,
                   bins = 16, geom = "polygon")+
    scale_fill_gradient("Densidad Poblacional",low = "green", high = "red") +
    scale_alpha(range = c(0.3, 0.7), guide = FALSE) +
    geom_point(data=SumConvenio, aes(x=CX, y=CY,colour =Colsubsidio, size=Consumo),alpha=0.4)+
    scale_colour_manual("Convenio",values=c("firebrick", "navy"))+
    scale_size("Consumo", range = c(1, 15), labels=comma)


install.packages("DT")
library(plotly)
ggplotly(mt)

m = cbind(matrix(rnorm(120, 1e5, 1e6), 40), runif(40), rnorm(40, 100))
colnames(m) = head(LETTERS, ncol(m))

datatable(TopConv, 
          options = list(pageLength = 1579,dom = 't',scrollY = '100px',
                         searching= FALSE)
          ) %>% 
    formatCurrency(c('Consumo', 'Consumo Promedio'), digits = 0) %>% 
    formatCurrency(c('Número de Clientes', 'Frecuencia'), digits = 0, currency = "")


table(SumConvenio$Colsubsidio)

=======
source("D:/CamiloYate/Funciones.r")

pkgs<-c("data.table", "lubridate", "dplyr", "ggmap", "ggplot2", "scales")
Loadpkg(pkgs)

Consumo<-fread("BD/Consumos.txt", sep=";", dec=",", na.strings = "#N/A")
colnames(Consumo)<-c("Persona","Tarjeta","NIT_Establecimiento","Cod_Establecimiento",
                     "Nom_Establecimiento","Fecha","Valor","Anno")

Convenios<-fread("BD/Convenios.csv") %>% select(-NIT)
str(Convenios)

Geo_Persona<-fread("BD/Persona_Geo.csv", sep=";", dec=",", na.strings = "#N/A") %>% 
  select(-c(Barrio,NivelSocioRes,Cod_Poblado,Nom_Mun,Nom_Dep,IdPersona_Emp_Seg_Empresarial))
names(Geo_Persona)<-c("IdPersona","V_x","V_y")

empresa<-fread("BD/GeoEmpresas.txt", sep=";", dec=",", na.strings = "#N/A") %>% 
    filter(!is.na(CX)) %>% 
    select(IdPersona, CX, CY) %>% 
    rename(EX=CX, EY=CY)

BD_V <- Consumo %>% 
    group_by(NIT_Establecimiento, Persona, Cod_Establecimiento, Nom_Establecimiento) %>% 
    summarise(Consumo=sum(Valor)) %>% 
    left_join(Geo_Persona, by=c("Persona"="IdPersona")) %>% 
    left_join(Convenios, by=c("Cod_Establecimiento"="Cod")) %>% 
    left_join(empresa, by=c("Persona"="IdPersona"))

rm(Consumo, Convenios, Geo_Persona, empresa)

SumPersona<-BD_V %>% ungroup() %>% select(Persona, V_x, V_y) %>% unique()
SumEmpresa<-BD_V %>% ungroup() %>% select(Persona, EX, EY) %>% unique()

SumConvenio<-BD_V %>% ungroup() %>% 
    group_by(NIT_Establecimiento,Cod_Establecimiento,CX,CY) %>% 
    summarise(Consumo=sum(Consumo)) %>% 
    mutate(Colsubsidio=as.factor(ifelse(NIT_Establecimiento==8600073361,"Colsubsidio","Otro")))

SumConvenio$Colsubsidio=ifelse(is.na(SumConvenio$NIT_Establecimiento),"Otro",SumConvenio$Colsubsidio)

TopConv<-BD_V %>%  group_by(Nom_Establecimiento) %>% 
    summarise(Consumo=sum(Consumo),
              'Número de Clientes'=n_distinct(Persona),
              Frecuencia=n(),
              'Consumo Promedio'=sum(Consumo)/n_distinct(Persona)
              ) %>% 
    rename('Convenio'=Nom_Establecimiento) %>% 
    arrange(desc(Consumo))

mapa<-get_map(location = "bogota", maptype = "toner", zoom = 11)

#Donde Viven

ggmap(mapa, maprange=F, extent="device")+
    geom_density2d(data = SumPersona, aes(x=V_x, y=V_y), size=0.5) +
    stat_density2d(data = SumPersona,
                   aes(x = V_x, y = V_y, fill = ..level.., alpha = ..level..), size = 0.5,
                   bins = 16, geom = "polygon")+
    scale_fill_gradient("Densidad Poblacional",low = "green", high = "red") +
    scale_alpha(range = c(0, 0.3), guide = FALSE) +
    geom_point(data=SumConvenio, aes(x=CX, y=CY,colour =Colsubsidio, size=Consumo),alpha=0.4)+
    scale_colour_manual("Convenio",values=c("red", "blue"))+
    scale_size("Consumo", range = c(1, 15), labels=comma)

#Donde Trabajan

mt<-ggmap(mapa, maprange=FALSE ,extent = 'device')+
    geom_density2d(data = SumEmpresa, aes(x=EX, y=EY), size=0.5) +
    stat_density2d(data = SumEmpresa,
                   aes(x = EX, y = EY, fill = ..level.., alpha = ..level..), size = 0.5,
                   bins = 16, geom = "polygon")+
    scale_fill_gradient("Densidad Poblacional",low = "green", high = "red") +
    scale_alpha(range = c(0.3, 0.7), guide = FALSE) +
    geom_point(data=SumConvenio, aes(x=CX, y=CY,colour =Colsubsidio, size=Consumo),alpha=0.4)+
    scale_colour_manual("Convenio",values=c("firebrick", "navy"))+
    scale_size("Consumo", range = c(1, 15), labels=comma)


install.packages("DT")
library(plotly)
ggplotly(mt)

m = cbind(matrix(rnorm(120, 1e5, 1e6), 40), runif(40), rnorm(40, 100))
colnames(m) = head(LETTERS, ncol(m))

datatable(TopConv, 
          options = list(pageLength = 1579,dom = 't',scrollY = '100px',
                         searching= FALSE)
          ) %>% 
    formatCurrency(c('Consumo', 'Consumo Promedio'), digits = 0) %>% 
    formatCurrency(c('Número de Clientes', 'Frecuencia'), digits = 0, currency = "")


table(SumConvenio$Colsubsidio)

>>>>>>> 5308310d9fd680f7f63a4eff58a3d19ee6c67bee
