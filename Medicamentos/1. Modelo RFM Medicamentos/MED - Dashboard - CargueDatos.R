source("C:/Users/hernyatt/Documents/CamiloYate/Funciones.r")
### Carga e instalacion de paquetes ====
pkgs <- c("data.table", "dplyr", "RODBC", "stringr", "lubridate")
Loadpkg(pkgs)

miss<-c("NULL","","NA","09-999999999999999", "30-99999999", "#NA", "N/A", "09-0", 
        "NO REGISTRA", "999999999999999", "99999999")

#### Medicamentos ----

## Medicamentos 

listado<-fread("BD/Depuradas/ListadMed.csv", na.strings = miss)

#### Personas ----

persona<-fread("BD/Depuradas/Persona.csv", na.strings = miss) %>% 
    rename(Categoria_Afiliado=categoria)

### Punto de venta ----

#### Transacciones ----

temp <- list.files("./BD/Depuradas/POS", pattern = "+.txt", recursive = F)
temp
prim<-14 ## Cambiar segun listado

MED<-fread(paste("./BD/Depuradas/POS/",temp[prim],sep=''), na.strings = miss)[ ,1:19] %>% 
    filter(!is.na(Id_Cliente), !is.na(prod_id)) %>% 
    mutate(Fecha=as.Date(as.character(Fecha), "%Y%m%d"),
           Venta_Bruta=as.numeric(gsub("\\D","",Venta_Bruta)),
           Venta_Neta=as.numeric(gsub("\\D","",Venta_Neta)),
           Cantidad_Productos=as.numeric(gsub("\\D","",Cantidad_Productos)),
           PertencetuClub=as.character(PertencetuClub),
           Id_Cliente=ifelse(grepl("-", Id_Cliente),Id_Cliente, paste0("09-",Id_Cliente)),
           NumIdcliente=as.numeric(gsub(",",".",str_split(Id_Cliente, "-", simplify = T)[,2]))
           ) %>% 
    select(-c(Afiliado, Segmento, PertencetuClub)) %>% 
    filter(!is.na(NumIdcliente))

for (i in prim+1:length(temp) ){
    tmp<-fread(paste("./BD/Depuradas/POS/",temp[i],sep=''), na.strings = miss)[ ,1:19] %>% 
        filter(!is.na(Id_Cliente), !is.na(Fecha)) %>% 
        mutate(Fecha=as.Date(as.character(Fecha), "%Y%m%d"),
               Venta_Bruta=as.numeric(gsub("\\D","",Venta_Bruta)),
               Venta_Neta=as.numeric(gsub("\\D","",Venta_Neta)),
               Cantidad_Productos=as.numeric(gsub("\\D","",Cantidad_Productos)),
               Id_Cliente=ifelse(grepl("-", Id_Cliente),Id_Cliente, paste0("09-",Id_Cliente)),
               PertencetuClub=as.character(PertencetuClub),
               NumIdcliente=as.numeric(gsub(",",".",str_split(Id_Cliente, "-", simplify = T)[,2]))) %>% 
        select(-c(Afiliado, Segmento, PertencetuClub))%>% 
        filter(!is.na(NumIdcliente))
    
    names(tmp)<-names(MED)
    
    MED<-rbind(MED,tmp)
    rm(tmp)
}
rm(temp)
table(year(MED$Fecha)*100+month(MED$Fecha))

# fwrite(MED, "C:/Users/hernyatt/Desktop/MED.csv", sep="|")

#### Union Total ----

BD <- MED %>% 
    left_join(persona, by=c("NumIdcliente"="idpersno")) %>% 
    left_join(listado, by=c("prod_id"="plu")) %>% 
    mutate(tipodetratamiento=ifelse(is.na(tipodetratamiento),"SIN CLASIFICACION",tipodetratamiento),
           autorizacion=ifelse(is.na(autorizacion),"NO",trimws(toupper(as.character(autorizacion)))),
           Patologia=ifelse(patologia_list %in% c("#N/A","|",NA), Patologia, patologia_list),
           Categoria=ifelse(categoria_list %in% c("#N/A","|",NA), Categoria, categoria_list),
           Patologia=ifelse(Patologia %in% c("#N/A","|",NA, "0"), "SIN PATOLOGIA", Patologia),
           Patologia=ifelse(grepl("-", Patologia),substr(Patologia, 5, length(Patologia) ),Patologia),
           Categoria=ifelse(grepl("-", Categoria),substr(Categoria, 5, length(Categoria) ),Categoria)
    ) %>% 
    select(-patologia_list, -categoria_list)

# ttest<-BD %>% 
#     group_by(Periodo=year(BD$Fecha)*100+month(BD$Fecha), Patologia, Categoria) %>% 
#     summarise(VentaBruta=sum(Venta_Bruta),
#               VentaNeta=sum(Venta_Neta))
# fwrite(ttest, "C:/Users/hernyatt/Desktop/Consulta_OSCAR.csv", sep="|")

# BD <- readRDS("D:/CamiloYate/2018/Medicamentos/4. Dashboard/BD/Depuradas/BD.rds")

table(BD$tipodetratamiento)
table(BD$autorizacion)
table(BD$Patologia)
table(BD$Categoria)

saveRDS(BD, file = "Protocolo/APP/Data/BD.rds")
fwrite(BD, "BD/Entregas/Depuraciones/DataMed.csv")


### Exportacion de listados ----

# Productos sin coincidencia en el listado
Ausentes <- BD %>% 
    filter(tipodetratamiento=="SIN CLASIFICACION" | Patologia=="SIN PATOLOGIA") %>% 
    select(prod_id, prod_nombre, tipodetratamiento, Patologia, Categoria) %>% unique()
fwrite(Ausentes, "./BD/Entregas/Depuraciones/ProdId_SinCoincidencia.csv")

### Eliminacionnde tablas auxiliares----
rm(MED, persona, i, miss, pkgs, prim, Ausentes, listado)