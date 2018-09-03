source("./Protocolo/Funciones.r")
### Carga e instalacion de paquetes ====
pkgs <- c("data.table", "dplyr", "RODBC", "stringr", "lubridate", "stringi")
Loadpkg(pkgs)

miss<-c("NULL","","NA","09-999999999999999", "30-99999999", "#NA", "N/A", "09-0")

temp <- list.files("./BD/Depuradas/POS", pattern = "+.txt", recursive = F)
prim<-12

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

lista_pro_id=c(1216226,1215564,1083271,1250139,1291308,1154811,932554,1105344,998119,998108,421565,
               685312,1148593,330113,1216230,1291296,1105347,1153621,1215476,1289494,834480,679949,
               1215546,1215541,1188983,685313,660996,599987,1259017,1216213,658818,1216179,1035576,
               1289496,973290,1280026,976395,1263509,1148594,1263510,481788,599988) 


# Articulos que se vedieron en la rimera campaña 20 a 29 de abril

Formulas<-MED %>% 
    mutate(cond1=ifelse(Categoria=="FORMULAS INFANTILES",1,0),
           cond2=ifelse(prod_id %in% lista_pro_id,1,0),
           cond3=ifelse(stri_detect_fixed(toupper(prod_nombre), "PANAL"),1,0)
           ) %>% 
    filter(cond1==1 | cond2==1 | cond3==1)

fwrite(Formulas, "~/FormulasInfantiles.csv", sep=";")



