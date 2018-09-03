source("./Protocolo/Funciones.r")
### Carga e instalacion de paquetes ====
pkgs <- c("data.table", "dplyr", "RODBC", "stringr", "lubridate")
Loadpkg(pkgs)

miss<-c("NULL","","NA","09-999999999999999", "30-99999999", "#NA", "N/A", "09-0")

#### Listados ----

# Transacciones 

temp <- list.files("./BD/Depuradas/POS", pattern = "+.txt", recursive = F)
prim<-1

MED<-fread(paste("./BD/Depuradas/POS/",temp[prim],sep=''), na.strings = miss)[ ,1:19] %>% 
    filter(!is.na(prod_id)) %>%
    select(prod_id, prod_nombre, Patologia, Categoria) %>% 
    unique()


for (i in prim+1:length(temp) ){
    print(temp[i])
    tmp<-fread(paste("./BD/Depuradas/POS/",temp[i],sep=''), na.strings = miss)[ ,1:19] %>% 
        filter(!is.na(prod_id)) %>%
        select(prod_id, prod_nombre, Patologia, Categoria)
    names(tmp)<-names(MED)
    
    MED<-rbind(MED,tmp) %>% unique()
    rm(tmp)
}

## Duplicados en la base -

MED %>% 
    select(prod_id, prod_nombre, Categoria, Patologia) %>% 
    unique() %>% 
    group_by(prod_id) %>% 
    mutate(Freq=n()) %>% 
    filter(Freq>1) %>% 
    select(-Freq) %>% 
    fwrite("./BD/Entregas/Depuraciones/Duplicados_Transaccionales.csv")

## Medicamentos 

# Listado
vade<-fread("./BD/Depuradas/Vademecum.csv", na.strings =miss) %>% 
    select(-c(CATEGORIA, `DESCRIPCION JERARQUIA 1`,`DESCRIPCION JERARQUIA 2`))
names(vade)<-Limpiar.Cadenas(names(vade), espacios = F)
vade<-vade %>% mutate(precioreg=ifelse(precioreg==0,NA,precioreg),
                      tipodetratamiento=ifelse(is.na(tipodetratamiento),"NO APLICA",tipodetratamiento))


# Patologia y Categoria
pato_categ<-fread("./BD/Depuradas/Pato_Categ.csv", na.strings =miss)
names(pato_categ)<-Limpiar.Cadenas(names(pato_categ), espacios = F)


#### Unión ----

Listado_PLU<- MED %>% 
    select(prod_id, prod_nombre, Categoria, Patologia) %>% 
    unique() %>% 
    left_join(vade, by=c("prod_id"="plu")) %>% 
    left_join(pato_categ, by=c("prod_id"="plu")) %>% 
    select(1:4, principioconcatenado, tipodetratamiento, categoria,patologia) %>% 
    mutate(PatologiaDef=ifelse(!is.na(patologia), patologia, Patologia),
           CategoriaDef=ifelse(!is.na(categoria), categoria, Categoria))

## Duplicados en la base -

Listado_PLU %>% 
    select(prod_id, prod_nombre, PatologiaDef, CategoriaDef) %>%
    unique() %>% 
    group_by(prod_id) %>% 
    mutate(Freq=n()) %>% 
    filter(Freq>1) %>% 
    select(-Freq) %>% 
    fwrite("./BD/Entregas/Depuraciones/Duplicados_Cruce.csv")

## Ausentes -

Listado_PLU %>% 
    select(prod_id, prod_nombre, tipodetratamiento, categoria, patologia) %>% 
    filter(!is.na(tipodetratamiento) |!is.na(categoria) | !is.na(patologia)) %>% 
    fwrite("./BD/Entregas/Depuraciones/Ausentes.csv")


