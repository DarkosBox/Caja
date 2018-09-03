source("C:/Users/hernyatt/Documents/CamiloYate/Funciones.r")
### Carga e instalacion de paquetes ====
pkgs <- c("data.table", "dplyr", "stringr", "tidyr")
Loadpkg(pkgs)

#### Cargue de Listados de Medicamentos----

listado<-fread("BD/Depuradas/ListadMed.csv", na.strings = c("","NO REGISTRA" )) %>% 
    mutate(patologia_list=ifelse(grepl("-", patologia_list),substr(patologia_list, 5, length(patologia_list) ),patologia_list),
           categoria_list=ifelse(grepl("-", categoria_list),substr(categoria_list, 5, length(categoria_list) ),categoria_list)) %>% 
    unique()

### Nuevas entregas ----

#Ausentes
ausentes<-fread("BD/Originales/Listados/Ausentes_Completado.csv", na.strings = c("","NO REGISTRA")) %>% 
    mutate(patologia_list=ifelse(grepl("-", patologia),substr(patologia, 5, length(patologia) ),patologia),
           categoria_list=ifelse(grepl("-", categoria),substr(categoria, 5, length(categoria) ),categoria),
           principioconcatenado=NA) %>% 
    select(plu=prod_id, principioconcatenado, tipodetratamiento, categoria_list, patologia_list) %>% unique()

#Duplicados
duplicados1<-fread("./BD/Originales/Listados/Duplicados_Cruce_Completado.csv", na.strings = c("","NO REGISTRA")) %>% 
    select(-prod_nombre) %>%
    mutate(PatologiaDef=ifelse(grepl("-", PatologiaDef),substr(PatologiaDef, 5, length(PatologiaDef) ),PatologiaDef),
           CategoriaDef=ifelse(grepl("-", CategoriaDef),substr(CategoriaDef, 5, length(CategoriaDef) ),CategoriaDef)) %>% 
    unique()
names(duplicados1)=c("plu", "pato1", "cate1")

duplicados2<-fread("./BD/Originales/Listados/Duplicados_Transaccionales_Completado.csv", na.strings = c("NO REGISTRA")) %>% 
    select(-prod_nombre) %>% 
    mutate(Patologia=ifelse(grepl("-", Patologia),substr(Patologia, 5, length(Patologia) ),Patologia),
           Patologia=ifelse(Patologia=="0", NA, Patologia),
           Categoria=ifelse(grepl("-", Categoria),substr(Categoria, 5, length(Categoria) ),Categoria)) %>% 
    unique()
names(duplicados2)=c("plu", "cate2", "pato2")

duplicados=duplicados1 %>% full_join(duplicados2) %>%
    mutate(cate=ifelse(is.na(cate1),cate2, ifelse(is.na(cate2),cate1, ifelse(cate1==cate2, cate1, cate1))),
           pato=ifelse(is.na(pato1),pato2, ifelse(is.na(pato2),cate1, ifelse(pato1==pato2, pato1, pato1)))
           ) %>% 
    select(plu, cate, pato) %>% 
    arrange(plu) %>% 
    group_by(plu) %>%
    filter(row_number()==1)
    
step1<-bind_rows(listado, ausentes, .id = "Fuente" ) %>% 
    arrange(plu) %>% 
    group_by(plu) %>% 
    fill(principioconcatenado, tipodetratamiento, categoria_list, patologia_list) %>% 
    fill(principioconcatenado, tipodetratamiento, categoria_list, patologia_list, .direction = "up") %>% 
    arrange(desc(Fuente)) %>% 
    filter(row_number()==1) %>% 
    left_join(duplicados)%>% 
    mutate(categoria_list=ifelse(!is.na(cate),cate,categoria_list),
           patologia_list=ifelse(!is.na(pato),pato,patologia_list)) %>% 
    select(-c(Fuente, cate, pato))

fwrite(step1, "BD/Depuradas/ListadMed.csv")

rm(list = ls(all=T))

