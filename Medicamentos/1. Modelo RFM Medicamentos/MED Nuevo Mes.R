rm(list = ls(all=T))
source("C:/Users/hernyatt/Documents/CamiloYate/Funciones.r")
### Carga e instalacion de paquetes ====
pkgs <- c("data.table", "dplyr", "stringr", "tidyr")
Loadpkg(pkgs)

#### Listado Patologia y Categoria

Listado <- readRDS("Protocolo/APP/Data/BD.rds") %>% select(prod_id, Patologia, Categoria) %>% 
    unique() %>%  
    mutate(Patologia=ifelse(Patologia=="SIN PATOLOGIA",NA,Patologia)) %>% 
    arrange(prod_id, Patologia) %>% 
    group_by(prod_id) %>% 
    fill(Patologia) %>% 
    arrange(prod_id) %>% 
    filter(row_number()==1) %>% 
    ungroup() %>% 
    mutate(prod_id=as.character(prod_id),
           Patologia=ifelse(is.na(Patologia),"SIN PATOLOGIA", Patologia))

#### Cargue y escritura de Datos ----

tipos=c(rep('character',9),rep('numeric',3),rep('character',5))

NuevoMes<-Cargar.Datos("./BD/Originales/Nuevo mes", clases = tipos) %>% 
    select(id_cliente,afiliado,segmento,pertencetuclub,id_sucursal,nombre_sucursal,ciudad,departamento,fecha,venta_neta,venta_bruta,cantidad_productos,prod_id,
           prod_nombre,prod_proveedor_nit,prod_proveedor_nombre,cod_ean) %>% 
    left_join(Listado, by=c("prod_id"="prod_id")) %>% 
    mutate()

mes=unique(substr(NuevoMes$fecha,1,6));mes

names(NuevoMes)<-c("Id_Cliente" ,"Afiliado","Segmento","PertencetuClub","Id_Sucursal" ,"nombre_Sucursal","Ciudad","Departamento","Fecha","Venta_Neta","Venta_Bruta",
                   "Cantidad_Productos","prod_id","prod_nombre","prod_proveedor_nit","prod_proveedor_nombre","cod_EAN","Patologia","Categoria" )

cont<-NuevoMes %>% group_by(Id_Cliente) %>% summarise(n=n()) %>%  arrange(desc(n))
head(cont)

fwrite(NuevoMes, paste0("./BD/Depuradas/POS/_",mes,".txt"))

# Llimpieza de ws----
rm(list=ls(all=T))

