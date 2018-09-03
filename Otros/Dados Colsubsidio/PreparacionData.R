rm(list=ls(all=T))
source("C:/Users/hernyatt/Documents/CamiloYate/Funciones.R")
pkgs <- c("data.table", "tidyverse", "lubridate")
Loadpkg(pkgs)

#### Cargar datos ----

## Consumo Recreación.
consumo<-fread("BD/agrupacion.txt", stringsAsFactors = T) 
names(consumo)<-Limpiar.Cadenas(names(consumo))

consumo<-consumo %>%  
  select(1,vacacionesrecreodeportivas,billar,clasesgrupales,escuelasdeportivas,futbol,infantil,videojuegos,gimnasios,
         inflables,pingpong,spa,zonashumedas, alquileres) %>% 
  mutate(Futbol=pmax(futbol,alquileres, na.rm = T),
         Juegos=pmax(billar,videojuegos, pingpong, na.rm = T),
         Fisico=pmax(clasesgrupales,gimnasios, na.rm = T),
         Yoga=pmax(clasesgrupales,gimnasios,spa, zonashumedas, na.rm = T),
         Infantil=pmax(vacacionesrecreodeportivas,escuelasdeportivas,infantil,videojuegos,inflables, na.rm = T)
         ) %>% 
  group_by(id_afiliado, Futbol, Juegos, Fisico, Yoga,Infantil) %>% 
  summarize(N_Beneficiarios=n()) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.),0,.))) %>% 
  group_by(id_afiliado) %>% 
  mutate_all( funs(max(., na.rm = T))) 

## Caracteristicas de la persona
fecha_ref=floor_date(Sys.Date(), "month") - days(1)

## Persona
persona<-fread("BD/Persona.csv", stringsAsFactors = T)
names(persona)<-Limpiar.Cadenas(names(persona))
persona<-persona %>% 
  mutate(Edad=Calcular.Edad(as.Date(fechanacimiento, "%d/%m/%Y"), fecha_ref)) %>% 
  select(-fechanacimiento)


levels(persona$segmento_poblacional)
table(persona$segmento_poblacional)

## Empresa
empresa<-fread("BD/Empresa.csv", stringsAsFactors = T)
names(empresa)<-Limpiar.Cadenas(names(empresa))
empresa<-empresa %>% 
  select(-razonsocial, -fecharetiro, -idnitprincipal, -nombreempprincipal, -id_ciiu, -fechaafiliacion, -piramide1, -piramide2) 
names(empresa)<-c(names(empresa)[1:3],paste0("n_",names(empresa)[4:13]))

Caracteristicas<-persona %>% 
  left_join(empresa) %>% 
  arrange(id_persona, piramide2) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  select(-id_empresa)

### Union Final ----
Mode <- function(x) {
  x=BD$nivel_socioeconomico[!is.na(BD$nivel_socioeconomico)]
  ux=unique(x)
  mode=ux[which.max(tabulate(match(x, ux)))]
  return(mode)
}

BD<- consumo %>% 
  left_join(Caracteristicas, by=c("id_afiliado"="id_persona")) 

rm(Caracteristicas, consumo, persona, empresa)

ax1<- median(BD$cx, na.rm = T);ax1
ax2<- median(BD$cy, na.rm = T);ax2
ax3<- Mode(BD$nivel_socioeconomico);ax3
ax4<- median(BD$n_basico, na.rm = T);ax4
ax5<- median(BD$n_medio, na.rm = T);ax5
ax6<- median(BD$n_joven, na.rm = T);ax6
ax7<- median(BD$n_alto, na.rm = T);ax7
ax8<- median(BD$n_f, na.rm = T);ax8
ax9<- median(BD$n_m, na.rm = T);ax9
ax10<- median(BD$n_categoriaa, na.rm = T);ax10
ax11<- median(BD$n_categoriab, na.rm = T);ax11
ax12<- median(BD$n_categoriac, na.rm = T);ax12
ax13<- Mode(BD$segmento_poblacional);ax13

BD1<-BD %>% 
  mutate(
    nivel_socioeconomico=ifelse(is.na(nivel_socioeconomico), ax3, nivel_socioeconomico),
    segmento_poblacional=ifelse(is.na(segmento_poblacional), ax13, segmento_poblacional),
    n_basico=ifelse(is.na(n_basico), ax4, n_basico),
    n_medio=ifelse(is.na(n_medio), ax5, n_medio),
    n_joven=ifelse(is.na(n_joven), ax6, n_joven),
    n_alto=ifelse(is.na(n_alto), ax7, n_alto),
    n_f=ifelse(is.na(n_f), ax8,  n_f),
    n_m=ifelse(is.na(n_m), ax9 , n_m),
    n_categoriaa=ifelse(is.na(n_categoriaa), ax10, n_categoriaa),
    n_categoriab=ifelse(is.na(n_categoriab), ax11, n_categoriab),
    n_categoriac=ifelse(is.na(n_categoriac), ax12, n_categoriac)
  )

colMeans((is.na(BD1)))

fwrite(BD1, "BD/Depuradas/BD.csv")

