<<<<<<< HEAD
rm(list=ls(all=T))

###  Librerias ----
source("C:/Users/Camilo Yate Tamara/Google Drive/R/Funciones.R")
pkgs<-c("data.table", "dplyr", "Hmisc", "ggplot2", "woeBinning")
Loadpkg(pkgs)

### Cargar Datos ----

# Base Calificada -
calif<-fread("./BD/Resultados/Calificadas/Jul2018.csv")

# Base Personas
persona <- fread("./BD/Persona_.txt") %>% 
  group_by(IdPersona) %>% 
  filter(row_number()==1)


# Union
bd<-calif %>% 
  left_join(persona , by =c("identificacion"="IdPersona")) %>% 
  mutate(Score=round(Prob1*1000,0),
         Score_Group=cut2(Score, g=10),
         Score_Group=as.factor(ifelse(Score<11, "[  0,  11)", as.character(Score_Group)))
         )

rm(calif, persona)

## Grupo por cliente :

max(bd$fecha_proceso)
fwrite(bd, "./Resultados/Tableau/ModeloRetencion.csv")

### Gráficas ----

## "Sigmoide"
aux1<-bd %>% select(Score) %>% arrange(Score) %>% mutate(n=row_number())

ggplot(aux1, aes(x=n, y=Score))+
  geom_smooth()+
  labs(title="Score de Deserción", x="", y="Score")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

table(bd$Score_Group)

# Binning

# vars_to_bin<-BD_Clasificacion %>% select(-c(Colsubsidio)) %>% names()
# bin<-woe.tree.binning(bd, 'Colsubsidio', vars_to_bin, 0.05)
# BD_Clasificacion<-woe.binning.deploy(BD_Clasificacion, bin) %>% select(-one_of(vars_to_bin))


=======
rm(list=ls(all=T))

###  Librerias ----
source("C:/Users/Camilo Yate Tamara/Google Drive/R/Funciones.R")
pkgs<-c("data.table", "dplyr", "Hmisc", "ggplot2", "woeBinning")
Loadpkg(pkgs)

### Cargar Datos ----

# Base Calificada -
calif<-fread("./BD/Resultados/Calificadas/Jul2018.csv")

# Base Personas
persona <- fread("./BD/Persona_.txt") %>% 
  group_by(IdPersona) %>% 
  filter(row_number()==1)


# Union
bd<-calif %>% 
  left_join(persona , by =c("identificacion"="IdPersona")) %>% 
  mutate(Score=round(Prob1*1000,0),
         Score_Group=cut2(Score, g=10),
         Score_Group=as.factor(ifelse(Score<11, "[  0,  11)", as.character(Score_Group)))
         )

rm(calif, persona)

## Grupo por cliente :

max(bd$fecha_proceso)
fwrite(bd, "./Resultados/Tableau/ModeloRetencion.csv")

### Gráficas ----

## "Sigmoide"
aux1<-bd %>% select(Score) %>% arrange(Score) %>% mutate(n=row_number())

ggplot(aux1, aes(x=n, y=Score))+
  geom_smooth()+
  labs(title="Score de Deserción", x="", y="Score")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

table(bd$Score_Group)

# Binning

# vars_to_bin<-BD_Clasificacion %>% select(-c(Colsubsidio)) %>% names()
# bin<-woe.tree.binning(bd, 'Colsubsidio', vars_to_bin, 0.05)
# BD_Clasificacion<-woe.binning.deploy(BD_Clasificacion, bin) %>% select(-one_of(vars_to_bin))


>>>>>>> 5308310d9fd680f7f63a4eff58a3d19ee6c67bee
