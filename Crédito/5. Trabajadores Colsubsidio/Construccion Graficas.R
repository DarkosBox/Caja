<<<<<<< HEAD
rm(list = ls(all=T))

Limpiar.Cadenas <-function(x, espacios=T){
  x<-gsub("\\.", "_",tolower(gsub("\\W","",x)))
  x<-gsub("([\\W])\\1+","\\1",iconv(x,to="ASCII//TRANSLIT"), perl=T)
  if(!espacios){
    x<-gsub("\\s","",iconv(x,to="ASCII//TRANSLIT"), perl=T)
  } else {
    x
  }
}

library("data.table");library("dplyr");library("ggplot2")

# Afiliados

afil<-fread("~/CamiloYate/2018/Otros/5. Innovacion/1. Proyecto Dados/BD/Persona_3Ago18.csv") %>% 
  filter(`Piramide 2`=="4.9 Colsubsidio") %>% 
  select(id_persona) %>% unique()

table(afil$`Piramide 2`)
  
# COnsumos
cons<-fread("~/CamiloYate/2018/Otros/5. Innovacion/1. Proyecto Dados/BD/agrupacion_colsubsidio_.txt", sep=";",
            colClasses = c(rep("character",3), "factor", rep("numeric",65))) %>% 
  select(-id_empresa, -id_grupo_familiar, -id_grupo_familiar, -Parentesco) %>% 
  group_by(id_afiliado) %>% 
  summarise_all(funs(max(., na.rm=T))) %>% 
  mutate_all(funs(ifelse(is.infinite(.), 0, .)))

# Uni??n
data<-afil %>% left_join(cons, by=c("id_persona"="id_afiliado")) %>% 
  mutate_if(is.numeric, funs(as.factor(ifelse(is.na(.), 0, .))))
names(data)=Limpiar.Cadenas(names(data))


names(data)
rm(g1)
vars<-names(data)[2:66]

vars[1]

for(i in 1:length(vars)){
  aux1<-data %>% group_by_(vars[i]) %>% 
    summarise(Freq=round((n()/13890)*100,0))
  names(aux1)<-c("var", "Porcentaje")
  
  ggplot(data=aux1, aes(x="", y=Porcentaje, fill=var)) +
    geom_bar(width = 0.3, stat = "identity")+
    coord_flip()+
    labs(title=vars[i])+
    scale_fill_discrete(name = vars[i])+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())
  
  ggsave(paste0("C:/Users/hernyatt/Desktop/Empleados/",i,".png"), width = 16, height = 9, dpi = 100)
  
}

names(data)
  

  
=======
rm(list = ls(all=T))

Limpiar.Cadenas <-function(x, espacios=T){
  x<-gsub("\\.", "_",tolower(gsub("\\W","",x)))
  x<-gsub("([\\W])\\1+","\\1",iconv(x,to="ASCII//TRANSLIT"), perl=T)
  if(!espacios){
    x<-gsub("\\s","",iconv(x,to="ASCII//TRANSLIT"), perl=T)
  } else {
    x
  }
}

library("data.table");library("dplyr");library("ggplot2")

# Afiliados

afil<-fread("~/CamiloYate/2018/Otros/5. Innovacion/1. Proyecto Dados/BD/Persona_3Ago18.csv") %>% 
  filter(`Piramide 2`=="4.9 Colsubsidio") %>% 
  select(id_persona) %>% unique()

table(afil$`Piramide 2`)
  
# COnsumos
cons<-fread("~/CamiloYate/2018/Otros/5. Innovacion/1. Proyecto Dados/BD/agrupacion_colsubsidio_.txt", sep=";",
            colClasses = c(rep("character",3), "factor", rep("numeric",65))) %>% 
  select(-id_empresa, -id_grupo_familiar, -id_grupo_familiar, -Parentesco) %>% 
  group_by(id_afiliado) %>% 
  summarise_all(funs(max(., na.rm=T))) %>% 
  mutate_all(funs(ifelse(is.infinite(.), 0, .)))

# Uni??n
data<-afil %>% left_join(cons, by=c("id_persona"="id_afiliado")) %>% 
  mutate_if(is.numeric, funs(as.factor(ifelse(is.na(.), 0, .))))
names(data)=Limpiar.Cadenas(names(data))


names(data)
rm(g1)
vars<-names(data)[2:66]

vars[1]

for(i in 1:length(vars)){
  aux1<-data %>% group_by_(vars[i]) %>% 
    summarise(Freq=round((n()/13890)*100,0))
  names(aux1)<-c("var", "Porcentaje")
  
  ggplot(data=aux1, aes(x="", y=Porcentaje, fill=var)) +
    geom_bar(width = 0.3, stat = "identity")+
    coord_flip()+
    labs(title=vars[i])+
    scale_fill_discrete(name = vars[i])+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())
  
  ggsave(paste0("C:/Users/hernyatt/Desktop/Empleados/",i,".png"), width = 16, height = 9, dpi = 100)
  
}

names(data)
  

  
>>>>>>> 5308310d9fd680f7f63a4eff58a3d19ee6c67bee
