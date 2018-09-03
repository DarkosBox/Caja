source("D:/CamiloYate/Funciones.R")

Depurar.Numero<-function(x){
    as.numeric(gsub(",","\\.",gsub("\\.","",x)))
}

### Librerias ----

pkgs<-c("tools","dplyr","tidyr", "data.table", "lubridate")
Loadpkg(pkgs)

### Carga de base de datos ----

temp <- list.files(".",pattern = "+.txt", recursive = T)
tipos=c('character','character','character','character','character','character','numeric','character',
        'character','character','numeric','character','character','numeric','numeric','character',
        'character','character','character','character','character','character','character','character',
        'character','character','character','character','numeric','numeric','character','character',
        'character','character','character','character','character','character','character','character',
        'character','character','numeric','numeric','numeric','character','character','character',
        'character','character','character','character','character','character','numeric','character',
        'numeric','character','character','character','character','character','numeric','character',
        'numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric',
        'numeric','numeric','numeric','numeric','numeric','numeric','character','numeric',
        'character','character','character','character','character','character','character','character',
        'character','character','character','numeric','character','character','character','character',
        'character','character','character','character','character','character','character','character')

BD <- fread(temp[1], colClasses = tipos)[,c(1,2,5,7,11,14,15,19,20,21,29,30,31,43,44,45,46,47,
                                            48,50,55,56,57,58,63,65,66,67,68,69,70,71,72,
                                            73,74,75,76,77,78,80,92)]
names(BD)<-Limpiar.Cadenas(names(BD), espacios = F)

error=character()
for (i in 2:length(temp) ){
    tmp<-fread(temp[i], colClasses = tipos)[,c(1,2,5,7,11,14,15,19,20,21,29,30,31,43,44,45,46,47,
                                               48,50,55,56,57,58,63,65,66,67,68,69,70,71,72,
                                               73,74,75,76,77,78,80,92)]
    names(tmp)<-Limpiar.Cadenas(names(tmp), espacios = F)
    if (dim(tmp)[2] == dim(BD)[2]) {
        BD<-rbind(BD,tmp)
    }
    else {
        error<-rbind(error, file_path_sans_ext(temp[1]))
    }
}
names(BD)<-Limpiar.Cadenas(names(BD), espacios = F)
rm(tipos, temp, tmp, error, i)

str(BD)

fwrite(BD,"BD/Depuradas/ConsolidacionCobranzas.csv", sep=";")

### Agregaciones ----

names(BD)

Q1<-BD %>% group_by(nroidentificacion) %>% 
    summarise(Saldo=sum(saldo),
              Cupo=max(limitecupo))


unique(BD$estadotarjeta)
=======
source("D:/CamiloYate/Funciones.R")

Depurar.Numero<-function(x){
    as.numeric(gsub(",","\\.",gsub("\\.","",x)))
}

### Librerias ----

pkgs<-c("tools","dplyr","tidyr", "data.table", "lubridate")
Loadpkg(pkgs)

### Carga de base de datos ----

temp <- list.files(".",pattern = "+.txt", recursive = T)
tipos=c('character','character','character','character','character','character','numeric','character',
        'character','character','numeric','character','character','numeric','numeric','character',
        'character','character','character','character','character','character','character','character',
        'character','character','character','character','numeric','numeric','character','character',
        'character','character','character','character','character','character','character','character',
        'character','character','numeric','numeric','numeric','character','character','character',
        'character','character','character','character','character','character','numeric','character',
        'numeric','character','character','character','character','character','numeric','character',
        'numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric',
        'numeric','numeric','numeric','numeric','numeric','numeric','character','numeric',
        'character','character','character','character','character','character','character','character',
        'character','character','character','numeric','character','character','character','character',
        'character','character','character','character','character','character','character','character')

BD <- fread(temp[1], colClasses = tipos)[,c(1,2,5,7,11,14,15,19,20,21,29,30,31,43,44,45,46,47,
                                            48,50,55,56,57,58,63,65,66,67,68,69,70,71,72,
                                            73,74,75,76,77,78,80,92)]
names(BD)<-Limpiar.Cadenas(names(BD), espacios = F)

error=character()
for (i in 2:length(temp) ){
    tmp<-fread(temp[i], colClasses = tipos)[,c(1,2,5,7,11,14,15,19,20,21,29,30,31,43,44,45,46,47,
                                               48,50,55,56,57,58,63,65,66,67,68,69,70,71,72,
                                               73,74,75,76,77,78,80,92)]
    names(tmp)<-Limpiar.Cadenas(names(tmp), espacios = F)
    if (dim(tmp)[2] == dim(BD)[2]) {
        BD<-rbind(BD,tmp)
    }
    else {
        error<-rbind(error, file_path_sans_ext(temp[1]))
    }
}
names(BD)<-Limpiar.Cadenas(names(BD), espacios = F)
rm(tipos, temp, tmp, error, i)

str(BD)

fwrite(BD,"BD/Depuradas/ConsolidacionCobranzas.csv", sep=";")

### Agregaciones ----

names(BD)

Q1<-BD %>% group_by(nroidentificacion) %>% 
    summarise(Saldo=sum(saldo),
              Cupo=max(limitecupo))


unique(BD$estadotarjeta)
str(BD)
