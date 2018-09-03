Loadpkg <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = TRUE)
}

convertir.contabilidad <- function(x){
  if(grepl("\\(.*\\)", x)){
    as.numeric(paste0("-", gsub("\\(|\\)", "", gsub("[\\$, ]", "", x))))
  } else {
    as.numeric(gsub("[\\$, ]", "", x))
  }
}

Limpiar.Cadenas <-function(x, espacios=T){
  x<-gsub("\\.", "_",tolower(gsub("\\W","",x)))
  x<-gsub("([\\W])\\1+","\\1",iconv(x,to="ASCII//TRANSLIT"), perl=T)
  if(!espacios){
    x<-gsub("\\s","",iconv(x,to="ASCII//TRANSLIT"), perl=T)
  } else {
        x
    }
}

Unir.Cadenas <- function(..., sep = " ", collapse = NULL, na.rm = F) {
    if (na.rm == F)
        paste(..., sep = sep, collapse = collapse)
    else
        if (na.rm == T) {
            paste.na <- function(x, sep) {
                x <- gsub("^\\s+|\\s+$", "", x)
                ret <- paste(na.omit(x), collapse = sep)
                is.na(ret) <- ret == ""
                return(ret)
            }
            df <- data.frame(..., stringsAsFactors = F)
            ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))
            
            if (is.null(collapse))
                ret
            else {
                paste.na(ret, sep = collapse)
            }
        }
}

Cargar.Datos<-function (carpeta, extension="csv", exhaustivo=F, n_ultimos=0, ordenado=T, ausentes=getOption("datatable.na.strings","NA"), separador="auto",
                        dec=".", quote="\"", header="auto"){
    
    if(ordenado) file.list <- paste0(carpeta,"/",list.files(carpeta, pattern = paste0("+.",extension), recursive = F))
    else file.list <- sort(paste0(carpeta,"/",list.files(carpeta, pattern = paste0("+.",extension), recursive = F)))
    m=length(file.list)
    n=ifelse((n_ultimos>=m| n_ultimos<1),1, m-(n_ultimos-1))
    
    file.list<-file.list[n:length(file.list)]
    
    print("Importando:: ")
    print(file.list)
    Union <- do.call("rbind",
                     lapply(file.list, FUN = function(file) {
                         fread(file, sep=separador, dec=dec, quote=quote, header = header,
                               na.strings = ausentes,
                               col.names = names(fread(file.list[1], nrows = 1))
                         )}
                     )
    )
    return(Union)
}
