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
