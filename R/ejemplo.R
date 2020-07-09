#' @title Divisores
#' @description Calcula los dividores del numero x.
#' @param x un entero
#' @details Por escribir
#' @examples
#' divisores(10)
#' @export 
divisores<-function(x){

 if(trunc(x)==x){x=as.integer(x)}

 if(!is.integer(x)){ return("El numero ingresado no es entero")}

 else{  y=1:trunc(x/2);   return(y[x%%y==0]) }

}

#' @title Perfecto
#' @description Verifica si x es o no perfecto.
#' @param x un entero
#' @details Por escribir
#' @examples
#' is.perfecto(10)
#' is.perfecto(6)
#' @export 
is.perfecto<-function(x){

 div=divisores(x)

 if(x==sum(div)){ print(paste("El numero",x,"es perfecto")); return(TRUE) }

 else{  print(paste("El numero",x,"no es perfecto"));  return(FALSE)  }

}
