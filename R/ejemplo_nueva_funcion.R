#' @import faraway
#' @title Multicolinealidad
#' @description Calcula el factor de inflacion de varianza de todas las combinaciones posibles
#' @param x un data.frame o matriz
#' @details Por escribir
#' @examples
#' x<-matrix(rnorm(100*5),nc=5,nr=100)
#' multicolinealidad(x) 
#' @export 
multicolinealidad<-function(x){
 m=ncol(x)
 VIF=list()
 if(m==1){return("solo hay una covariable")}else{
  for(j in 2:m){ VIF[[j-1]]=vif(x[,1:j]) }
  return(VIF)
 }
}