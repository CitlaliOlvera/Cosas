library(parallel)
dim <- 40
num <-  dim^2
nucleos <- 15

actual <- matrix(rep(0,num), nrow=dim, ncol=dim)
al <- sample(1:num,nucleos)
al2 <- sample(1:nucleos)
for(y in 1:nucleos){
  actual[al[y]]=al2[y]
}
suppressMessages(library("sna"))
png("Reto1p2_t0P.png")
plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
graphics.off()

paso <- function(pos){
  fila <- floor((pos - 1) / dim) + 1
  columna <- ((pos - 1) %% dim) + 1
  if(actual[fila, columna] == 0){	
    
    vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                        max(columna - 1, 1): min(columna + 1, dim)]
    
    if((sum(vecindad)) > 0)
    {
           vecaux <- c()
      for(g in 1:length(vecindad)){
        if(vecindad[g] != 0 ){
          vecaux <- c(vecaux, vecindad[g])
        }
      }
      valor <- vecaux[1]
      return(valor)
    }
    else
    {
      return(actual[fila,columna])
    }
  }
  else {
    return(actual[fila,columna])
  }
}


cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")

for (iteracion in 1:35) {
  clusterExport(cluster, "actual")
  siguiente <- parSapply(cluster, 1:num, paso)
  msj <- paste("Iteracion:", iteracion)
  print(msj)

  actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
  salida = paste("Reto1p2_tP", iteracion, ".png", sep="")
  tiempo = paste("Paso", iteracion)
  png(salida)
  plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
  graphics.off()
  
  if (all(actual != 0)) { # todos murieron
    print("Termina propagacion")
    break;
  }
}
stopCluster(cluster)