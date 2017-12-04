#################################################
######## INTENTO 2 / PROYECTO DE SIMULACION #####
#################################################

cant.paradas <- 10 #cantidad de paradas
cap.camion <- 60 #capacidad del camion
t.llegada <- runif(cant.paradas) #tasa de llegada de personas a la parada por unidad de tiempo
t.bajada <- c(0,runif(cant.paradas -1)) #tasa de bajada de pasajeros por parada
t.entre.paradas <- sample(3:15,cant.paradas, replace = TRUE) #tiempo de traslado entre paradas aproximado luego se le va a sumar  un numero de variacion
t.diferencia <- rep(0, cant.paradas) #tiempo de  retaraso o antisipaci??n
p.esperando <- rep(0, cant.paradas) #personas esperando en cada parada
Paradas <- data.frame(t.llegada, t.bajada, t.entre.paradas, t.diferencia, p.esperando)

Llegada <- c(0)#auxiliares solo para que el data frame T.espera tenga estos nombres en las columnas 
Salida <- c(0)
Parada <- c(0)
T.Espera <- data.frame(Llegada, Salida, Parada)#Registro de tiempo de espera de las personas en las paradas.

#c <- cap.camion

t.estimado.llegada <- c() #tiempo estimado de llegada con retrasos o antisipaciones 
for(i in 1:cant.paradas){
  t.estimado.llegada <- c(t.estimado.llegada, sum(Paradas$t.entre.paradas[1:i]))
}
#camiones
camiones <- matrix(0, ncol = 2+cant.paradas)
camiones[,2]<- 60
camiones <- data.frame(camiones)
colnames(camiones) <- c("pos", "cap", 1:cant.paradas)
#-----------------------------
tmax <- 10* sum(Paradas$t.entre.paradas[1:cant.paradas]) #duracion de la simulacion 
pa <- 0
pv <- 0
#-----------------------------
genera_pasaje <- function(i){
  for(j in 1:cant.paradas){ #las personas pueden llegar para cada min
    n <- runif(1)
    if(n < Paradas$t.llegada[j]){
      Paradas$p.esperando[j] <<- Paradas$p.esperando[j]+1 #llega una persona a la parada
      T.Espera <- rbind(T.Espera, c(i, 0, j))#se registra la persona que acaba de llegar
    }
  }
  return(T.Espera)
}

cant.camiones <- 1
#######
for(i in 1:tmax){
  #i=15
  if(i%%15 == 0){
    camiones <- rbind(camiones, c(0,60, rep(0,cant.paradas)))
    cant.camiones <- cant.camiones+1
  }
  
  T.Espera<-genera_pasaje(i)
  for(g in 1: cant.camiones){ #actualiza la pos del camion
    if(camiones[g,cant.paradas+2]==0){
      camiones[g,1] <- camiones[g,1]+1
    }
  }
  for(w in 1:cant.camiones){
    # w=1
    if(camiones[w,1]==t.estimado.llegada[1]){#estoy en la primera parada
      if(camiones[w,2] >= Paradas$p.esperando[1]){#se suben todas las personas si hay espacio suficiente
        camiones[w,2] <- camiones[w,2] - Paradas$p.esperando[1]
        #se registra tiempo de salida o termino de espera para el data frame t.Espera
        personas.esperando <- which(T.Espera$Parada == 1 & T.Espera$Salida == 0)
        for(d in personas.esperando){
          T.Espera$Salida[d] <- i
        }
        Paradas$p.esperando[1] <- 0 
        camiones[w,3] <- i
      }else{#si no hay espacio suficiente se suben las que quepan
        cuantas<- Paradas$p.esperando[1] - camiones[w,2] #cuantas se quedan
        personas.esperando <- which(T.Espera$Parada == 1 & T.Espera$Salida == 0)
        for(d in personas.esperando[1:camiones[w,2]]){
          T.Espera$Salida[d] <- i
        }
        Paradas$p.esperando[1] <- cuantas
        camiones[w,2] <- 0
        camiones[w,3] <- i
      }
    }
    if(camiones[w,1] %in% t.estimado.llegada[2:cant.paradas]){#si estas en una parada distint de la primera
      parad <- which(t.estimado.llegada == camiones[w,1])#en que parada estamos
      camiones[w,2] = camiones[w,2] + floor((cap.camion - camiones[w,2])* Paradas$t.bajada[parad])#se bajan personas del camion
      if(camiones[w,2] >= Paradas$p.esperando[parad]){#se suben todas las personas si hay espacio suficiente
        camiones[w,2] <- camiones[w,2] - Paradas$p.esperando[parad]
        #se registra tiempo de salida o termino de espera para el data frame t.Espera
        personas.esperando <- which(T.Espera$Parada == parad & T.Espera$Salida == 0)
        for(d in personas.esperando){
          T.Espera$Salida[d] <- i
        }
        Paradas$p.esperando[parad] <- 0 
        camiones[w,parad+2] <- i
      }else{#si no hay espacio suficiente se suben las que quepan
        cuantas<- Paradas$p.esperando[parad] - camiones[w,2] #cuantas se quedan
        personas.esperando <- which(T.Espera$Parada == parad & T.Espera$Salida == 0)
        for(d in personas.esperando[1:camiones[w,2]]){
          T.Espera$Salida[d] <- i
        }
        Paradas$p.esperando[parad] <- cuantas
        camiones[w,2] <- 0
        camiones[w,parad+2] <- i
      }
    }
    #ACCIDENTE
    for(g in 1: cant.camiones){
      if(camiones[g,cant.paradas+2]==0){
        if(runif(1) < pa){
          camiones[g,1] <- camiones[g,1]- sample(1:10,1)
        }
      }
    }
    #VENTAJA
    for(g in 1: cant.camiones){
      # g=1
      if(camiones[g,cant.paradas+2]==0){
      if(runif(1) < pv){
        camiones[g,1] <- camiones[g,1] + 1
        
        if(camiones[g,1]==t.estimado.llegada[1]){#estoy en la primera parada
          if(camiones[w,2] >= Paradas$p.esperando[parad]){#se suben todas las personas si hay espacio suficiente
            camiones[w,2] <- camiones[w,2] - Paradas$p.esperando[parad]
            #se registra tiempo de salida o termino de espera para el data frame t.Espera
            personas.esperando <- which(T.Espera$Parada == parad & T.Espera$Salida == 0)
            for(d in personas.esperando){
              T.Espera$Salida[d] <- i
            }
            Paradas$p.esperando[parad] <- 0 
            camiones[w,3] <- i
          }else{#si no hay espacio suficiente se suben las que quepan
            cuantas<- Paradas$p.esperando[parad] - camiones[w,2] #cuantas se quedan
            personas.esperando <- which(T.Espera$Parada == parad & T.Espera$Salida == 0)
            for(d in personas.esperando[1:camiones[w,2]]){
              T.Espera$Salida[d] <- i
            }
            Paradas$p.esperando[parad] <- cuantas
            camiones[w,2] <- 0
            camiones[w,3] <- i
          }
        }
        if(camiones[g,1] %in% t.estimado.llegada[2:cant.paradas]){
          parad <- which(t.estimado.llegada == camiones[g,1])#en que parada estamos
          camiones[w,2] = camiones[w,2] + floor((cap.camion - camiones[w,2])* Paradas$t.bajada[parad])#se bajan personas del camion
          if(camiones[w,2] >= Paradas$p.esperando[parad]){#se suben todas las personas si hay espacio suficiente
            camiones[w,2] <- camiones[w,2] - Paradas$p.esperando[parad]
            #se registra tiempo de salida o termino de espera para el data frame t.Espera
            personas.esperando <- which(T.Espera$Parada == parad & T.Espera$Salida == 0)
            for(d in personas.esperando){
              T.Espera$Salida[d] <- i
            }
            Paradas$p.esperando[parad] <- 0 
            camiones[w,parad+2] <- i
          }else{#si no hay espacio suficiente se suben las que quepan
            cuantas<- Paradas$p.esperando[parad] - camiones[w,2]#cuantas se quedan
            personas.esperando <- which(T.Espera$Parada == parad & T.Espera$Salida == 0)
            for(d in personas.esperando[1:camiones[w,2]]){
              T.Espera$Salida[d] <- i
            }
            Paradas$p.esperando[parad] <- cuantas
            camiones[w,2] <- 0
            camiones[w,parad+2] <- i
          }
        }
      }
    }
    }
  }
  if(all(camiones[,cant.paradas+2]!= 0)){
    break
  }
}
T.Espera <- T.Espera[-1,]