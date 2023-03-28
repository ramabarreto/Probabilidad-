#Probabilidad curso 12, TP 1. Ejercicio 13, inciso a.

#Una urna contiene 10 bolas numeradas del 0 al 9.
#Se extraen al azar con reposición cinco bolas. Calcular la probabilidad de que
#1. las cinco sean iguales.
#2. según el orden de extracción se observen los números 1,3,5,7,9.
#3. se observen los cinco números impares.
#4. las cinco sean distintas.

#Integrantes: Alejandro Saragossi y Ramiro Barreto

#Empecemos por definir nuestra urna, para ello, denotemos bi a la bolita i, con i = 0, 1, ...,9.

urna <- c("b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9")

#Fijemos además una semilla para que cada vez que comience el experimento el R haga lo mismo.

set.seed(27)

#Simulemos ahora nuestro experimento mediante la función "simulation" 

simulation <- function(n)
{
  #Definamos la matriz donde cargaremos nuestros resultados y el vector "sacada" que llevara los resultados 
  #de cada experimento aleatorio.
  
  A <- matrix(0, nrow = n, ncol = 5)
  sacada <- c()
  for(i in 1:n)
  {
    sacada <- sample(urna, 5, TRUE) #Del vector "urna" saco 5 elementos con reposición
    A[i,] <- sacada #En cada fila de la matriz colocamos los resultados del i ésimo experimento aleatorio
  }
  return(A) #La función devuelve la matriz A con la que trabajaremos a continuación
}

#Para resolver cada punto, buscaremos funciones que devuelvan, para una dada cantidad de simulaciones (n), 
#la frecuencia relativa del evento en estudio, teniendo en cuenta que esta es la probailidad cuando n 
#tiende a infnito
 
#Punto 1.

#Para hacerlo, compara si el primer elemento de la j ésima fila es igual a los demás

prob_pto1 <- function(v)
{  
  cont <- 0
  A <- simulation(v)
  for(j in 1:v)
  {
    if(A[j,1] == A[j,2] && A[j,1] == A[j,3] && A[j,1] == A[j,4] && A[j,1] == A[j,5])
    {
    cont <- cont + 1
    }
  }
  #Ahora bien, podemos estimar la probabilidad mediante la frecuencia relativa; en nuestro caso: cont/v
  rta <- cont/v
  return(rta)
}

#Punto 2. 

#En este caso, la función evalúa, para cada vector fila, si coincide con el pedido (1,3,5,7,9) 

prob_pto2 <- function(a)
{
  cont <- 0
  A <- simulation(a)
  for (j in 1:a)
  {
    if(A[j,1] == "b1" && A[j,2] == "b3" && A[j,3] == "b5" && A[j,4] == "b7" && A[j,5] == "b9")
    {
      cont <- cont + 1
    }
  }
  #Nuevamente, para la aproximación de la probabilidad usamos la frecuencia relativa 
  rta <- cont/a
  return(rta)
}

#Punto 3

#Habiendo definido el vector "impar", prob_pto3 evalúa si las componentes de cada vector fila pertenecen al mismo

prob_pto3 <- function(b)
{
  cont <- 0
  A <- simulation(b)
  impar <- c("b1", "b3", "b5", "b7", "b9")
  for (j in 1:b)
  {
    if(A[j,1] %in% (impar) && A[j,2] %in% (impar) && A[j,3] %in% (impar) && A[j,4] %in% (impar) && A[j,5] %in% (impar))
    {
      cont <- cont + 1
    }
  }
  #Análogamente a lo ya hecho, calculamos la frecuencia relativa
  rta <- cont/b
  return(rta)
}

#Punto 4

#Usando "unique(w)", que devuelve una copia del vector w sin los elementos repetidos, analizamos si la 
#dimensión de dicho vector es igual a 5; esto implicaría que se han extraído bolas distintas  

prob_pto4 <- function(t)
{
  cont <- 0
  A <- simulation(t)
  for (j in 1:t)
  {
    if(length(unique(A[j,])) == 5)
    {
      cont <- cont + 1
    } 
  }
  rta <- cont/t
  return(rta)
}
