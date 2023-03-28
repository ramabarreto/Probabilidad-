#Probabilidad curso 12, TP 2. 

#Ejercicio (d) IV
#1. Simular 1000 valores de la variable X.
#2. A partir de la muestra generada en el ítem 1 realizar una estimación gráfica de la
#densidad de X.
#3. A partir de la muestra generada en el ítem 1 estimar la media y varianza de X.
#4. Sea Y = g(X), donde g es la función dada como dato. ¿Qué tipo de variable aleatoria
#es Y ?. Simular 1000 valores de la variable Y a partir de la muestra generada en el
#ítem 1. Estimar la media y varianza de Y .
#5. Graficar la función de distribución empírica de Y correspondiente a la muestra generada en el ítem 4.
#6. Aproximar las probabilidades que se indican en cada caso utilizando la función graficada en el ítem 5.

#Integrantes: Alejandro Saragossi y Ramiro Barreto

#Por definición, F_X(x) es igual a la integral de menos infinito a x de f_x(t) diferencial t. 
#Realizando dicho cálculo se obtiene que F_X(x) = (-(1-x)^(3/2)+1) 1{0<= x <= 1} + 1{x => 1}
#Ahora bien, a partir de una uniforme, nuestra intención es simular X. Con lo cual, debemos 
#hallar h(U) = X tal que F_X(x) = F_U(u) = u. Despejando, x = 1 - (1-u)^(2/3). 
#En consecuencia, procederemos a simular 1000 valores de una variable aleatoria uniforme y evaluar cada uno de 
#ellos en la función que hallamos. 
#Con estos conceptos realicemos una función que simule 1000 valores de la variable X

#Fijemos además una semilla para que cada vez que comience el experimento el R haga lo mismo.

set.seed(27)

#Punto 1. 

muestras <- function(n)
{
  #Para simular n valores de una uniforme (0, 1), simplemente aplicamos:
  x <- c()
  u <- c()
  for (i in 1:n)
  {
    u[i] <- runif(1, 0, 1)
    x[i] <- 1 - (1-u[i])^(2/3) #Aplicamos la inversa generalizada a cada valor obtenido
  }
  return(x)
}
kurtosis(x)
#Se simulan 1000 valores valuando la función en n = 1000
muestras(1000)

#Punto 2.
#Ploteando la función density() obtenemos una aproximación gráfica de la densidad de X

plot(density(muestras(1000)), main = "Estimación de la función de densidad", xlim=c(0, 1), ylim = c(0, 1.5))

#Podemos también comparar el gráfico de la función de densidad real con la estimada a través de la función par(): 

plot(density(muestras(1000)), main = "Comparación entre la estimación y la real", xlim=c(0, 1), ylim = c(0, 1.5))
xg <- seq(0, 1, 0.0005)
yg <- 3/2*sqrt(1-xg)
par(new = TRUE)
plot(xg, yg, xlab = "", ylab = "", pch=20, type = "l", col = "red")

#Punto 3. 
#Para resolver realizaremos dos funciones que para un dado número de repeticiones nos entregan la esperanza 
#y la varianza estimadas. 

esperanza_estimadaX <- function(e)
{
  esp <- mean(muestras(e)) #Para calcular el promedio y así estimar la esperanza utilizamos mean()
  return(esp)
}

#En este caso en particular evaluamos en 1000

esperanza_estimadaX(1000)

#De manera similar, para la varianza, creamos la función varianza_estimada(v)

varianza_estimadaX<- function(v)
{
  var <- (sd(muestras(v)))^2 #Mediante sd() obtenemos el desvío estándar, que elevado al cuadrado, nos da la varianza
  return(var)
}

#Análogamente, tenemos que la varianza estimada para 1000 repeticiones es:

varianza_estimadaX(1000)

#Punto 4.
#La variable Y es mixta, pues tiene un conjunto de átomos no vacío (esto se aprecia posteriormente en el 
#gráfico de la función de distribución empírica) y la suma de las probabilidades de los mismos es menor a 1. 

#Para simular los valores de la variable Y, definiremos una nueva función que aplique la transformación dada
#a cada valor de la muestra generada en 1, teniendo en cuenta el intervalo en que se encuentre dicho valor. 

variableY <- function(a)
{
  #Empezamos definiendo un vector vacío, v, en donde guardaremos las a muestras
  
  v <- c()
  
  #Posteriormente asignamos a x, el vector que contiene la muestra generada en el ítem 1
  
  x <- muestras(a)
  
  #En esta instancia, discriminamos todos los casos posibles indicando qué transformación aplicar a la 
  #i ésima componente de x
  
  for (i in 1:a) 
  {
    if(0.2 <= x[i] & x[i] < 0.7)
    {
      v[i] <- 8*(x[i] - 0.2)^2
    }
    if(x[i] >= 0.7)
    {
      v[i] <- 2  
    }
    if(x[i] < 0.2)
    {
      v[i] <- 0
    }
      
  }
  return(v) 
}

#La simulación de los mil valores de la variable Y a partir de la muestra generada en el ítem 1 está dada por

variableY(1000)

#Equivalentemente a lo hecho en el punto 3, definimos dos funciones que estimen la esperanza y la varianza de Y

esperanza_estimadaY <- function(e)
{
  esp <- mean(variableY(e)) #Para calcular el promedio y así estimar la esperanza utilizamos mean()
  return(esp)
}

#En este caso en particular evaluamos en 1000

esperanza_estimadaY(1000)

varianza_estimadaY<- function(v)
{
  var <- (sd(variableY(v)))^2 #Mediante sd() obtenemos el desvío estándar, que elevado al cuadrado, nos da la varianza
  return(var)
}

#Análogamente, tenemos que la varianza estimada de Y para 1000 repeticiones es:

varianza_estimadaY(1000)

#Punto 5. 
#En cuanto a la gráfica de la función de distribución empírica de Y correspondiente 
#a la muestra generada en el ítem 4, aplicando la función ecdf() y ploteando, se tiene que

plot(ecdf(variableY(1000)), main="Función de distribución empírica", pch = 20)

#Punto 6.
#Con el fin de estimar las probabilidades pedidas, haremos uso de la función de distribución empírica:

Fn <- function(n)
{
  Fn <- ecdf(variableY(1000)) #Asignamos a Fn la función de distribución empírica de las 1000 muestras de 
                              #la variable Y
  return(Fn(n))
}

#Por definición P(Y <= y) = F_Y(y), con lo cual, mediante Fn() podemos estimar 
#(basándonos en la función de distribución empírica) la P(Y <= 1). De este modo:

probabilidad_Y_menor_o_igual_a_uno <- Fn(1)

#Se sabe que la probabilidad de un átomo es "el salto que pega la función en ese punto", lo que equivale
#a la diferencia entre la función de distribución "a derecha y a izquierda". Adoptaremos 1.99999999 como 
#punto inmediatamente a la izquierda de 2 y, de aquí:

probabilidad_Y_igual_a_dos <- Fn(2) - Fn(1.99999999)


