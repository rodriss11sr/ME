## Jaime Millan Ibañez Archilla
## Rodrigo Sosa Saez
## Alexandro Fernandez Boreisha

## Abrir el archivo 
library(readx1)
datos <- read_excel("viviendasChamberi.csv")
##Sacamos una muestra de población de 100 individuos de los que
## conocemos el precio y el numero de habitaciones de su inmueble en la zona de Chamberí 
## y queremos observar, cual supera las expectativas, es decir, la media, y cual no,
## dado un nivel de confianza del 95% 

##Hacemos un resumen del archivo de datos
summary(viviendasChamberi)
zAlphaMedio <- 1.96 ## este dato lo sacamos de la tabla, al ser 95%, si quisieramos cambiarl solo habría que modificar el valor


##Sacamos una muestra de 100 pisos, de precio. 
muestraPrecio  <- viviendasChamberi[1:100,"Precio"]
##Hacemos un resumen de la variable precio
summary(muestraPrecio)

##Sacamos una muestra de 100 pisos,de numero de habitaciones
muestraHabitaciones <- viviendasChamberi[1:100,"Habitaciones"]
##Hacemos un resumen de la variable habitaciones
summary(muestraHabitaciones)

##Guardamos la longitud del espacio muestral (es el mismo para ambos)
n <- length(unlist(muestraPrecio))
#Mostramos
n

##Calculamos la media del precio
mediaPrecio <- mean(unlist(muestraPrecio))
#Mostramos
mediaPrecio
##Calculamos la media del numero de habitaciones
mediaHabitaciones <- mean(unlist(muestraHabitaciones))
#Mostramos
mediaHabitaciones

##Calculamos la desviación tipica del precio 
desviacionPrecio <- sd(unlist(muestraPrecio))
#Mostramos
desviacionPrecio
##Calculamos la desviación tipica del numero de habitaciones
desviacionHabitaciones <- sd(unlist(muestraHabitaciones))
#Mostramos
desviacionHabitaciones

##Calculamos el error estandar del precio 
errorPrecio <- desviacionPrecio/sqrt(n)
#Mostramos
errorPrecio

##Calculamos el error estandar del numero de habitaciones
errorHabitacion <- desviacionHabitaciones/sqrt(n)
#Mostramos
errorHabitacion

##Calculamos el limite inferior del precio
limiteInferiorPrecio <- mediaPrecio - (zAlphaMedio * errorPrecio)
#Mostramos
limiteInferiorPrecio

##Calculamos el limite inferior del numero de habitaciones
limiteInferiorHabitacion <- mediaHabitaciones - (zAlphaMedio * errorHabitacion)
#Mostramos
limiteInferiorHabitacion

##Calculamos el limite superior del precio
limiteSuperiorPrecio <- mediaPrecio + (zAlphaMedio * errorPrecio)
#Mostramos
limiteSuperiorPrecio

##Calculamos el limite superior del numero de habitaciones 
limiteSuperiorHabitacion <- mediaHabitaciones + (zAlphaMedio * errorHabitacion)
#Mostramos
limiteSuperiorHabitacion

##Vamos a crear el intervalo una vez calculados los limites
## Podremos observar después, los datos que queremos contrastar del enunciado
intervaloPrecio <- data.frame(n,mediaPrecio,desviacionPrecio,zAlphaMedio,errorPrecio,limiteInferiorPrecio,limiteSuperiorPrecio)

##Mostramos
intervaloPrecio

##Vamos a crear el intervalo una vez calculados los limites
intervaloHabitacion <- data.frame(n,mediaHabitaciones,desviacionHabitaciones,zAlphaMedio,errorHabitacion,limiteInferiorHabitacion,limiteSuperiorHabitacion)

#Mostramos 
intervaloHabitacion 

##Consideramos una vez obtenidos los resultados
## que los pisos por debajo del limite inferior del intervalo del precio, se situan aún así cerca de la media. 
## .Además, podemos afirmar por el intervalo de las habitaciones
## que estos contaran solo con 1 o menos cuartos. Por otra parte, 
## los pisos que superan el limite superior del intervalo del precio
## contarán con 2 habitaciones. Por lo que se puede afirmar, que por lo general 
## los pisos analizados de Chamberí contarán la mayoría con 1 cuarto y la mayoría, 
## no superarán los 1300 euros, situandose muy cercanos a la media. 

## Por tanto, observamos de forma general, que no va a haber una gran variación
## respecto a la media en ninguno de los 2 casos.


## Ahora, realizaremos un analisis similar, pero con la superficie de los inmuebles.
## Para ello contaremos con un nivel de confianza del 95 % también y con un tamaño 
## muestra de 100. 

##Sacamos una muestra de 100 pisos, de superficie.
muestraSuperficie <- viviendasChamberi[1:100,"Superficie"]
##Hacemos un resumen de la variable superficie
summary(muestraSuperficie)

##Calculamos la media de la superficie
mediaSuperficie <- mean(unlist(muestraSuperficie))
##Mostramos
mediaSuperficie

##Calculamos la desviación típica de la superficie
desviacionSuperficie <- sd(unlist(muestraSuperficie))
##Mostramos
desviacionSuperficie

##Calculamos el error estandar de la superficie
errorSuperficie <- desviacionSuperficie / sqrt(n)
##Mostramos
errorSuperficie

##Calculamos el limite inferior de la superficie 
limiteInferiorSuperficie <- mediaSuperficie - (zAlphaMedio * errorSuperficie)
##Mostramos
limiteInferiorSuperficie

##Calculamos el limite superior del numero de la superficie
limiteSuperiorSuperficie <- mediaSuperficie + (zAlphaMedio * errorSuperficie)
##Mostramos
limiteSuperiorSuperficie

##Vamos a crear el intervalo una vez calculados los limites
intervaloSuperficie <- data.frame(n,mediaSuperficie,desviacionSuperficie,zAlphaMedio,errorSuperficie,limiteInferiorSuperficie,limiteSuperiorSuperficie)
##Mostramos
intervaloSuperficie

## Tras realizar el analisis en la variable superficie, observamos que todos los pisos
## tienen un tamaño estandar, es decir, todos los habitaculos van a tener practicamente la misma superficie
## teniendo una diferencia entre ellos de aproximadamente de 7 m^2, lo que nos asegura
## que no se vaya a contar con otra habitacion, pero si, que haya una diferencia entre ellas
## en cuanto a amplitud. Todo ello, estando muy relacionado con la variación en el precio
## y con el hecho de que la mayoría de pisos cuenten con 1 solo cuarto. 
## Además, al igual que en los otros casos observamos que los limite se encuentran muy cercanos  a la media
## habiendo aún así mayores diferencias. 


## Vamos a realizar ahora un contraste de hipotesis, relacionadas con la superficie.
## Y vamos a valorarla respecto a la media, utilizando un nivel de significacion
## del 5%.
## La inmobiliaria quiere asegurar que la superficie de los pisos será mayor o igual a
## 61 m^2, y, para ello vamos a utilizar el siguiente contraste de hipotesis: 

## La hipotesis nula -> H0 : mu >= 61
## La hipotesis alternativa -> H1 : mu < 61 

## Como nuestro tamaño muestral n = 100 >> 30; utilizaremos una aproximación 
## a la Normal. 

## A continuación, como la desviación tipica es conocida, utilizaremos la siguiente formula
## mediaSuperficie < 61 - zAlpha *(desviacionSuperficie/ sqrt(n))

## Vamos a insertar manualmente el valor de zAlpha
## con un nivel de significación del 5% 
 zAlpha <- 2.49  
 ##Mostramos
 zAlpha
 
 ##Manualmente introducimos mu0 
 muSubCero <- 61 
 ##Mostramos
 muSubCero
 
 ## Calculamos la parte de la derecha 
 parteDerecha <- zAlpha * (desviacionSuperficie/sqrt(n))
 ##Mostramos
 parteDerecha
 
##Una vez sabemos el resultado, lo comparamos con un if, y sabemos si podemos llegar 
## a tener razon, o hay que afirmar otra cosa. 
if(mediaSuperficie < muSubCero - parteDerecha){
  print("Se rechaza la hipotesis nula, por tanto la inmobiliaria no tiene razon")
  
} else {
  print("Se acepta la hipotesis nula, por tanto la inmobiliaria tiene razon")
}
 
 ## Ahora vamos a realizar otra hipotesis, en la que queremos observar
 ## De nuevo, la inmobiliaria quiere observar una serie de datos para poder
 ## publicitarse mejor, en este caso, quiere mostrar, respecto a la desviación tipica, que es 
 ## menor o igual a un determinado valor sigma0. 
 
 ## Por otro lado, el tamaño muestral sigue siendo el mismo porque se quiere 
 ## la mayor precisión posible. Utilizaremos 20 grados de libertad y un 
 ## nivel de significación del 5 % 
 
 ##  Las hipotesis serán las siguientes :
 ## La hipotesis nula -> H0 : sigma <= sigma0 
 ## La hipotesis alternativa -> H1 : sigma > sigma0 
 
 
 ## Para ello utilizaremos la siguiente formula : ((n - 1)s^2)/(sigma0^2) > jiCuadrado(n-1,alpha)
 ## Donde s^2 = 1/(n -1 ) *(sum(j = 1 -> n)((xj - mediaPrecio)^2))
 
 ## n grados de libertad, cogemos una nueva muestra 
 muestraNuevaPrecio <- viviendasChamberi[1:20,"Precio"]
 ## Mostramos
 summary(muestraNuevaPrecio)
 
 ## Introducimos sigma0 manualmente
 sigma0 <- 500 
 ## Mostramos
 sigma0
 
 ## Introducimos el valor manualmente (de la tabla)  
 jiCuadrado <-38.572
 ## Mostramos
 jiCuadrado  
 
 ## Calculamos la media de nuevo 
 mediaNuevaPrecio <- mean(unlist(muestraNuevaPrecio))
 ## Mostramos
 mediaNuevaPrecio
 
 ## Calculamos s^2, la cuasivarianza
 cuasiVarianzaPrecio <- var(muestraNuevaPrecio)
 ## Mostramos
 cuasiVarianzaPrecio
 
 ## Calculamos la parte izquierda 
 parteIzquierda <- ((n - 1)*cuasiVarianzaPrecio)/sigma0^2
 ## Mostramos
 parteIzquierda

 if(parteIzquierda > jiCuadrado){
   print("Se rechaza la hipotesis nula, por tanto la inmobiliaria no tiene razon")
 } else {
   print("Se acepta la hipotesis nula, por tanto la inmobiliaria tiene razon")
 } 
 ## En este caso, la inmobiliaria no tiene razon por lo que habría que ajustar 
 ## el precio de los pisos para que la diferencia no fuera tan grande y fuera mas 
 ## igualitario 