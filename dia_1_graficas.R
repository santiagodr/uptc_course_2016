######   Introduccion-repaso R y Graficas usando ggplot2

# Curso Ecofisiologia y comportamiento en contexto evolutivo - UPTC - 2016
# Por Santiago David 

###### Si no estan ya instalados #######
#instalar paquetes
#install.packages("ggplot2")
#install.packages("gapminder")
#install.packages("lattice")

#####
# Cargar paquetes
#####

library(gapminder)
library(ggplot2)
library(lattice)

#####
## Cargar datos usando base de datos gapminder

data(gapminder) #cargamos datos en ambiente R
head(gapminder) #variables  
str(gapminder) #estructura de la base de datos

### tres formas diferentes de graficar en R, cada una con diferente sintaxis ###

# 1 - Funciones Base
plot(x,y) 
plot(gapminder$lifeExp, gapminder$gdpPercap) # ejemplo usando funciones basicas

# 2 - Paquete para graficas "Lattice"
xyplot(y~x, data) # diferente sintaxis, argumentos se adicionan en parentesis
xyplot(gdpPercap~lifeExp, data = gapminder) # ejemplo usando lattice

# 3 - Paquete para graficas "ggplot2"
ggplot(data, aes(x,y) + geom_point()) 
# construido con la idea que todas las graficas se pueden expresar con el mismo set de parametros
# un set de datos
# un sistema de coordenadas
# un set de "geos" o la representacion grafica de los datos

ggplot(data = gapminder,
       aes(lifeExp, gdpPercap)) + geom_point() # ejemplo usando ggplot


##########
# Ejercicio 1 - Modifique el ejemplo de forma que se observe como "expectativa de vida" a cambiado a lo largo del tiempo



#la clave para entender ggplot esta en pensar en una figura como capas que se adicionan

# Otro ejemplo
ggplot(data = gapminder, 
       aes(x=year, y=lifeExp, 
           by=country)) +
  geom_line() # en este caso observamos como a cambiado la expectativa de vida en los ultimos 55 años, en este caso usando una capa "geom_line"

# En el ejemplo anterior, usamos la funcion "aes" para indicarle al tipo de grafica "geom", acerca 
# de la ubicacion de los puntos para "x" y "y". Otra propiedad que podemos modificar es el "color"

ggplot(data = gapminder,
       aes(year, lifeExp, 
           by=country, color=continent)) +
         geom_line() 

########### 
#Ejercicio 2 - que pasa si queremos observar ambos, puntos y lineas?



# es importante notar que en ggplot cada capa se adiciona sobre la anterior,
# en este caso los puntos sobre las lineas
# aqui podemos observarlo

ggplot(data = gapminder, 
       aes(x=year, y=lifeExp, 
           by=country)) +
  geom_line(aes(color=continent)) +
  geom_point()


########## 
# Ejercicio 3 - Cambie el orden de los puntos y las lineas en la grafica, que pasa en este caso?


### Tambien podemos adicionar otras capas como una linea de regresion o transformar los datos directamente en la grafica

ggplot(data=gapminder, aes(x=lifeExp,y= gdpPercap))+
  geom_point()+scale_y_log10() # cambiar escala de todos los datos

ggplot(data=gapminder, aes(x=lifeExp,y= gdpPercap))+
  geom_point()+scale_y_log10() + geom_smooth(method="lm")  # Adicionar regresion linear e intervalos de confianza

ggplot(data=gapminder, aes(x=lifeExp,y= gdpPercap,colour=continent))+
  geom_point()+scale_y_log10() + geom_smooth(method="lm")  # colores por continente

ggplot(data=gapminder, aes(x=lifeExp,y= gdpPercap,colour=continent))+
  geom_point()+scale_y_log10() + geom_smooth(method="lm",size=1.5) # aumentar tamaño de linea

ggplot(data=gapminder, aes(x=lifeExp,y= gdpPercap,colour=continent))+
  geom_point()+scale_y_log10() + geom_smooth(method="loess",size=1.5) # usando metodo diferente

ggplot(data=gapminder, aes(x=lifeExp,y= gdpPercap,colour=continent))+
  geom_point(colour="black",size=0.4,shape=3)+scale_y_log10() + geom_smooth(method="lm",size=1.5)  # cambiar el tamaño de puntos


rm(list=ls())

##########
# Ejercicio 2 - Usando datos de Anolis en el Caribe 
##########


# 1- cargar base de datos de Anolis y explorar el tipo de variables en la matriz
# 2- seleccione dos variables morfologicas que usted piense NO estan ~100% correlacionadas (ej. SVLength y TailLentgh)
# 3- Grafique la relacion entre estas variables usando ggplot y coloreelas por isla
# 4- Ahora coloree por ecomorfotipos, que diferencias observa?
# 5- Agregue una linea de regresion a la grafica





######################

### Opcional ###

### Analisis ecomorfologias Anolis usando analisis multivariado y grafica

acp<-prcomp(anolis[,4:13]) #analisis componentes principales indicando que columnas incluir

summary(acp) #Cual es el porcentaje de varianza en datos explicada por el primer componente?, por el segundo?

print(acp) #permite ver los "eigenvalues"
plot(acp,type="lines")
acp$rotation

#Guardar valores de los primeros dos componentes principales en una dataframe
xmat<-predict(acp)
valores<-as.data.frame(xmat,stringAsFactors=FALSE)
valores[,c(1,2)]

#pegar esos valores para cada especie en la base de datos original
acpvalores<-cbind(valores[,c(1,2)])
attach(anolis)
anolis$PC1<-acpvalores$PC1
anolis$PC2<-acpvalores$PC2
detach(anolis)

#graficar primeros dos componentes principales para visualizar las ecomorfologias

is.character(anolis$Ecomorph)
typeof(anolis$Ecomorph)
anolis$Ecomorph<-as.character(anolis$Ecomorph) # tenemos que convertir variable a categorica
anolis$Island<-as.character(anolis$Island)

plot(anolis$PC1,anolis$PC2,pch = anolis$Island)
plot(anolis$PC1,anolis$PC2,pch = anolis$Ecomorph)

ggplot(data = anolis, aes(x= PC1, y= PC2, colour= Island))+
  geom_point(size=2)
ggplot(data = anolis, aes(x= PC1, y= PC2, colour= Ecomorph))+
  geom_point(size=3)

# con base en la grafica, que se puede deducir de las morfologias de diferentes especies utilizando recursos ecologicos similares?


