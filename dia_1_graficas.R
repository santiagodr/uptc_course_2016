###Introduccion-repaso R y Graficas usando ggplot2
# Por Santiago David (2016)
# Curso Ecofisiologia y comportamiento en contexto evolutivo - UPTC - 2016

#####
#instalar paquetes
#install.packages("ggplot2")
#install.packages("gapminder")

#####
# Cargar paquetes
#####

library(gapminder)
library(ggplot2)

#####
## Cargar datos usando base de datos gapminder
#####

data(gapminder) #cargamos datos en ambiente R
head(gapminder) #variables  
str(gapminder) #estructura de la base de datos

### tres formas diferentes de graficar en R ###
NOTE### incluir las diferencias... 


# ggplot esta construido de forma que todas las graficas se puedan interpretar usando los mismos
# parametros

ggplot(data=gapminder, aes(x=lifeExp,y= gdpPercap,colour=continent))+
  geom_point()
ggplot(data=gapminder, aes(x=year,y= gdpPercap,colour=continent))+
  geom_point() + geom_line()
ggplot(data=gapminder, aes(x=year,y= lifeExp,by=country,colour=continent))+
  geom_point() + geom_line()
ggplot(data=gapminder, aes(x=year,y= lifeExp,by=country))+
  geom_point() + geom_line(aes(colour=continent))

### Life expectancy - Expectativa de vida
ggplot(data=gapminder, aes(x=lifeExp,y= gdpPercap,colour=continent))+
  geom_point()
ggplot(data=gapminder, aes(x=lifeExp,y= gdpPercap,colour=continent))+
  geom_point()+scale_y_log10() # cambiar escala de todos los datos
ggplot(data=gapminder, aes(x=lifeExp,y= gdpPercap,colour=continent))+
  geom_point()+scale_y_log10() + geom_smooth(method="lm")  # Adicionar regresion linear e intervalos de confianza
ggplot(data=gapminder, aes(x=lifeExp,y= gdpPercap,colour=continent))+
  geom_point()+scale_y_log10() + geom_smooth(method="lm",size=1.5) # aumentar tamaño de linea
ggplot(data=gapminder, aes(x=lifeExp,y= gdpPercap,colour=continent))+
  geom_point()+scale_y_log10() + geom_smooth(method="loess",size=1.5) # usando metodo diferente

ggplot(data=gapminder, aes(x=lifeExp,y= gdpPercap,colour=continent))+
  geom_point(colour="black",size=0.4,shape=3)+scale_y_log10() + geom_smooth(method="lm",size=1.5)  # cambiar el tamaño de puntos


## EXTRA = poblacion y expectativa de vida
ggplot(data = gapminder, aes(x=lifeExp, y=pop,colour=continent))+
  geom_point()+scale_y_log10()

##### Para borrar todo del ambiente de R #####
rm(list=ls())



### Ejercicio usando Datos de Anolis en Islas del Caribe


# 1- cargar base de datos de Anolis y explorar el tipo de variables en la matriz
# 2- seleccione dos variables morfologicas que usted piense NO estan ~100% correlacionadas (ej. SVLength y TailLentgh)
# 3- Grafique la relacion entre estas variables usando ggplot y coloreelas por isla
# 4- Ahora coloree por ecomorfotipos, que diferencias observa?
# 5- Agregue una linea de regresion a la grafica

### Anolis data ###

anolis<-read.csv("anolis.convergence.csv")
str(anolis)

ggplot(data=anolis, aes(x= SVLength,y= TailLength,colour=Island))+
  geom_point(size=2) + geom_smooth(method="lm", size=1.5)

ggplot(data=anolis, aes(x= SVLength,y= TailLength,colour=Ecomorph))+
  geom_point(size=2) + geom_smooth(method="lm", size=1.5)

#otras opciones
ggplot(data=anolis, aes(x= FemurLength,y= TibiaLength,colour=Ecomorph))+
  geom_point(size=2) + geom_smooth(method="lm", size=1.5)

ggplot(data=anolis, aes(x= HumerusLength,y= RadiusLength,colour=Ecomorph))+
  geom_point(size=2) + geom_smooth(method="lm", size=1.5)



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

#What does this imply about the body forms of species utilizing similar ecological resources?



############################
#Repaso Opcional

#crear base de datos simple con tres tipos de variables diferentes

perro<-c("selva","roska","guardian")
peso<-c(3.4,6,10.5)
bravo<-c(TRUE,FALSE,TRUE)

#unir las variables en una data frame
mascotas<-data.frame(perro,peso,bravo)

mascotas$peso[2]+mascotas$peso[3]

#cinco tipos de datos#
typeof(mascotas$peso[1])
typeof(1L)
typeof(1+1i)
typeof(TRUE)
typeof("banana")

todo<-c(3.14, "banano", TRUE)
typeof(todo)



