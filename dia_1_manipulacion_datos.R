###### Manipulacion de datos usando dplyr y tidyr

# Curso Ecofisiologia y comportamiento en contexto evolutivo - UPTC - 2016
# Por Santiago David

###### Instalar paquetes
#install.packages("dplyr", dependencies = TRUE)
#install.packages("tidyr", dependencies = TRUE)

####################################
# Cargar paquetes
####################################

library(gapminder)
library(dplyr)
library(tidyr)
library(ggplot2)

###################
## Cargar datos usando base de datos gapminder
####################

data(gapminder)
head(gapminder)   


# Explicar uso de "pipes" o tubos %>% 

# La implementacion de "pipes" en R permite que argumentos funcionales sean pasados a funciones en una especie de tuberia
# algunas ventajas son:
# menor necesidad de parentesis anidados
# orden de las funciones mas claro, ya que se leen en una direccion (izq a derecha)
# organizacion del codigo en general se mejora
# Basicamente le dice a R que tome lo que esta a la izquierda del "pipe" y lo pase a la derecha como un argumento

# ejemplo por http://blog.revolutionanalytics.com/2014/07/magrittr-simplifying-r-code-with-pipes.html

hourly_delay <- filter(summarise(group_by(filter(flights, !is.na(dep_delay)), date, hour), 
    delay = mean(dep_delay), n = n()), n > 10)

#Aqui tenemos el mismo codigo, pero en lugar de anidar una funcion en otra, los datos se pasan de una a otra con el operador "%>%"

hourly_delay <- flights %>% 
filter(!is.na(dep_delay)) %>% 
group_by(date, hour) %>% 
summarise( 
delay = mean(dep_delay), 
n = n() ) %>% 
filter(n > 10)

# los paquetes "dplyr" y "tidyr" se benefician mucho del uso de este operador...

# "dplyr" utiliza como base 5 verbos que reflejan la mayoria de manipulacion que queremos hacer de una base de datos

#########################
## Select - permite seleccionar ciertas columnas de los datos
#########################

select(gapminder, continent)

gapminder %>%
  select(continent)

gapminder %>%
  select(-continent)

gapminder %>%
 select(country:lifeExp)

# se pueden combinar otras funciones utiles como "contains()", "starts_with()", "ends_with()"

#########################
## Filter - permite seleccionar subconjuntos de datos por filas, usando cualquier funcion logica valida
#########################

gapminder %>% 
  filter(year == 2002)

gapminder %>%
  filter(continent =='Europe',
         year==1987)

gapminder %>%
  select(lifeExp)%>%
  filter(lifeExp > 40)

gapminder %>%
  filter(continent == "Africa") %>%
  select(country, year, lifeExp) 

# el simbolo "|" significa 'or' en R
gapminder %>%
  filter(continent=="Europe"|continent=="Asia")

# el simbolo "&" significa "and" en R
gapminder %>%
  filter(year>=1987&year<=2002)

#########################
## Arrange - permite ordenar las filas por una o mas columnas en orden ascendente o descendente
#########################

gapminder %>% 
  arrange(gdpPercap)

gapminder %>% 
  arrange(desc(lifeExp))

#########################
## Mutate - permite crear nuevos elementos o columnas e incluso usarlos en la misma funcion
#########################

gapminder %>%
  mutate(gdp_total = gdpPercap * pop,
         gdp_billion = gdp_total/ 10^9) 
  
#########################
## Summarise - permite resumir los datos y calcular estadisticas, es mas util cuando se combina con "group_by"
#########################

gapminder %>%
  group_by(continent) %>%
  summarize(mean = mean(lifeExp))

gapminder %>% 
  group_by(continent, year) %>%
  summarize(mean=mean(lifeExp))


### Ejercicio 1 - Cual es la media  de expectativa de vida y media poblacional para cada pais en America en el año 1987



##### "gather" y "spread"

# crear una pequeña base de datos
A <- data.frame(
  c1 = c('A', 'A', 'A', 'A', 'A', 'B', 'B'),
  c2 = c('a', 'a', 'a', 'b', 'b', 'c', 'd'),
  c3 = c(1, 3, 1, 1, 2, 2, 1))

B <- A %>%  
  group_by(c1,c2)%>%
  summarize(total = sum(c3), 
            number = n())  #transformar la base de datos preservando la informacion

# podemos tomar un factor y dispersar "spread" los niveles en columnas 
C <- B %>%
  select(-total) %>%
  spread(c2, number) 

# podemos hacer lo inverso a "spread"
# crear una nueva variable para unificar/colectar los datos 

C %>%
  gather(V1, V2, a:d) # can also use a,b,c,d or -c1


##### practicar usando base de datos
gap_wide <- read.csv("gapminder_wide.csv")

### Ejercicio: examine la base de datos... cual puede ser una forma de simplificar esta base de datos?

# poner columnas similares en una sola columna
gaplong <- gap_wide %>%
  gather(obstype_year, obs_values, 
         starts_with("gdpPercap"), 
         starts_with("pop"),
         starts_with("lifeExp"))%>%
  View

# separar informacion de una sola columna en dos variables
head(gaplong)

gaplong_separate <- gaplong %>%
  separate(obstype_year, into = c("obstype", "year"))

# Ejercicio 2: usando gaplong, calcule la media de expectativa de vida para cada continente


