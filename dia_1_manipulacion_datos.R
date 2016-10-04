###### Manipulacion de datos usando dplyr

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