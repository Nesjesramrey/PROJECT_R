   #CARGA DE LIBRERIAS A OCUPAR

install.packages("moodeest")
library(moodeest)
library(dplyr)
library(waffle)
library(ggplot2)
library(class)
library(randomForest)
library(hrbrthemes)
library(magrittr)
library(ggwaffle)
library(tidyverse)


    #LECTURA DE ARCHIVO
getwd()
setwd("C:/Users/USER/Documents")
spotify <- read.csv("SpotifyFeatures.csv")
View(spotify)

    #REVISION DE CARACTERISTICAS DEL ARCHIVO EN GENERAL
dim(spotify)
summary(spotify)
unique(spotify)
head(spotify)
nrow(spotify)
ncol(spotify)
names(spotify)
str(spotify)

    #CAMBIO DE PARAMETRO Y AGREGAR COLUMNA PARA ANALIZAR POR MINUTOS LAS CANCIONES
spotify$minutes <- (spotify$duration_ms/1000)/60
unique(spotify$genre)
levels(spotify$genre)


      #ORDENAR POR 3 TIPOS DE GENERO
niveles.orden <- c('Movie','R&B','Alternative')
spotify$genre_orden <- factor(x = spotify$genre, levels = niveles.orden, ordered = TRUE)
spotify$genre_orden <- NULL



   #FUNCIONES
spotify %>% 
  select(track_name,popularity,minutes) %>%
  filter((popularity > 6) & (minutes > 4.5)) %>%
  head(5)

spotify %>%
  select(-genre) %>%
  head(5)

spotify %>% 
  select(starts_with('S')) %>% 
  head(5)


spotify %>% 
  arrange(minutes) %>% 
  head(5)

spotify %>% 
  arrange(desc(minutes)) %>% 
  head(5)



   #FUNCION AGRUPAR
spotify %>%
  group_by(genre) %>%
  count()

spotify %>% 
  group_by(genre) %>%
  summarise(Mean.minutes = mean(minutes),
            Median.minutes = median(minutes),
            Max.minutes = max(minutes),
            Min.minutes = min(minutes))




    #DETERMINAR CORRELACIONES Y PLOTS
x <- spotify$minutes
y <- spotify$duration_ms

spotify
plot(y,x, 
     main = "Grafico", 
     xlab = "Minutes", 
     ylab = "Pop")


spotify.1 <- data.frame(minutes = x, duration_ms = y)
summary(spotify.1)
str(spotify.1)

plot(table(t(spotify.1))) 

cor(x,y)

scatter.smooth(x=spotify.1$minutes, y=spotify.1$duration_ms, main ="Minutes and popularity")

cor(x,y)

modlin <- lm(minutes ~ popularity, data = spotify.1)

summary(modlin)

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(modlin, las = 1)




spotify %>% as.factor() %>% summary()

ggplot(data = spotify, aes(x = minutes, fill = popularity)) +
  geom_bar() +
  scale_fill_manual(values=c("#D69CE1", "#037ffc", "#03fc98","#03fc98")) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6),legend.position = "none") +
  xlab('minutos')+
  ylab("popularidad")



summ <- spotify %>% group_by(genre) %>% summarise(users = length(genre)) 
summ <- summ[order(-summ$users),]
summ %>% ggplot() +
  geom_bar(aes(x = reorder(genre, users), y = users, fill=reorder(genre, users)), stat = 'identity') 

p <- summ[1:10,] %>% ggplot() +
  geom_bar(aes(x = reorder(genre, users), y = users, fill=reorder(genre, users)), stat = 'identity') 
p

p <- p + coord_flip()
p




    #HACER DATAFRAMES PARA ANALISIS POR SEPARADO

spot2 <- spotify[ , c(1,2,3,5,8,19)]

view(spot2)
unique(spot2$popularity)
write.csv("spot2.csv")
summary(spot2)
unique(spot2$genre)



SPOT.POP <- spot2 %>% 
  select(genre,track_name,popularity,minutes) %>%
  filter((popularity > 40) & (minutes > 4.5))
view(SPOT.POP)



SPOT.POP %>% 
  ggplot( aes(x = popularity, y = minutes, colour=genre)) + 
  geom_point() +
  ggtitle('Relacion Popularidad minutos')+
  theme_minimal() 


install.packages("hexbin")
library(stat_binhex)
SPOT.POP %>% 
  ggplot( aes(x = popularity, y = minutes)) + 
  geom_hex() +
  ggtitle('Relacion popularidad y minutos')+
  theme_minimal() + 
  scale_fill_gradient(low = 'white', high = 'red')



SPOT.POP.num <- SPOT.POP [ , -c(1,2)]
m.cor <- cor(SPOT.POP.num)
cor.df <- melt(cor(SPOT.POP.num))

cor.df %>% ggplot(aes(X1,X2)) +  
  geom_tile(aes(fill = value)) +  
  ggtitle('Matriz de correlación')+ 
  scale_fill_gradient(low = 'blue', high = 'red') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

media.popularity <- mean(SPOT.POP$popularity)


ggplot(SPOT.POP, aes(popularity)) + 
  geom_histogram(bins = 23, 
                 colour = 'black', 
                 fill = 'green',
                 alpha = 0.6) + 
  geom_vline(xintercept=media.popularity, linetype="dashed", color = "red") + 
  ggtitle('Histogram for Texture Mean') + 
  labs(x = 'Popularity', y = 'Minutes')+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15))  


hist(SPOT.POP$popularity)

ggplot(SPOT.POP, aes(popularity)) + 
  geom_histogram()

