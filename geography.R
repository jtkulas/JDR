
temp <- read.csv("initial_data_screen.csv", header=FALSE, na.strings="")   ## NOTE: 404 vars (9/28/21) make sure to check indexing used throughout script

x <- paste("item", sep="",1:404)
y <- t(temp[2,])
## decluttering Qualtrics excess
data <- temp[-c(1:3),]                                           ## Getting rid of all 3 weird Qualtrics rows
colnames(data) <- x

incomplete <- read.csv("inprogress.csv", header=FALSE, na.strings="")   ## NOTE: 404 vars (9/28/21) make sure to check indexing used throughout script

data2 <- incomplete[-c(1:2),-c(10,12,16,113,199,285,371,379,400, 406:411)]                                          

data3 <- as.data.frame(cbind(data2$V8,data2$V9,data2$V1,data2$V1,data2$V1,data2$V1,data2$V1,data2$V1,data2))

colnames(data3) <- x

rm(x, y, temp)       

use <- rbind(data,data3)


library(leaflet)

use$item14 <- as.numeric(as.character(use$item14))
use$item15 <- as.numeric(as.character(use$item15))

## create leaflet map
leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  setView(-98.35, 39.7,
          zoom = 4) %>%
  addTiles() %>%
  addCircles(data=use, lng = ~item15, lat = ~item14, weight = 10)
