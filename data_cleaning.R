########################################################
########################################################
########################################################
## Making sense of Prolific data

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

## write.csv(as.data.frame(cbind(x,y)), "codebook.csv")        ## Codebook for itemX <-> Survey item matching

use2 <- use
data <- data.frame(lapply(use, function(x) as.numeric(as.character(x))))

num <- nrow(data)


#######################################################
#######################################################
#######################################################
## Identifying iffy responses 10/8/21

use2$missing <- rowSums(is.na(use2[18:404])) 
hist(use2$missing)
descr::freq(use2$missing)

library(careless)
use2$careless_long <- longstring(use2[18:399])
hist(use2$careless_long)
descr::freq(use2$careless_long)


## STILL NEED FILE WITH BAD RESPONDENTS IDENTIFIED (E.G., Item_18=2, na > 200, longstring > 20, CARELESS CHECKS > 1)


invalid <- use2[ which(use2$item18 == 2), ]
careless <- use2[which(use2$careless_long > 20), ]
nas <- use2[which(use2$missing > 200), ]

invalid$reason <- "Did not accept invite"
careless$reason <- "Longstring over 20"
nas$reason <- "More than 200 NAs"

prolific_no <- as.data.frame(rbind(invalid, careless, nas))

different <- use2[ which(use2$item18 == 2 | use2$careless_long > 20 | use2$missing > 200), ]
write.csv(different[,c(1:2,405:406)], "ProlificFlagged.csv")
## Attention checks: 61 == 5; 145 == 5; 248 == 2; 308 == 3

attention <- use2[which(use2$item61 == 5 & use2$item145 == 5 & use2$item248 == 2 & use2$item308 == 3), ]
write.csv(attention[,c(1:2,405:406)], "ProlificPassed.csv")
# invalid <- data[ which(data$item18 == 2), ]
# write.csv(invalid, "dont_accepts.csv")
write.csv(invalid[,c(1:2, 405:406)], "ProlificRefused.csv")

######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
## Morgan's title script

#Job title
library(labourR)


#ESCO/ISCO classification using the labourR package
#ISCO = International Standard Classification of Occupations

use$id <- 1:nrow(use) #adds unique identifier to satisfy inane requirement of classify_occupation()

ISCO <- classify_occupation(use, text_col = 'item400', isco_level = 2) 

m3 <-merge(use, ISCO, by.x = "id", by.y = "id", all.x = TRUE)

descr::freq(m3$preferredLabel)



##### RENATA trying to change the classifications

use$id <- 1:nrow(use) #adds unique identifier to satisfy inane requirement of classify_occupation()

ISCO <- classify_occupation(use, text_col = 'item400', isco_level = 2) 

job_groups <- table(ISCO$iscoGroup)
as.numeric(ISCO$iscoGroup)

table(ISCO$iscoGroup)
hist(ISCO$iscoGroup)

#1 (Managers): 99
#2 (Professionals): 278
#3 (Technicians and associate professionals):184 
#4 (Clerical support workers):19
#5 (Service and sales workers): 26
#6 (Skilled agricultural, forest and fishery workers): 2
#7 (Craft and relate trades workers): 9
#8 (Plant and machine operators and assemblers):19
#9 (Elementary occupations): 3
#0 (Armed forces occupations): 0




# data[,c(1:404)] <- 1                             ## mock data to test script
chars <- as.data.frame(t(data[22:117]))            ## isolating characteristics

labels <- read.csv("codebook.csv")                 ## grabbing codebook for item labels

chars$tot <- rowMeans(chars[1:nrow(data)], na.rm=TRUE)
chars$label <- labels[c(22:117),3]

sortchars <- chars[order(-chars$tot),]

topchars <- head(sortchars, 10)
bottomchars <- tail(sortchars,10)

topchars <- topchars[tail(names(topchars),2)]         ## taking last two columns
topchars$label <- substr(topchars$label, 32,100)      ## shortening labels

papaja::apa_table(topchars, # apa contains the data.frame needed for apa_table
                  caption = "Top 10 work characteristics.",
                  align = c("m{1cm}","m{1cm}","m{14cm}"),
                  landscape = TRUE,
                  escape = F)

bottomchars <- bottomchars[tail(names(bottomchars),2)]         ## taking last two columns
bottomchars$label <- substr(bottomchars$label, 32,100)      ## shortening labels

papaja::apa_table(bottomchars, # apa contains the data.frame needed for apa_table
                  caption = "Bottom 10 work characteristics.",
                  align = c("m{1cm}","m{1cm}","m{14cm}"),
                  landscape = TRUE,
                  escape = F)

###############################################

resource <- as.data.frame(t(data[118:202]))            ## isolating characteristics

resource$tot <- rowMeans(resource[1:nrow(data)], na.rm=TRUE)
resource$label <- labels[c(118:202),3]

sortresource <- resource[order(-resource$tot),]

topresource <- head(sortresource, 10)
bottomresource <- tail(sortresource,10)

topresource <- topresource[tail(names(topresource),2)]         ## taking last two columns
topresource$label <- substr(topresource$label, 32,100)      ## shortening labels

papaja::apa_table(topresource, # apa contains the data.frame needed for apa_table
                  caption = "Top 10 work resources.",
                  align = c("m{1cm}","m{1cm}","m{14cm}"),
                  landscape = TRUE,
                  escape = F)

bottomresource <- bottomresource[tail(names(bottomresource),2)]         ## taking last two columns
bottomresource$label <- substr(bottomresource$label, 32,100)      ## shortening labels

papaja::apa_table(bottomresource, # apa contains the data.frame needed for apa_table
                  caption = "Bottom 10 work resources.",
                  align = c("m{1cm}","m{1cm}","m{14cm}"),
                  landscape = TRUE,
                  escape = F)

###############################################

hindrance <- as.data.frame(t(data[203:287]))            ## isolating characteristics

hindrance$tot <- rowMeans(hindrance[1:nrow(data)], na.rm=TRUE)
hindrance$label <- labels[c(203:287),3]

sorthindrance <- hindrance[order(-hindrance$tot),]

tophindrance <- head(sorthindrance, 10)
bottomhindrance <- tail(sorthindrance,10)

tophindrance <- tophindrance[tail(names(tophindrance),2)]         ## taking last two columns
tophindrance$label <- substr(tophindrance$label, 32,100)      ## shortening labels

papaja::apa_table(tophindrance, # apa contains the data.frame needed for apa_table
                  caption = "Top 10 work hindrances.",
                  align = c("m{1cm}","m{1cm}","m{14cm}"),
                  landscape = TRUE,
                  escape = F)

bottomhindrance <- bottomhindrance[tail(names(bottomhindrance),2)]         ## taking last two columns
bottomhindrance$label <- substr(bottomhindrance$label, 32,100)      ## shortening labels

papaja::apa_table(bottomhindrance, # apa contains the data.frame needed for apa_table
                  caption = "Bottom 10 work hindrances.",
                  align = c("m{1cm}","m{1cm}","m{14cm}"),
                  landscape = TRUE,
                  escape = F)

###############################################

challenge <- as.data.frame(t(data[288:372]))            ## isolating characteristics

challenge$tot <- rowMeans(challenge[1:nrow(data)], na.rm=TRUE)
challenge$label <- labels[c(288:372),3]

sortchallenge <- challenge[order(-challenge$tot),]

topchallenge <- head(sortchallenge, 10)
bottomchallenge <- tail(sortchallenge,10)

topchallenge <- topchallenge[tail(names(topchallenge),2)]         ## taking last two columns
topchallenge$label <- substr(topchallenge$label, 32,100)      ## shortening labels

papaja::apa_table(topchallenge, # apa contains the data.frame needed for apa_table
                  caption = "Top 10 work challenges.",
                  align = c("m{1cm}","m{1cm}","m{14cm}"),
                  landscape = TRUE,
                  escape = F)

bottomchallenge <- bottomchallenge[tail(names(bottomchallenge),2)]         ## taking last two columns
bottomchallenge$label <- substr(bottomchallenge$label, 32,100)      ## shortening labels

papaja::apa_table(bottomchallenge, # apa contains the data.frame needed for apa_table
                  caption = "Bottom 10 work challenges.",
                  align = c("m{1cm}","m{1cm}","m{14cm}"),
                  landscape = TRUE,
                  escape = F)

#######################################################
#######################################################
#######################################################
## Study 2 DVs

data$item387      <- 7- data$item387           ## only reflected engagement item

data$engage       <- rowMeans(data[380:399], na.rm=TRUE)

data$absorption   <- rowMeans(data[c(380:386)], na.rm=TRUE)
data$vigor        <- rowMeans(data[c(387:392)], na.rm=TRUE) 
data$dedication   <- rowMeans(data[c(393:399)], na.rm=TRUE) 

data$cognitive    <- rowMeans(data[c(380:382, 387:388, 393:395)], na.rm=TRUE)
data$affective    <- rowMeans(data[c(383:384, 389:390, 396:397)], na.rm=TRUE) 
data$behavioral   <- rowMeans(data[c(385:386, 391:392, 398:399)], na.rm=TRUE) 

######################################################

data$burnout      <- rowMeans(data[c(373:376)], na.rm=TRUE)
data$stress       <- rowMeans(data[c(377:379)], na.rm=TRUE)


r <- corx::corx(data[,405:413],                     ## can extend if needed
                triangle = "lower",
                stars = c(0.05, 0.01, 0.001),
                describe = c(`$M$` = mean, `$SD$` = sd))

papaja::apa_table(r$apa, # apa contains the data.frame needed for apa_table
                  caption = "Scale intercorrelations (outcome variables).",
                  note = "* p < 0.05; ** p < 0.01; *** p < 0.001",
                  landscape = TRUE,
                  escape = F)