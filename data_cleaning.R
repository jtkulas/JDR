########################################################
########################################################
########################################################
## Getting Qualtrics data into R - 9/21/21

temp <- read.csv("prolific.csv", header=FALSE, na.strings="")   ## NOTE: 404 vars (9/28/21) make sure to check indexing used throughout script

x <- paste("item", sep="",1:404)
y <- t(temp[2,])

data <- temp[-c(1:3),]                                      ## Getting rid of all 3 weird Qualtrics rows
colnames(data) <- x

rm(x, y, temp)                                                 ## decluttering Qualtrics excess
## write.csv(as.data.frame(cbind(x,y)), "codebook.csv")        ## Codebook for itemX <-> Survey item matching

num <- nrow(data)

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


r <- corx(data[,405:413],                     ## can extend if needed
          triangle = "lower",
          stars = c(0.05, 0.01, 0.001),
          describe = c(`$M$` = mean, `$SD$` = sd))

papaja::apa_table(r$apa, # apa contains the data.frame needed for apa_table
                  caption = "Scale intercorrelations (outcome variables).",
                  note = "* p < 0.05; ** p < 0.01; *** p < 0.001",
                  landscape = TRUE,
                  escape = F)

#######################################################
#######################################################
#######################################################
## Study 1 rankings

# data[,c(1:404)] <- 1                             ## mock data to test script
chars <- as.data.frame(t(data[22:117]))            ## isolating characteristics

labels <- read.csv("codebook.csv")                 ## grabbing codebook for item labels

chars$tot <- rowMeans(chars[1:nrow(data)], na.rm=TRUE)
chars$label <- labels[c(22:117),3]

sortchars <- chars[order(chars$tot),]

topchars <- head(sortchars, 10)
bottomchars <- tail(sortchars,10)

###############################################

resource <- as.data.frame(t(data[118:202]))            ## isolating characteristics

resource$tot <- rowMeans(resource[1:nrow(data)], na.rm=TRUE)
resource$label <- labels[c(118:202),3]

sortresource <- resource[order(resource$tot),]

topresource <- head(sortresource, 10)
bottomresource <- tail(sortresource,10)

###############################################

hindrance <- as.data.frame(t(data[203:287]))            ## isolating characteristics

hindrance$tot <- rowMeans(hindrance[1:nrow(data)], na.rm=TRUE)
hindrance$label <- labels[c(203:287),3]

sorthindrance <- hindrance[order(hindrance$tot),]

tophindrance <- head(sorthindrance, 10)
bottomhindrance <- tail(sorthindrance,10)



