## Getting Qualtrics data into R - 9/21/21

temp <- read.csv("prolific.csv", header=FALSE, na.strings="")

x <- paste("item", sep="",1:404)
y <- t(temp[2,])

data <- temp[-c(1:3),]                                      ## Getting rid of all 3 weird Qualtrics rows
colnames(data) <- x

## write.csv(as.data.frame(cbind(x,y)), "codebook.csv")        ## Codebook for itemX <-> Survey item matching





num <- nrow(data)