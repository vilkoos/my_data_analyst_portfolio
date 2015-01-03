
#---------- get the data ------------------------

load("samsungData.rda")
dat <- na.omit (samsungData)

#--------- scale the measurements ---------------

dat2 <- dat[,-c(562,563)]          # remove non numeric cols

dat3 <- as.data.frame(scale(dat2)) # scale the movement measures
dat3[,562] <- dat[,562]            # put back subject
dat3[,563] <- dat[,563]            # put back activity
names(dat3)[562] <- "subject"
names(dat3)[563] <- "activity"

#--------- split into train and test set -----------

#there are 21 sujects
#1   3   5   6   7   8  11  14  15  16  17  19  21  22  23  25  26  27  28  29  30 
# the first four 1,3,5,6 should be in the test set
# the last four 27, 28, 29, 30 should be in the test set
# fom the middel 13, 6 should be in the train set, the other 7 in the test set
# draw 6 random from the range 5:17
#set.seed(333)
#add_to_train = sample(5:17, 6, replace=FALSE)
# this gives 7,8,16,17,22,23 to train set 
#       and 11,14,15,19,21,25,26 to test set

train_set <- c(1,3,5,6,7,8,16,17,22,23)
test_set  <- c(11,14,15,19,21,25,26,27,28,29,30)

train <- dat3[dat3$subject %in% train_set,]
test  <- dat3[dat3$subject %in% test_set ,]

#--------- make predictor (a tree)-------------- 

library(class)
knn1 <- knn(train=train[,1:561], test=test[1,1:561], cl=train$activity, k=3)
summary(knn1)
table(train$activity)

#------- use predictor to predict trainset

nn = 10
outp = rep("x",nn)
for (i in 1:nn) { outp[i] <- knn(train[,1:561], test[i,1:561], cl=train$activity, k=5)}
table(train$activity[1:nn],outp[1:nn])


####################################################################################

#--------- do PCA on train set ----------------------

train2 <- train[,-c(562,563)] # remove non numeric cols

pca1 <- princomp(train2)    # do pca
summary(pca1)
plot(pca1)

loads <- loadings(pca1)[,1:20]

#--------- make PCAtrain set ----------------------

numTrain <- as.matrix(train[,1:561])

PCtrain <- data.frame(numTrain %*% loads)
PCtrain[,21] <- train[,562]            # put back subject
PCtrain[,22] <- train[,563]            # put back activity
names(PCtrain)[21] <- "subject"
names(PCtrain)[22] <- "activity"

#--------- make PCAtest set ----------------------

numTest <- as.matrix(test[,1:561])

PCtest <- data.frame(numTest %*% loads)
PCtest[,21] <- test[,562]            # put back subject
PCtest[,22] <- test[,563]            # put back activity
names(PCtest)[21] <- "subject"
names(PCtest)[22] <- "activity"

#------- use predictor to predict trainset

nn = 1000
outp = rep("x",nn)
for (i in 1:nn) { outp[i] <- knn(PCtrain[,1:20], PCtest[i,1:20], cl=PCtrain$activity, k=10)}
table(PCtrain$activity[1:nn],outp[1:nn])

