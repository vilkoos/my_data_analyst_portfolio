
#---------- get the data ------------------------

load("samsungData.rda")
dat <- na.omit (samsungData)
#7353 obs of 563 vars, there were no NA's

#-------- inspect range of values of each variable -----------------


#names(dat)[562] = "subject" 
"the subject carrying the phone
there are 21 sujects
  1   3   5   6   7   8  11  14  15  16  17  19  21  22  23  25  26  27  28  29  30 
347 341 302 325 308 281 316 323 328 366 368 360 408 321 372 409 392 376 382 344 383
"

#names(dat)[563] = "activity"
" The activity preformend
there are 6 activities:
  laying  sitting standing     walk walkdown   walkup 
    1407     1286     1374     1226      986     1073 
"

for (i in 1:561) {
   xmax  <- max (dat[,i])
   xmin  <- min (dat[,i])
   xmean <- mean(dat[,i])
   out <- c(xmax, round(xmean,1), xmin)
   print(format(out,digits=1,width=10))
}
" The vars 1:561 have vaules -1<= val <= 1
Note that the mean is not always zero
"

"
table(dat[,562],dat[,563])
    
     laying sitting standing walk walkdown walkup
  1      50      47       53   95       49     53
  3      62      52       61   58       49     59
  5      52      44       56   56       47     47
  6      57      55       57   57       48     51
  7      52      48       53   57       47     51
  8      54      46       54   48       38     41
  11     57      53       47   59       46     54
  14     51      54       60   59       45     54
  15     72      59       53   54       42     48
  16     70      69       78   51       47     51
  17     71      64       78   61       46     48
  19     83      73       73   52       39     40
  21     90      85       89   52       45     47
  22     72      62       63   46       36     42
  23     72      68       68   59       54     51
  25     73      65       74   74       58     65
  26     76      78       74   59       50     55
  27     74      70       80   57       44     51
  28     80      72       79   54       46     51
  29     69      60       65   53       48     49
  30     70      62       59   65       62     65
"

#--------- do exploaratory principal component analysis -----------

dat2 <- dat[,-c(562,563)] # remove non numeric cols

pca1 <- princomp(dat2)    # do pca
summary(pca1)
plot(pca1)

loads <- loadings(pca1)[,1:7]

#--------- try tree -----------------------------------

dat3 <- dat[,-562] # remove subject from dataset

#treeDat2 <- dat3[,-562] 
treeDat2 <- dat3 
for (i in 1:561){ names(treeDat2)[i] <- paste("var",as.character(i), sep="") }

names(treeDat2)[562]

library(tree)
tree1 <- tree( factor(activity) ~ ., data=treeDat2)
summary(tree1)

plot(tree1)
text(tree1)


dat4 <- dat3
for (i in 1:561){ names(dat4)[i] <- paste("var",as.character(i), sep="") }
names(dat4)

dat4 <- na.omit(dat4)

library(tree)
#tree1 <- tree(activity ~., data=dat4, method="model.frame")
tree1 <- tree(factor(activity) ~., data=dat4)
summary(tree1)

plot(tree1)
text(tree1)


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
set.seed(333)
add_to_train = sample(5:17, 6, replace=FALSE)
# this gives 7,8,16,17,22,23 to test set 
#       and 11,14,15,19,21,25,26 to test set

train_set <- c(1,3,5,6,7,8,16,17,22,23)
test_set  <- c(11,14,15,19,21,25,26,27,28,29,30)

train <- dat3[dat3$subject %in% train_set,]
test  <- dat3[dat3$subject %in% test_set ,]

table(train$subject)
"  1   3   5   6   7   8  16  17  22  23 
347 341 302 325 308 281 366 368 321 372 "
table(test$subject)
"11  14  15  19  21  25  26  27  28  29  30 
316 323 328 360 408 409 392 376 382 344 383"



