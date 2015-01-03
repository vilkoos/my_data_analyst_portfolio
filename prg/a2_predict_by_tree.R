
#---------- get the data ------------------------

load("samsungData.rda")
dat <- na.omit (samsungData)


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

train <- dat[dat$subject %in% train_set,]
test  <- dat[dat$subject %in% test_set ,]

#--------- make predictor (a tree)-------------- 

treeDat2 <- train[,-562] 
for (i in 1:561){ names(treeDat2)[i] <- paste("v",as.character(i), sep="") }

library(tree)
set.seed(7363)
tree2 <- tree(factor(activity) ~ . ,data=treeDat2)
summary(tree2) #Misclassification error rate: 0.08436 = 281 / 3331 
plot(tree2)
text(tree2)

#----- make nice plot for paper -------------

plot(tree2)
text(tree2)
text(main = "figure 1: tree used as predictor")

names(dat)[41]
names(dat)[42]
names(dat)[53]
names(dat)[54]
names(dat)[58]
names(dat)[70]
names(dat)[53]
names(dat)[202]
names(dat)[303]
names(dat)[348]
names(dat)[560]
"names(dat)[41]
[1] tGravityAcc-mean()-X
> names(dat)[42]
[1] tGravityAcc-mean()-Y
> names(dat)[53]
[1] tGravityAcc-min()-X
> names(dat)[54]
[1] tGravityAcc-min()-Y
> names(dat)[58]
[1] tGravityAcc-energy()-Y
> names(dat)[70]
[1] tGravityAcc-arCoeff()-Y,1
> names(dat)[53]
[1] tGravityAcc-min()-X
> names(dat)[202]
[1] tBodyAccMag-std()
> names(dat)[303]
[1] fBodyAcc-bandsEnergy()-1,8
> names(dat)[348]
[1] fBodyAccJerk-std()-X
> names(dat)[560]
[1] angle(Y,gravityMean)"

#------- use predictor to predict trainset -----------------------

tree2.pred <- predict(tree2,treeDat2)
inp <- table(treeDat2$activity,predict(tree2,type="class"))
print(inp)
"        laying sitting standing walk walkdown walkup
  laying      612       0        0    0        0      0
  sitting       0     498       57    0        0      0
  standing      0     127      494    0        0      0
  walk          0       0        0  569       10      9
  walkdown      0       0        0   20      415     26
  walkup        0       0        0   16       16    462"

outp = c(0,0,0,0,0,0)
for (n in 1:6){ outp[n] = round(inp[n,n]/sum(inp[,n]),2)*100 }
print(outp)
"[1] 100  80  90  94  94  93"

#------- use predictor to predict testset ---------

testDat <- test[,-562]
for (i in 1:561){ names(testDat)[i] <- paste("v",as.character(i), sep="") }

pred <- predict(tree2,testDat,type="class")
table(testDat$activity,predict(tree2,type="class"))
inp <- table(testDat$activity,pred)
print(inp)
"        pred
           laying sitting standing walk walkdown walkup
  laying      795       0        0    0        0      0
  sitting      15     660       56    0        0      0
  standing      0      58      695    0        0      0
  walk          0       0        9  516       17     96
  walkdown      0       2        0   14      385    124
  walkup        0       0       38   99       40    402"

outp = c(0,0,0,0,0,0)
for (n in 1:6){ outp[n] = round(inp[n,n]/sum(inp[,n]),2)*100 }
print(outp)
"98 92 87 82 87 65"

#------- use predictor to predict testsubject 11 ---------

testSubj = 11

test0 <- test[test$subject == testSubj,]
testDat <- test0[,-562]
for (i in 1:561){ names(testDat)[i] <- paste("v",as.character(i), sep="") }

pred <- predict(tree2,testDat,type="class")
#table(testDat$activity,predict(tree2,type="class"))
inp <- table(testDat$activity,pred)
print(inp)
"         pred
           laying sitting standing walk walkdown walkup
  laying       57       0        0    0        0      0
  sitting       0      53        0    0        0      0
  standing      0       0       47    0        0      0
  walk          0       0        0   59        0      0
  walkdown      0       0        0    0       43      3
  walkup        0       0        0    0        0     54"

outp = c(0,0,0,0,0,0)
for (n in 1:6){ outp[n] = round(inp[n,n]/sum(inp[,n]),2)*100 }
print(outp)
"[1] 100 100 100 100 100  95"

#------- use predictor to predict testsubject 14 ---------

testSubj = 14

test0 <- test[test$subject == testSubj,]
testDat <- test0[,-562]
for (i in 1:561){ names(testDat)[i] <- paste("v",as.character(i), sep="") }

pred <- predict(tree2,testDat,type="class")
inp <- table(testDat$activity,pred)
print(inp)
"        pred
           laying sitting standing walk walkdown walkup
  laying       51       0        0    0        0      0
  sitting       0      38       16    0        0      0
  standing      0       0       60    0        0      0
  walk          0       0        1    7        0     51
  walkdown      0       0        0    0       45      0
  walkup        0       0        0    0        4     50"

outp = c(0,0,0,0,0,0)
for (n in 1:6){ outp[n] = round(inp[n,n]/sum(inp[,n]),2)*100 }
print(outp)
"[1] 100 100  78 100  92  50"

#------- use predictor to predict testsubject 15 ---------
#       and 11,14,15,19,21,25,26 to test set

testSubj = 15

test0 <- test[test$subject == testSubj,]
testDat <- test0[,-562]
for (i in 1:561){ names(testDat)[i] <- paste("v",as.character(i), sep="") }

pred <- predict(tree2,testDat,type="class")
inp <- table(testDat$activity,pred)
print(inp)
"        pred
           laying sitting standing walk walkdown walkup
  laying       72       0        0    0        0      0
  sitting       0      59        0    0        0      0
  standing      0       2       51    0        0      0
  walk          0       0        0   54        0      0
  walkdown      0       0        0    0       42      0
  walkup        0       0        0    5        3     40"

outp = c(0,0,0,0,0,0)
for (n in 1:6){ outp[n] = round(inp[n,n]/sum(inp[,n]),2)*100 }
print(outp)
"[1] 100  97 100  92  93 100"

#------- use predictor to predict testsubject 19 ---------
#       and 11,14,15,19,21,25,26 to test set

testSubj = 19

test0 <- test[test$subject == testSubj,]
testDat <- test0[,-562]
for (i in 1:561){ names(testDat)[i] <- paste("v",as.character(i), sep="") }

pred <- predict(tree2,testDat,type="class")
inp <- table(testDat$activity,pred)
print(inp)
"        pred
           laying sitting standing walk walkdown walkup
  laying       83       0        0    0        0      0
  sitting      15      58        0    0        0      0
  standing      0       0       73    0        0      0
  walk          0       0        0   28       17      7
  walkdown      0       0        0    0       39      0
  walkup        0       0        0    9        1     30"

outp = c(0,0,0,0,0,0)
for (n in 1:6){ outp[n] = round(inp[n,n]/sum(inp[,n]),2)*100 }
print(outp)
"[1]  85 100 100  76  68  81"

#------- use predictor to predict testsubject 21 ---------
#       and 11,14,15,19,21,25,26 to test set

testSubj = 21

test0 <- test[test$subject == testSubj,]
testDat <- test0[,-562]
for (i in 1:561){ names(testDat)[i] <- paste("v",as.character(i), sep="") }

pred <- predict(tree2,testDat,type="class")
inp <- table(testDat$activity,pred)
print(inp)
"        pred
           laying sitting standing walk walkdown walkup
  laying       90       0        0    0        0      0
  sitting       0      83        2    0        0      0
  standing      0       0       89    0        0      0
  walk          0       0        0   42        0     10
  walkdown      0       0        0    0        0     45
  walkup        0       0        0    0        0     47"

outp = c(0,0,0,0,0,0)
for (n in 1:6){ outp[n] = round(inp[n,n]/sum(inp[,n]),2)*100 }
print(outp)
"[1] 100 100  98 100 NaN  46"

#------- use predictor to predict testsubject 25 ---------
#       and 11,14,15,19,21,25,26 to test set

testSubj = 25

test0 <- test[test$subject == testSubj,]
testDat <- test0[,-562]
for (i in 1:561){ names(testDat)[i] <- paste("v",as.character(i), sep="") }

pred <- predict(tree2,testDat,type="class")
inp <- table(testDat$activity,pred)
print(inp)
"         laying sitting standing walk walkdown walkup
  laying       73       0        0    0        0      0
  sitting       0      65        0    0        0      0
  standing      0       0       74    0        0      0
  walk          0       0        8   50        0     16
  walkdown      0       2        0    5        1     50
  walkup        0       0       34   25        0      6"

outp = c(0,0,0,0,0,0)
for (n in 1:6){ outp[n] = round(inp[n,n]/sum(inp[,n]),2)*100 }
print(outp)
"[1] 100  97  64  62 100   8"


#------- use predictor to predict testsubject 26 ---------
#       and 11,14,15,19,21,25,26 to test set

testSubj = 26

test0 <- test[test$subject == testSubj,]
testDat <- test0[,-562]
for (i in 1:561){ names(testDat)[i] <- paste("v",as.character(i), sep="") }

pred <- predict(tree2,testDat,type="class")
inp <- table(testDat$activity,pred)
print(inp)
"        pred
           laying sitting standing walk walkdown walkup
  laying       76       0        0    0        0      0
  sitting       0      78        0    0        0      0
  standing      0       1       73    0        0      0
  walk          0       0        0   53        0      6
  walkdown      0       0        0    0       50      0
  walkup        0       0        0   10        4     41"

outp = c(0,0,0,0,0,0)
for (n in 1:6){ outp[n] = round(inp[n,n]/sum(inp[,n]),2)*100 }
print(outp)
"[1] 100  99 100  84  93  87"

#----------- all in one --------------------

dif <- matrix(rep.int(0,66),nrow=11,ncol=6)
tel = 1
#soll = c(98, 92, 87, 82, 87, 65)
soll = c(100, 80, 90, 94, 94, 93)
for (s in test_set) { 
  test0 <- test[test$subject == s,]
  testDat <- test0[,-562]
  for (i in 1:561){ names(testDat)[i] <- paste("v",as.character(i), sep="") }  
  pred <- predict(tree2,testDat,type="class")
  inp <- table(testDat$activity,pred)
  outp = c(0,0,0,0,0,0)
  for (n in 1:6){ outp[n] = round(inp[n,n]/sum(inp[,n]),2)*100 }
  #print(outp)
  for (i in 1:6) { dif[tel,i] <- outp[i] - soll[i] } 
  tel = tel + 1
}
diff_test_set <- data.frame(dif)

#----------- all in one on train subjects--------------------

dif <- matrix(rep.int(0,60),nrow=10,ncol=6)
tel = 1
#soll = c(98, 92, 87, 82, 87, 65)
soll = c(100, 80, 90, 94, 94, 93)
for (s in train_set) { 
  test0 <- train[train$subject == s,]
  testDat <- test0[,-562]
  for (i in 1:561){ names(testDat)[i] <- paste("v",as.character(i), sep="") }  
  pred <- predict(tree2,testDat,type="class")
  inp <- table(testDat$activity,pred)
  outp = c(0,0,0,0,0,0)
  for (n in 1:6){ outp[n] = round(inp[n,n]/sum(inp[,n]),2)*100 }
  #print(outp)
  for (i in 1:6) { dif[tel,i] <- outp[i] - soll[i] } 
  tel = tel + 1
}
diff_train_set <- data.frame(dif)



