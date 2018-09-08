## Feature Selection in R using Boruta 
## Step 1 lets Import Libraries 
#install.packages("Boruta") -- Install the package
#install.packages("mlbench")
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)
#----------------------------#
#data
inp<-Sonar
str(inp)

##set seed -- this is to repeat randomness 
set.seed(123)
boruta<-Boruta(Class ~ . ,data=inp,doTrace=2,maxRuns=600)
print(boruta)

#> print(boruta)
#Boruta performed 599 iterations in 2.84675 mins.
#34 attributes confirmed important: V1, V10, V11, V12, V13 and 29 more;
#20 attributes confirmed unimportant: V14, V24, V25, V29, V3 and 15
#more;
#6 tentative attributes left: V2, V30, V34, V39, V59 and 1 more;

## The above gives the required variables 

#--------Now let's plot --------#

plot(boruta)
plotImpHistory(boruta)


### Now lets move to tentative fix 
bor<-TentativeRoughFix(boruta)
print(bor)

## we can use att stats to give us mean etc 
attStats(boruta)


#### now let's run the Random forest and test the results

## Data partition
set.seed(143)
 dat<-sample(2,nrow(inp),replace=TRUE,prob = c(0.75,0.25))
 train<-inp[dat==1,]
 test<-inp[dat==2,]
 
 ## implementation of random forest
 rf75<-randomForest(Class ~ .,data=train)
 rf75
 summary(rf75)
 
 
 ## predict
 p<-predict(rf75,test)
 confusionMatrix(p,test$Class)
 
 ### got 81.36 % accuracy , this is by using all attributes
 
#--- now lets use only the important variables suggested by Baruta
 
getNonRejectedFormula(boruta)
## Class ~ V1 + V2 + V4 + V5 + V8 + V9 + V10 + V11 + V12 + V13 + 
#V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V26 + 
#  V27 + V28 + V30 + V31 + V32 + V34 + V35 + V36 + V37 + V39 + 
#  V43 + V44 + V45 + V46 + V47 + V48 + V49 + V51 + V52 + V59
#--------------------------------------------------------------#

##  we got the above formula (confirm + tentitative)

## again run random forest
set.seed(123)
rf40<- randomForest(Class ~ V1 + V2 + V4 + V5 + V8 + V9 + V10 + V11 + V12 + V13 + 
                      V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V26 + 
                      V27 + V28 + V30 + V31 + V32 + V34 + V35 + V36 + V37 + V39 + 
                      V43 + V44 + V45 + V46 + V47 + V48 + V49 + V51 + V52 + V59 ,
                    data= train )

p2<-predict(rf40,test)
confusionMatrix(p2,test$Class)

### we got same accuracy , now lets try after removing tentative fix 

getConfirmedFormula(boruta)
##Class ~ V1 + V4 + V5 + V9 + V10 + V11 + V12 + V13 + V15 + V16 + 
## V17 + V18 + V19 + V20 + V21 + V22 + V23 + V26 + V27 + V28 + 
#  V31 + V32 + V35 + V36 + V37 + V43 + V44 + V45 + V46 + V47 + 
 # V48 + V49 + V51 + V52
set.seed(121)
rf34<-randomForest(Class ~ V1 + V4 + V5 + V9 + V10 + V11 + V12 + V13 + V15 + V16 + 
                     V17 + V18 + V19 + V20 + V21 + V22 + V23 + V26 + V27 + V28 + 
                     V31 + V32 + V35 + V36 + V37 + V43 + V44 + V45 + V46 + V47 + 
                     V48 + V49 + V51 + V52 ,
                   data=train )
 p3<-predict(rf34,test)
 confusionMatrix(p3,test$Class)
 
 
 ### woah there is an super increase in the accuracy 
 