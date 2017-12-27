##104p ?з? ???? ?????ϱ?

spamD <- read.table(file.choose(), header=T, sep='\t')
table(spamD$rgroup)
spamTrain <- subset(spamD, spamD$rgroup>=10)
spamTest <- subset(spamD, spamD$rgroup<10) # 10개 이하로 테스트 데이터
spamVars <- setdiff(colnames(spamD), list('rgroup', 'spam'))
spamFormula <- as.formula(paste('spam=="spam"',
                                paste(spamVars, collapse =' + '), sep=' ~ '))
print(spamVars)
#glm - ??????ƽ ȸ?ͺм?
#
spamModel <- glm(spamFormula,family=binomial(link='logit'),data=spamTrain)
spamTrain$pred <- predict(spamModel, newdata=spamTrain,type='response') #트레인 데이터
spamTest$pred <- predict(spamModel, newdata=spamTest, type='response') # 테스트 데이터
print(with(spamTest, table(y=spam,glmPred=pred>0.5))) # 0.5이상인 것만 보기
#테이블에서 대각선의 값이 높을 수록 예측이 잘된 것.
# 맞다 아니다라는 결과값을 볼 때c
spamD$spam
tail(spamD$spam)

print(with(spamTest, table(y=spam,glmPred=pred>0.10)))
sample <- spamTest[c(7,35,224,327), c('spam','pred')]
sample <- spamTest[c(50,100,200,300), c('spam','pred')]
print(sample)

cM <- table(truth=spamTest$spam, prediction=spamTest$pred<0.5)
print(cM)


#의사결정트리

install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

tModel<- rpart(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=iris)
tModel
rpart.plot(tModel)


#KNN

head(iris)
table(iris$Species)
summary(iris)

install.packages("ggvis")
library(ggvis)
iris %>% ggvis(~Petal.Length, ~Petal.Width, fill=~factor(Species)) %>% layer_points()

min_Max_normalrizer <- function(x){
  num <- x-min(x)
  denom <- max(x)-min(x)
  return(num/denom)
}
normalized_iris <- as.data.frame(lapply(iris[1:4], min_Max_normalrizer))
summary(normalized_iris) #스케일과 다르게 min과 max를 0,1로 바꿈
set.seed(1234)
random_sample <- sample(2, nrow(iris), replace=TRUE, prob = c(0.67, 0.33)) #복원을 하면서샘플을 뽑고 대체한다.
iris.training <- iris[random_sample==1, 1:4]
iris.training
iris.test <- iris[random_sample==2, 1:4]
iris.testLabels <- iris[random_sample==2, 5]

install.packages("class")
library("class")
iris_Model <- knn(train=iris.training, test=iris.test, cl=iris.testLabels, k=3)


install.packages("gmodels")
library(gmodels)
CrossTable(x=iris.testLabels, y=iris_Model, expected = FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)

#K-Means
kmean_iris <- iris
kmean_iris$Species <- NULL
(clusters <- kmeans(kmean_iris, 3)) ## 앞뒤 괄호 다시 디스플레이
table(iris$Species, clusters$cluster)


plot(kmean_iris[c("Sepal.Length", "Sepal.Width")],
     col=clusters$cluster, pch=c(15,16,17)[as.numeric(clusters$cluster)])






# random forest

install.packages("randomForest") # 랜덤포레스트 모델 
install.packages("caret") #특징 선택
install.packages("e1071") #모델 튜닝
install.packages("ROCR") #모델 평가

library(randomForest)
library(caret)
library(e1071)
library(ROCR)

data_F <- read.table(file.choose(), header = T, sep="\t")
formula.init <- ("spam~.")
formula.init <- as.formula(formula.init)
(rf_Model <- randomForest(formula.init, data=data_F, importance=T, proximity=T))

test.class.var <- data_F[,58]
test.class.var

rf.prediction <- predict(rf_Model, type="class")
confusionMatrix(data=rf.prediction, reference=test.class.var,positive=NULL)





















