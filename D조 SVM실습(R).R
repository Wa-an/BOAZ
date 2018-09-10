install.packages("e1071")
library(e1071)
data(iris)

# Q1. r패키지 "e1070"의 svm()함수를 이용해 svm분류 수행
svm.e1071 <- svm(Species ~ . , data = iris,
                 type = "C-classification", kernel = "radial",
                 cost = 10, gamma = 0.1)
# type : svm()의 수행방법 채택 (디폴트는 C-classification 또는 eps-regression 이다.)
# kernel : 훈련과 예측에 사용되는 커널
#          'radial'옵션은 가우시안 RBF를 의미
# cost : 제약 위배의 비용(디폴트 1)
# gamma : 선형을 제외한 모든 커널에 요구되는 모수(디폴트 : 1/데이터차원)

summary(svm.e1071)

# plot()함수를 이용해 결과 시각화
plot(svm.e1071, iris, Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))

plot(svm.e1071, iris, Sepal.Width ~ Sepal.Length,
     slice = list(Petal.Width = 2.5, Petal.Length = 3))
# slice : 변수가 2개 이상일 때 상수값을 할당


pred <- predict(svm.e1071, iris, decision.values = TRUE)
(acc <- table(pred, iris$Species))
# 해석 : 분류된 데이터를 실제값과 비교했을 때 'setosa','virginica'는 50개 모두 완벽히 분류
#         'versicolor'는 50개중 47개가 잘 분류.


# classAgreement() : 모형의 정확도 확인
classAgreement(acc)


# tuned() : 최적의 모수를 제공, 동시에 여러 모수값에 대한 검정 결과 제공 
tuned <- tune.svm(Species~., data = iris, gamma = 10^(-6:-1),
                  cost = 10^(1:2))
# 6×2=12개의 조합에서 모수조율이 이루어짐
summary(tuned)






# Q2. r패키지 "kernlab"의 ksvm()함수를 이용해 svm분류 수행
install.packages("kernlab")
library(kernlab)
data(iris)
svm.kernlab <- ksvm(Species ~ ., data = iris, type = "C-bsvc",
                      kernel = "rbfdot", kpar = list(sigma = 0.1),
                      C = 10, prob.model = TRUE)
svm.kernlab


# plot() : 분류된 결과에 대한 각 변수별 분포를 상자그림의 형태로 나타냄
fit <- fitted(svm.kernlab)
par(mfrow=c(2,2))
plot(fit, iris[,1], main="Sepal.Length")
plot(fit, iris[,2], main="Sepal.Width")
plot(fit, iris[,3], main="Petal.Length")
plot(fit, iris[,4], main="Petal.Width")
par(mfrow=c(1,1))

# predidct() : 새로운 자료에 대한 분류 수행 가능
table(predict(svm.kernlab, iris), iris[,5])
# 여기서, 모형 구축에 사용된 자료 재사용해 분류 수행
# 그 결과, setosa, viginica는 50개 모두. versicolor는 50개중 47개가 제대로 분류 







# Q3. r패키지 "e1071"의 svm()함수를 이용해 svr(서포트벡터회귀)분류 수행
# 분석용 자료 생성
x <-c(1:20)
y <- c(3,4,8,4,6,9,8,12,15,26,35,40,45,54,49,59,60,62,63,68)
data<-data.frame(x, y)


# svr과 비교를 위해 lm()함수를 통해 단순회귀를 적합
plot(data, pch=16)
model <- lm(y ~ x, data)
abline(model)
lm.error <- model$residuals # same as data$Y - predictedY
(lmRMSE <- sqrt(mean(lm.error^2)))
# 그 결과, RMSE = 5.703778을 얻음


# svm() : SVR을 수행
model <- svm(y ~ x , data)
pred.y <- predict(model, data)

points(data$x, pred.y, col = "red", pch=4) 
# SVR수행결과+ 단순회귀 결과와 함께 그림, pch=4는 ‘x’임

error <- data$y - pred.y
svmRMSE <- sqrt(mean(error^2))
svmRMSE
# SVR의 RMSE= 3.157061로 단순회귀에 비해 감소!


# tune() : 각 모형의mse값 제공(mse의 제곱근:RMSE)
tuneResult <- tune(svm, y ~ x, data = data,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
# 11× 8 = 88 개 모수 조합에서 조율


# 격자탐색 : 정교한 모수 tuning을 위해 실시
tuneResult <- tune(svm, y ~ x, data=data,
                   ranges = list(epsilon=seq(0,0.2,0.01), cost=2^(2:9))) 

# 21X8 = 168개 모수 조합

print(tuneResult)
# 결과 : 최소 오차를 가지는 모형의 epsilon과 cost값을 보여줌



# 최적의 모형을 찾는 쉬운 방법
tunedModel <- tuneResult$best.model
tpred.y <- predict(tunedModel, data)
error <- data$y - tpred.y
tsvmRMSE <- sqrt(mean(error^2))
tsvmRMSE
# 결과 RMSE가 작은 모형을 찾아 성능이 개선
