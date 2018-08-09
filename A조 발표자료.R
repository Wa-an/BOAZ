##############Generalized Linear Model####################
#GLM은 선형화가 어려운 자료들을 선형화시키는 작업
#여러 모형들(gaussian,poisson 등)이 있지만 가장 대표적인 방법이 로지스틱 회귀분석(y값이 범주형일 때 선형화하여 분류를 할 때 사용)
#실습도 로지스틱회귀분석으로 진행

#1. 데이터 불러오기(UCLA데이터 참조)
#목표:고등학교 GRE, GPA, RANK가 입학(admission)에 어떤 영향을 주는지 로지스틱 회귀분석을 통해 분석한다.
data <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
str(data)
head(data)

#2. 적절한 변수 설정
#반응(종속)변수(Y값)는 입학여부이므로 admit이고, 나머지가 설명변수라고 할 수 있는데
#1번 단계에서 str 결과를 보면 rank변수가 4개의 변수가 아니고 int로 설정되어 하나의 변수로 해석하고 있기 때문에 
#분석에 들어가기 전에 rank를  factor로 변경시켜준다.
data$rank <-as.factor(data$rank)
str(data)

#3. 로지스틱 회귀분석 및 해석
#선형화 작업에는 “glm” 함수를 사용하는데, 선형 회귀분석에서의 “lm”과 유사한 함수라고 보면 된다.
#로지스틱 회귀분석을 하려면 family값에 binomial을 넣어주면 되며, y값의 범주는 2개여야만 한다.
model <- glm(admit ~ gre + gpa + rank, data =data, family = "binomial")
summary(model)
confint(model) #로그가능도로 구한 신뢰구간, 일반적간
confint.default(model) #회귀계수의 표준편차로 구한 신뢰구가
  #유의수준 0.05로 확인하면위 결과에서는 모든 변수가 유의한 것을 알 수 있다. 
  #Coefficient는 로지스틱 회귀모형에서 회귀계수가 변수가 한 단위 증가했을 때 log(odds)의 증가량으로 해석할 수 있다.
  #ex)gre가 1증가할 때, admission의 log odds(non-admission에 대한)가 0.0022 증가한다.

#4. anova 분석 및 R-Squared값 확인
anova(model, test="Chisq") 
  #y가 범주형이므로 카이스퀘어 분석 실시
  #Resid.Dev값이 많이 하락 할 수록 의미있는 변수이며, 그 값이 작아 질 수록 모델의 성능이 좋아진다.
  #해당 변수가 하나씩 포함 될 때마다 Resid.Dev값으로 모델의 성능이 얼마나 나아지는 지 확인 가능
  #rank변수가 폼할 될 때 Dev값이 가장 많이 하락한 것을 알 수 있다.
install.packages("pscl")
library(pscl)
pR2(model)
  #로지스틱 회귀분석에도 선형 회귀 분석에서의 R2와 유사한 개념이 존재한다. Mcfadden R2으로 모델 fit을 확인가능하다.
  #“pscl” 패키지의 pR2 함수를 사용하여 Mcfadden R2()를 알아볼 수 있다.
  #로지스틱 회귀분석에서 R2값은 대개 낮게 나오는 편이므로, 모형평가에서 R2에 너무 의존할 필요는 없다

#5. 오즈비 계산 및 예측
exp(coef(model))  #오즈비 산출
exp(confint(model))  #오즈비에 대한 95% 신뢰구간
newdata<- with(data, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
newdata
newdata$rankP <- predict(model, newdata = newdata, type = "response") 
newdata
  #gre와 gpa가 평균수준일 때, rank에 따라 합격률이 얼마나 다른지 확인가능

#6. 모델 평가
install.packages("ROCR")
library(ROCR)

p <- predict(model, newdata=data, type="response")
pr <- prediction(p, data$admit)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
  #ROC곡선은 로지스틱 회귀모형과 같이 반응값이 범주형인 모델을 평가할 때 사용,
  #그래프가 왼쪽 상단에 가까울수록 좋은 모델
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
  #AUC는 ROC그래프 아래영역의 넓이이다. 1에 가까울수록 좋은 모델이며, 
  #판단 기준은 대략적으로 excellent =  0.9~1, good = 0.8~0.9, fair = 0.7~0.8
  #poor = 0.6~0.7, fail = 0.5~0.6 이렇게 되겠다.
  #AUC값이 좋지 않으므로 좋은 모델이라고 할 수 없다.


#####################Regularization####################
#정규화는 최적의 선형모형을 찾을 때 사용
rm(list=ls())
install.packages("glmnet")
install.packages("MASS")
install.packages("mlbench")

library(MASS) #glm 
library(mlbench) #Sonar데이터 사용 위하여

#1. 데이터 불러오기(Sonar-수중 음파 탐지기 신호 분류 데이터)
data(Sonar)
str(Sonar)
?Sonar
head(Sonar)
  # 61개의 변수로 이루어진 208개의 데이터
  # v1~v60의 설명변수는 연속형변수
  # But, 종속변수 Class는 M, R 총 2개의 범주로 이루어진 범주형변수

#2. 변수설정
Sonar$Class<-as.character(Sonar$Class)
Sonar$Class[Sonar$Class=="M"]<-1
Sonar$Class[Sonar$Class=="R"]<-0
Sonar$Class<-as.numeric((Sonar$Class))
str(Sonar)
  # 범주형 종속변수 Class에 대해 Class=M이면 1, Class=R이면 0으로 변환
  # Class를 제외한 다른 변수는 모두 연속형변수이므로 범주형변수 간의 독립성검정은 시행하지 않음
  # 변수 Class를 숫자변수로 전환한 후 모든 변수가 숫자 변수인 것을 확인

#3. 로지스틱 회귀분석
x = as.matrix(Sonar[,-61])
  #x를 관측값(행렬 형태)으로 저장
y = rep(1, nrow(Sonar))
  #y를 분류된 값(즉, R이면 0, M이면 1)으로 저장
y[which(Sonar[,61]=="R")] = 0
fit.glm = glm(Class~., data=Sonar,family='binomial') 
  # Warning messages:
  # 1: glm.fit: 알고리즘이 수렴하지 않았습니다 
  # 2: glm.fit: 적합된 확률값들이 0 또는 1 입니다 
fit.glm$coef #계수가 매우 크다.
  #정말 과적합 되었는가?
fit.glm$fitted.values
sum((fit.glm$fitted.values > 0.5) == y ) 
  # 변수 60개에 대하여 208개 모두 완벽하게 분류됨 -> overfitting 의심 
V<-paste("V",1:60,sep="")
VV<-paste("Class ~", paste(V,collapse = "+"))
  # ROC plot을 그리기 위해 Class~V1+V2+...+V60  형태를 만들어줌
install.packages("pROC")
library(pROC)
par(mfrow=c(1,2))
roc(as.formula(VV),data=Sonar,plot=T)
  # ROC plot이 직선에 가까운 것으로 보아 logistic regression이 적합하지 않다고 판단할 수 있음

#4. regularization 작업 진행 - glmnet 패키지 사용
#glmnet package에 대한 참고 사이트
#https://cran.r-project.org/web/packages/glmnet/glmnet.pdf
library(glmnet)
?glmnet

#4-1.Ridge : 가중치들의 제곱합을 최소화
Sonar$Class<-as.factor(Sonar$Class) 
  # 능형회귀를 이용하기 위한 종속변수의 범주화
set.seed(0205)
index<-sample(1:length(Sonar[,1]),length(Sonar[,1])*0.7,F)
train<-Sonar[index,]
test<-Sonar[-index,]
  # Sonar 데이터를 7:3의 비율로 train set과 test set으로 랜덤 분류함
reg.ridge<-cv.glmnet(x=data.matrix(train[,-61]),y= data.matrix(train[,61]),family = "binomial", alpha = 0,nfold=10)
  # train set 데이터에 대한 능형회귀 분석 실행
  # 종속변수 y: Class = train[,61]
coef(reg.ridge)
# logistic regression에 비해 추정된 베타계수가 현저히 작아짐
par(mfrow=c(1,1))
plot(reg.ridge)
reg.ridge$lambda.min
  # cv error를 최소화하는 lambda값 = 0.140766
pred.ridge<-predict(reg.ridge,data.matrix(test[,-61]),s= reg.ridge$lambda.min,type="class")
table(Actual= test[,61],Predicted = pred.ridge)
  # (0,0)과 (1,1)이 각각 23, 28이므로 예측이 잘 진행되었음 확인 가능
# Accuracy 확인
install.packages("caret")
library(caret)
confusionMatrix(pred.ridge,test$Class)
  #정확도 계산하는 
  # Accuracy = 80.95%



#4-2.Lasso : 가중치의 절대값의 합을 최소
lasso.reg<-cv.glmnet(x= data.matrix(train[,-61]),y= data.matrix(train[,61]),family = "binomial", alpha = 1,nfold=10)
plot(lasso.reg)
lasso.reg$lambda.min
  # cv error 최소화하는 lambda값 = 0.0194945 (과적합의 위험 존재)
  # test set에 대한 model 예측 (최소 lambda값 이용)
pred.lasso<-predict(lasso.reg,data.matrix(test[,-61]),s= lasso.reg$lambda.min,type="class")
table(Actual= test[,61],Predicted = pred.lasso)
  ## (0,0)과 (1,1)이 각각 21, 23이므로 ridge regression보다 예측이 잘 되지 않음
  # 모델 정확도 확인
confusionMatrix(pred.lasso,test[,61])
  # Accuracy = 69.84%
  # ridge regression보다 Accuracy 작음
  
## 결론
## Ridge Regression이 Sonar 데이터에 가장 적합하다

install.packages("e1071")
install.packages("lattice")
install.packages("ggplot2")
library(e1071)
library(lattice)
library(ggplot2)
