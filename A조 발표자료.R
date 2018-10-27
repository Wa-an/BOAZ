install.packages("MASS") # 데이터가 들어있는 패키지
install.packages("caret") # 데이터 준비과정에 사용
install.packages("neuralnet") # 신경망 모형 구축에 사용
install.packages("vcd") # 데이터 시각화에 사용

library(caret)
library(MASS)
library(neuralnet)
library(vcd)

data(shuttle)
str(shuttle) # 데이터 구조보기

# 데이터 256개 관찰값, 7개의 변수
# 모든 변수 범주형, 반응변수는 자동과 비자동 2개의 수준을 갖고 있음
# Stability : 안정적인 위치 여부
# error : 오차의 크기 
# sign : 오차의 부호, 양 또는 음
# wind : 바람방향
# magn : 바람의 강도
# vis : 가시성


table(shuttle$use) # 거의 57%로 자주, 자동착륙 기능을 사용하라는 결정이 내려짐/ Auto:145 Nonauto: 111
table1<-structable(wind+magn~use,shuttle)
table1
# 강도가 light인 역풍(headwind)의 경우에는 각각 auto가 19번, noauto가 13번 일어났음

# vcd 패키지에는 mosaic 함수 있음, structable함수로 생성한 표 그려주고, 카이제곱 검정을 위한 유의확률 제공함
mosaic(table1,shade=T)
# mosaic function -> 다변량 변수, 범주형 데이터
# 유의확률이 0.99844로 유의하지 않게 나타났으므로 변수들은 독립이며, 이는 바람의 방향이나 세기를 알아도 
# 자동착륙을 써야 할지 예측하는데 도움이 되지 않는다는 것을 의미한다.

mosaic(use~error+vis,shuttle,gp=gpar(fill=c("yellow", "blue")), direction="v")
# 사각형이 두가지 음영으로 표시 / 귀무가설의 기각과 변수의 종속성이 반영되어 있음
# 도표는 가시성을 받아 종축으로 쪼갬
# if, 가시성이 no이면 자동착륙을 사용함
# 그림에서 왼쪽에 위치한 vis가 no인 열은 모두 밝은 회색으로 auto를 사용함
# 그 다음, 횡축방향으로 오류를 쪼갬
# if, vis가 yes일 때, error가 SS 또는 MM이면 자동착륙을 추천할 수 있음 
# 즉, auto를 나타내는 부분은 밝은 회색 / 회색으로 칠해진 부분이 유의수준을 나타냄 -> p-value 따로 표시 할 필요 없음

table(shuttle$use,shuttle$stability)
prop.table(table(shuttle$use,shuttle$stability)) # table을 비율로 나타낼 수 있ㅇ

chisq.test(shuttle$use,shuttle$stability) # 카이제곱검정

# 신경망을 위한 데이터 준비과정에서 모든 설명변수와 종속변수들은 모두 수치형(numeric)이 어야 함
#더미변수는 범주형 변수를 연속형 변수로 변환 -> 정확히 따지자면 연속형 변수"스럽게" 만든다.
# 따라서 caret 패키지를 활용하여 쉽게 입력피처로 사용할 dummy변수 만들 수 있음
dummies<-dummyVars(use~.,shuttle,fullRank=T)
dummies

shuttle.2=as.data.frame(predict(dummies,newdata=shuttle))
names(shuttle.2)
head(shuttle.2)

shuttle.2$use<-ifelse(shuttle$use=="auto",1,0)
table(shuttle.2$use)


set.seed(123) #난수 생성
trainIndex<-createDataPartition(shuttle.2$use,p=.7,list=FALSE) # 훈련 데이터 : 테스트 데이터 = 7:3
shuttleTrain<-shuttle.2[trainIndex,]
shuttleTest<-shuttle.2[-trainIndex,]

# neuralnet패키지 사용

n<-names(shuttleTrain)
form<-as.formula(paste("use~",paste(n[!n %in% "use"],collapse = "+")))
form
# hidden : 은닉층의 개수와 한 층에 들어 있는 뉴런의 개수로 이루어진 벡터 / 층의 개수는 3개까지 가능, 기본값으로 1을 가짐
# act.fct : 이 인자는 활성화 함수 / 기본값인 logistic을 사용하거나, tanh을 선택 할 수 있음
# err.fct : 오류를 계산하는 데 사용하는 미분가능 함수 / 기본값으로 sse를 사용함
# linear.output : 논리값을 갖는 인자 / 활성함수를 무시할 것인지 말 것인지 정함


fit<-neuralnet(form,data=shuttleTrain,err.fct="ce",linear.output=FALSE)
fit$result.matrix

head(fit$generalized.weights[[1]])

plot(fit)
# 위 plot에서 노드 사이의 연결선 위에 각 변수와 2의 가중값이 나타나 있음
# 일반화 가중값 또한 plot으로 검사할 수 있음
# vis.yes와 wind.tail을 비교해 보면 wind.tail이 전반적으로 낮은 가중값 중 하나임을 알 수 있음


par(mfrow=c(1,2))
gwplot(fit,selected.covariate="vis.yes")
gwplot(fit,selected.covariate="wind.tail")
# vis.yes는 비대칭 분포이며, wind.tail은 고른 분포를 보이고 예측력이 없음을 알 수 있음

resultsTrain<-compute(fit,shuttleTrain[,1:10])
predTrain<-resultsTrain$net.result # 예측값 목록

predTrain<-ifelse(predTrain>=0.5,1,0) # 결과는 확륙값으로 표시되므로 이를 0또는 1로 변환하고 이것을 Confusion Matrix 만듦
table(predTrain,shuttleTrain$use) # 결과에서 100%의 정확도를 보임 (TP:76/ TN: 104)


resultsTest<-compute(fit,shuttleTest[,1:10])
predTest<-resultsTest$net.result
predTest<-ifelse(predTest>=0.5,1,0)
table(predTest,shuttleTest$use) # test에서도 거의 100% (TP: 34/ TN: 40/ FN:1 )

which(predTest==1 & shuttleTest$use==0) # test data에서 어디에서 허위양성이 나왔는지 확인하고 싶을 때 which사용함



install.packages('devtools')
devtools::install_github("rstudio/keras") 
library(keras)
?dataset_cifar10 #CIFAR10이란
cifar<-dataset_cifar10()
train_x<-cifar$train$x/255 # DATA 학습

train_y<-to_categorical(cifar$train$y,num_classes = 10) #원핫 인코딩, 0,1 바이너리로 표현

test_x<-cifar$test$x/255 #DATA test
test_y<-to_categorical(cifar$test$y,num_classes=10) 


dim(train_x) # 차원 학인
cat("No of training samples\t",dim(train_x)[[1]],"\tNo of test samples\t",dim(test_x)[[1]])

#모델 선언
model<-keras_model_sequential()

#모델 구현
model %>% 
 #첫 2D convolution

 layer_conv_2d(filter=32,kernel_size=c(3,3),padding="same", input_shape=c(32,32,3) ) %>% 
 layer_activation("relu") %>% 

 #2번째 2D convolution

 layer_conv_2d(filter=32 ,kernel_size=c(3,3)) %>% layer_activation("relu") %>%


 #pooling layer convolution을 통해 도출한 값을 추출 -> 차원/복작도를 줄여주는 효과
layer_max_pooling_2d(pool_size=c(2,2)) %>% 

 #overfitting을 피하기 위해 dropout
 layer_dropout(0.25) %>%
 layer_conv_2d(filter=32 , kernel_size=c(3,3),padding="same") %>% layer_activation("relu") %>% layer_conv_2d(filter=32,kernel_size=c(3,3) ) %>% layer_activation("relu") %>% 
 layer_max_pooling_2d(pool_size=c(2,2)) %>% 
 layer_dropout(0.25) %>%

 #flatten the input 
 layer_flatten() %>% 
 layer_dense(512) %>% 
 #reflu activagtion
 layer_activation("relu") %>% 
 layer_dropout(0.5) %>% 
 #output layer-10 classes-10 units 
 layer_dense(10) %>% 
 #최종적으로 비선형 activation function softmax -> nonlinearlity 때문
layer_activation("softmax") 

#for computing Probabilities of classes-"logit(log probabilities)

#Optimizer선택 -ADAM-Adaptive Momentum Estimation
opt<-optimizer_adam( lr= 0.0001 , decay = 1e-6 )
#lr-학습률 , decay - 매 이터레이션 별 학습률 decay

model %>%
 compile(loss="categorical_crossentropy",
 optimizer=opt,metrics = "accuracy")

#신경망 구조
summary(model)


# MODEL 학습
data_augmentation <- TRUE 
if(!data_augmentation) { 
 model %>% fit( train_x,train_y ,batch_size=32,
 epochs=80,validation_data = list(test_x, test_y),
 shuffle=TRUE)
}else {
 #이미지 생성

gen_images <- image_data_generator(featurewise_center = TRUE,
 featurewise_std_normalization = TRUE,
 rotation_range = 20,
 width_shift_range = 0.30,
 height_shift_range = 0.30,
 horizontal_flip = TRUE )

 gen_images %>% fit_image_data_generator(train_x)
 model %>% fit_generator(
 flow_images_from_data(train_x, train_y,gen_images,
 batch_size=32,save_to_dir="~/Desktop/CIFAR"),
 steps_per_epoch=as.integer(50000/32),epochs = 80,
 validation_data = list(test_x, test_y) )
} 