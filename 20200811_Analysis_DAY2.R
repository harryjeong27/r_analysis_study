# -------------------------------연 습 문 제------------------------------------
# 연습문제 2. cancer data의 종양의 양/악성 예측(Decision Tree)
# Answer 1
# step 0) data loading
cancer <- read.csv('cancer.csv')
cancer$id <- NULL

# step 1) 교차산점도 시각화
# 종양의 양/악성에 영향을 미치는 주요 변수 확인
dev.new()
cancer$diagnosis <- as.factor(cancer$diagnosis)
plot(cancer[ , 2:10], col = cancer$diagnosis)    # factor가 숫자로 인식되어 컬러반영
# 영향 o => radius_mean, perimeter_mean, area_mean, ...

# step 2) sampling
v_rn <- sample(1:nrow(cancer), size = nrow(cancer) * 0.7)
cancer_tr <- cancer[v_rn, ]
cancer_te <- cancer[-v_rn, ]

# step 3) modeling
cancer_dt1 <- rpart(diagnosis ~ ., data = cancer_tr)    # depth = 4

# step 4) visualization
dev.new()
plot(cancer_dt1)
text(cancer_dt1)

rpart.plot(cancer_dt1)

# step 5) model evaluation
pre_te <- predict(cancer_dt1, newdata = cancer_te, type = 'class')
sum(cancer_te$diagnosis == pre_te) / nrow(cancer_te) * 100

# step 6) model tuning(minbucket)
cancer_dt1$control

score_tr <- c() ; score_te <- c()
for(i in 1:10) {
  cancer_dt <- rpart(diagnosis ~ ., cancer_tr,
                     control = rpart.control(minbucket = i))
  v_pre_tr <- predict(cancer_dt, newdata = cancer_tr, type = 'class')
  v_pre_te <- predict(cancer_dt, newdata = cancer_te, type = 'class')
  
  vscore_tr <- sum(cancer_tr$diagnosis == v_pre_tr) / nrow(cancer_tr)
  vscore_te <- sum(cancer_te$diagnosis == v_pre_te) / nrow(cancer_te)
  
  score_tr <- c(score_tr, vscore_tr)
  score_te <- c(score_te, vscore_te)
}

dev.new()
plot(1:10, score_tr, type = 'o', col = 'blue', ylim = c(0.85, 1))    # 학습된 데이터이기 때문에 예측력이 더 높음
lines(1:10, score_te, type = 'o', col = 'red')
# test와 train의 score 격차가 적으면서 test score가 높은 minbucket 찾기
# train score >>> test score : 5% 기준 넘으면 과대적합
# train score <<< test score : 과소적합

# 새로운 데이터에 대한 예측 => 기존 데이터 살짝 변경해서 시도
new_data <- cancer[10, -1] + 0.01
predict(cancer_dt1, newdata = new_data, type = 'class')
# ------------------------------------------------------------------------------

# [ 트리 기반 모델의 변수 선택 기능 ]
# - 트리 모델은 각 변수의 중요도를 자식 노드의 불순도를 통해 계산
# - 중요도가 높은 변수를 상위 split 조건으로 사용
# - 변수 간 중요도 차이가 큰 경우 특정 변수만 재사용 가능성 있음
# - 트리 모델의 변수 중요도는 분석 시 중요 변수 파악에 용이하게 사용

# 불순도 (<-> 중요도)
# - 특정 조건으로 인해 분리된 자식노드의 클래스의 혼합 정도
# - 주로 지니계수로 측정
# - 2개 class를 갖는 경우 f(p) = p(1-p)로 계산
# - 불순도는 높을수록 낮은 설명력, 낮을수록 높은 설명력 -> 중요도 높을수록 상단 배치

# 예제) 지니계수
# - AAAAAA : 오분류율 0 => p(A가 속할 확률) = 1, f(p) = 1 * (1-1) = 0
# - AAAABB : 오분류율 2/6 => p = 4/6, f(p) = 4/6 * (1 - 4/6) = 0.222
# - AAABBB : 오분류율 3/6 => p = 3/6, f(p) = 3/6 * (1 - 3/6) = 0.25
# => 지니계수는 오분류율이 높을수록 커짐

# 설명변수의 중요도 체크
# way 1)
iris_dt1$variable.importance
# Petal.Width Petal.Length Sepal.Length  Sepal.Width 
# 88.96940     81.34496     54.09606     36.01309 

# way 2) 교차 산점도

# 1.1.2 조건부 추론 나무
# - 기존 decision tree를 보안
# - 분류 기준에 따른 적합성을 통계적으로 해석하는 장치 추가 (변수의 유의성 검정)
install.packages('party')
library(party)

ctree(formula = ,    # Y ~ X
      data = )       # data set

iris_dt3 <- ctree(Species ~ ., data = iris_train)

dev.new()
rpart.plot(iris_dt2)

dev.new()
plot(iris_dt3)
# 유의성도 검증해줌

# 1.1.3 Random Forest
# - Decision Tree의 과대적합 현상을 해결하기 위해 여러 개의 서로 다른 모양의 
#   tree를 구성, 종합하여 최종 결론을 내는 방식
# - Random Forest Classifier와 Random Forest Regressor 존재
# - 분류모델인 경우는 다수결로, 회귀모델인 경우는 평균으로 최종결론
install.packages('randomForest')
library(randomForest)

# 1) 데이터 수집 및 sampling
v_rn <- sample(1:nrow(iris), size = nrow(iris) * 0.7)
iris_tr <- iris[v_rn, ]
iris_te <- iris[-v_rn, ]

# 2) modeling
randomForest(fomular = ,
             data = )

iris_rf1 <- randomForest(Species ~ ., iris_tr)
# Call:
#   randomForest(formula = Species ~ ., data = iris_tr) 
# Type of random forest: classification       => Species가 factor이므로 분류모델이 됨
# Number of trees: 500                        => elbow point까지만 하면 됨 (속도도 생각해야 하므로)
# No. of variables tried at each split: 2     => 각 트리마다 시도되는 설명변수 개수
#                                             => 각 트리마다 train data를 랜덤 선택함 by 복원추출
#                                             => 그럼에도 같은 train data가 들어간다면? 설명변수를 고정해서 진행
#                                             => 2개의 설명변수 만으로 진행
# 
# OOB estimate of  error rate: 2.86%          => 오분류율
# Confusion matrix:
#   setosa versicolor virginica class.error
# setosa         36          0         0  0.00000000
# versicolor      0         34         2  0.05555556
# virginica       0          1        32  0.03030303

# 3) 평가
pre_te <- predict(iris_rf1, newdata = iris_te, type = 'class')
sum(iris_te$Species == pre_te) / nrow(iris_te)

iris_rf1$predicted    # iris_tr를 predict 함수에 넣은 것과 같음
iris_rf1$importance   # 각 매개변수의 중요도 표시

# 4) 예측
new_data <- data.frame(Sepal.Length = 6,
                       Sepal.Width = 0.1,
                       Petal.Length = 5,
                       Petal.Width = 2)
predict(iris_rf1, newdata = new_data, type = 'class')

# 5) 매개변수 튜닝
# 5-1) ntree : 트리의 개수 (default = 500)
randomForest( ~ , , ntree = 10)

# -------------------------------연 습 문 제------------------------------------
# 연습문제 3. ntree의 elbow point 확인
score_te <- c()
for(i in 1:1000) {
  iris_rf <- randomForest(Species ~., iris_tr, ntree = i)
  pre_te <- predict(iris_rf, newdata = iris_te, type = 'class')  
  vscore_te <- sum(iris_te$Species == pre_te) / nrow(iris_te)
  score_te <- c(score_te, vscore_te)
}

dev.new()
plot(1:1000, score_te, type = 'o', ylab = 'score', xlab = 'ntree',
     ylim = c(0.9, 1), main = 'tree수 변화에 따른 예측점수 변화')
# ------------------------------------------------------------------------------

# 5-2) mtry
# - 각 split시 고려되어지는 설명변수의 후보의 개수
# - 서로 다른 트리를 구성하기 위해 생긴 매개변수 형태
# - 값이 작을수록 서로 다른 트리가 형성
  
# mtry = 1 => 매번 랜덤하게 선택된 설명변수 선택 => 서로 다른 트리 구성
#          => 트리가 복잡해지거나 예측력이 낮아질 확률 존재
# mtry = 4 => 똑같은 설명변수 선택 -> 똑같은 트리(단, 학습데이터 동일 가정)
  
# -------------------------------연 습 문 제------------------------------------
# 연습문제 4. mtry 변화에 따른 예측력과 과대적합 여부 확인
score_te <- c() ; score_tr <- c()
for (i in 1:4) {
  iris_rf <- randomForest(Species ~., iris_tr, mtry = i)
  pre_tr <- predict(iris_rf, newdata = iris_tr, type = 'class')  
  pre_te <- predict(iris_rf, newdata = iris_te, type = 'class')  
  
  vscore_tr <- sum(iris_tr$Species == pre_tr) / nrow(iris_tr)
  vscore_te <- sum(iris_te$Species == pre_te) / nrow(iris_te)
  
  score_tr <- c(score_tr, vscore_tr)
  score_te <- c(score_te, vscore_te)
}

dev.new()
plot(1:4, score_tr, type = 'o', ylab = 'score', xlab = 'mtry',
     ylim = c(0.7, 1), main = 'tree수 변화에 따른 예측점수 변화')
lines(1:4, score_te, type = 'o', col = 'red')

# Random Forest에서의 회귀
data('airquality')
airquality

randomForest(Ozone ~ ., airquality,
             mtry = 3, ntree = 100, na.action = na.omit)

# Call:
#   randomForest(formula = Ozone ~ ., data = airquality, mtry = 3, ntree = 100, na.action = na.omit) 
# Type of random forest: regression
# Number of trees: 100
# No. of variables tried at each split: 3
# 
# Mean of squared residuals: 292.0016
# % Var explained: 73.39    # R square => 설명력이 낮음 -> 설명변수 추가 필요
# ------------------------------------------------------------------------------

# -------------------------------연 습 문 제------------------------------------
# 연습문제 5. cancer data by randomForest
cancer <- read.csv('cancer.csv')
cancer$id <- NULL

head(cancer)
# step 1) 데이터 수집 및 sampling
cancer$diagnosis <- factor(cancer$diagnosis)
v_rn <- sample(1:nrow(cancer), size = nrow(cancer) * 0.7)
cancer_tr <- cancer[v_rn, ]
cancer_te <- cancer[-v_rn, ]

# step 2) modeling
cancer_rf1 <- randomForest(diagnosis ~ ., data = cancer_tr)

# step 3) evaluation
pre_te <- predict(cancer_rf1, cancer_te, type = 'class')
sum(cancer_te$diagnosis == pre_te) / nrow(cancer_te)

# step 4) tuning
# 1) ntree
score_te <- c()
for(i in 1:1000) {
  cancer_rf1 <- randomForest(diagnosis ~., cancer_tr, ntree = i)
  pre_te <- predict(cancer_rf1, newdata = cancer_te, type = 'class')  
  vscore_te <- sum(cancer_te$diagnosis == pre_te) / nrow(cancer_te)
  score_te <- c(score_te, vscore_te)
}

dev.new()
plot(1:1000, score_te, type = 'o', ylab = 'score', xlab = 'ntree',
     ylim = c(0.9, 1), main = 'tree수 변화에 따른 예측점수 변화')

# 2) mtry
score_te <- c() ; score_tr <- c()
for (i in 1:7) {
  cancer_rf1 <- randomForest(diagnosis ~., cancer_tr, mtry = i)
  pre_tr <- predict(cancer_rf1, newdata = cancer_tr, type = 'class')  
  pre_te <- predict(cancer_rf1, newdata = cancer_te, type = 'class')  
  
  vscore_tr <- sum(cancer_tr$diagnosis == pre_tr) / nrow(cancer_tr)
  vscore_te <- sum(cancer_te$diagnosis == pre_te) / nrow(cancer_te)
  
  score_tr <- c(score_tr, vscore_tr)
  score_te <- c(score_te, vscore_te)
}

dev.new()
plot(1:7, score_tr, type = 'o', ylab = 'score', xlab = 'mtry',
     ylim = c(0.8, 1), main = 'tree수 변화에 따른 예측점수 변화')
lines(1:7, score_te, type = 'o', col = 'red')

mtry = 2
# ------------------------------------------------------------------------------