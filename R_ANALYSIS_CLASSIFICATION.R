# ------------------------------- ANALYSIS 1 -----------------------------------
# [ 분석 기초 ]
# 1. 데이터 분석
# - 데이터 마이닝 : 데이터로부터 의미 있는 정보(현상)를 찾는 분석 기법
#   ex) 비오는날 빨간립스틱의 수요가 증가함을 발견

# 2. 미래 예측
# 머신러닝(정형데이터)
# 사용자가 데이터로부터 직접 패턴을 찾는게 아닌 기계(모델)가 데이터의 학습을 
# 통해 규칙을 찾아주는 분석 기법

# 2.1 지도학습 : 예측값(Y)이 존재 o
# X ~ Y

#   1) 회귀기반(Y가 연속형) : 수요예측, 부동산가격예측 ...
#   2) 분류기반(Y가 범주형) : 이탈예측, 생존여부예측, ...
#   - 트리기반 모델
#   - 거리기반 모델
#   - 확률/통계 모델
#   - 신경망 모델(딥러닝)

# 2.2 비지도학습 : 예측값(Y)이 존재 x
#   1) 군집분석 (거리기반 모델) => 평가 없음
#   2) 연관분석
#   3) 딥러닝(비정형데이터)
# 인공지능 > 머신러닝 > 딥러닝
# 신경망 구조의 머신러닝 기법을 딥러닝이라 표현
# 주로 비정형데이터의 분석 시 사용
# 블랙박스 모델 : 결과만 확인가능, 패턴(원인)은 파악하기 어려움
# ------------------------------------------------------------------------------

# [ 지도학습 - 분석 1. 분류 분석 ]
# - 지도학습의 일부(Y가 존재)
# - Y가 범주형인 경우
# - 대표적 모델로 트리기반 모델, 거리기반 모델, 확률통계기반 모델 존재

# [ Data Analysis Process - 분류분석, 비통계적]
# 0) Setting Purpose of Analysis
# 1) data loading (데이터 수집 - Y의 분류에 영향을 미칠 것 같은 X들을 수집)
# 2) preprocessing (데이터 전처리 - 이상치/결측치 제거 및 수정)
# 3) model selection based on data (모델 선택)
# 4) data split into train/test (데이터 분리)
# 5) model fitting by train data set (모델 학습)
# 6) model scoring by test data set (모델 평가)
# - 비통계적이기 때문에 모델평가가 어려움 => 몇개 중 몇개를 맞췄는지로 판단
# - 모델의 학습과 평가의 데이터셋은 분리시킴 => 같은 데이터 사용 시 확률 높아지는 문제발생
# 7) model tuning (모델 튜닝)
# 7-1) parameter tuning => 매개변수 튜닝
# 7-2) feature importance check => 특성 중요도 튜닝 (설명변수 중요도)
# 8) review & apply

# 1.1 트리기반 모델
# - DT -> RF -> GB -> XGB (발전방향)
# - RF : 각 트리가 독립적, 병렬처리에 유리 (cpu당 작업 나눠줌)
# - GB : 앞에 있는 트리를 참고함 (상관적), 적은 수의 트리로도 예측력 높음
#      : 보안에는 좋지만, 앞 트리가 완성될 때까지 기다려야하므로 속도가 느림
#      : 직렬처리에 유리
# - 데이터포인트 : 각 데이터
# - outlier 민감 x
# - 범주, 연속형 변수 모두 포함 가능
# - 학습시간에 비해 예측시간이 빠름
# - 연속형 데이터에 대한 scaling(표준화) 필요 x

# 1.1.1 Decision Tree (트리기반 모델)
# - 분류 분석을 수행하는 트리기반 모델의 가장 시초 모델
# - 패턴 학습이 단순하여 패턴을 시각화 할 수 있음
# - 패턴이 Tree 구조를 띔
# - 비통계적 모델이므로 모델 해석이 매우 용이
# - 단일 의사결정이므로 예측력이 불안하거나 과대적합 가능성 있음

# [ 실습 : cancer data의 Decision Tree 모델 적용 ]
install.packages('rpart')
library(rpart)
# 1) data loading
cancer <- read.csv('cancer.csv')
cancer$id <- NULL

# 2) checking variables.importance
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

# 지니계수
# - AAAAAA : 오분류율 0 => p(A가 속할 확률) = 1, f(p) = 1 * (1-1) = 0
# - AAAABB : 오분류율 2/6 => p = 4/6, f(p) = 4/6 * (1 - 4/6) = 0.222
# - AAABBB : 오분류율 3/6 => p = 3/6, f(p) = 3/6 * (1 - 3/6) = 0.25
# => 지니계수는 오분류율이 높을수록 커짐

# 2-1) variable.importance
cancer_dt1$variable.importance
# 예) iris data
# Petal.Width Petal.Length Sepal.Length  Sepal.Width 
# 88.96940     81.34496     54.09606     36.01309 

# 2-2) 교차 산점도
# 종양의 양/악성에 영향을 미치는 주요 변수 확인
dev.new()
cancer$diagnosis <- as.factor(cancer$diagnosis)
plot(cancer[ , 2:10], col = cancer$diagnosis)    # factor가 숫자로 인식되어 컬러반영
# 영향 o => radius_mean, perimeter_mean, area_mean, ...

# 3) sampling : test / train data set으로 나누기
v_rn <- sample(1:nrow(cancer), size = nrow(cancer) * 0.7)
cancer_tr <- cancer[v_rn, ]
cancer_te <- cancer[-v_rn, ]

# 4) modeling & check
rpart(formula = ,        # Y ~ X
      data = )           # data

cancer_dt1 <- rpart(diagnosis ~ ., data = cancer_tr)    # .은 앞에 Species를 제외한 나머지
cancer_dt1

# 5) visualization
# 5-1)
dev.new()
plot(cancer_dt1, compress = T)
text(cancer_dt1, cex = 0.8)

# 5-3) 외부 패키지 활용
install.packages('rpart.plot')
library(rpart.plot)

rpart.plot(cancer_dt1)

dev.new()
prp(cancer_dt1,
    type = 4,        # 그래프 출력 모양
    extra = 2,       # 추가 정보 => 정분류 개수 출력
    digits = 3)      # 출력 숫자의 자리수

# 6) model evaluation
# 실제 정답을 알고 있는 평가용 데이터셋의 X들만 모델에 학습,
# 리턴되는 예측값(Y, predict value, fitted value)과 실제값을 비교하여 총 몇건 중 
# 몇건을 맞췄느냐로 평가점수를 계산

# 해당 모델의 과정을 통해 Y가 뭔지 예측
pre_te <- predict(cancer_dt1,           # 예측값을 얻고자 하는 모델
                  newdata = cancer_te,  # 데이터프레임 구조
                  type = 'class')       # Y의 출력 형태 (생략 시 확률)

# cancer_dt1에서 종속변수가 diagnosis이므로 newdata에 종속변수 포함해도 사용안함
# => X값들만 input, Y는 무시됨
# 모델 학습과 모델 평가의 데이터셋이 같으면 데이터를 기억하고 긍정적인 결과를 만듦

sum(cancer_te$diagnosis == pre_te) / nrow(cancer_te) * 100    # 91.8% => 할때마다 결과 다름
# cross validation(교차검증) : 샘플링 별 결과가 다르므로 여러번 측정 후 평가 점수를 일반화 시키는 기술

# 7) model tuning
cancer_dt1$control    # 매개변수

# 7-1) minsplit = minbucket
# - minbucket = round(minsplit / 3)
# - 추가 가지치기 매개변수 (추가로 가지치기를 할지 말지)
# - how? 오분류 개수 > minsbucket인 경우 추가 가지치기 진행
# - minbucket값 보다 오분류개수가 작아질 때까지 가지치기 계속 시도
# - 하지만 추가로 가지치기할 분류 기준이 더이상 없는 경우에는 stop
# - minbucket값이 작을수록 더 복잡한 모델의 생성 확률 높아짐 (예측력)
# - minbucket값이 작을수록 모델이 복잡해져 overfit 현상 발생 확률 증가 (과대적합)
# - 과대적합 : train 데이터에만 너무 적합해져 실제 데이터에서는 확률이 떨어지는 현상

# minbucket값의 변화에 따른 모델의 예측점수 변화 확인 (매개변수 튜닝)
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
plot(1:10, score_tr, type = 'o', col = 'blue', ylim = c(0.85, 1))
lines(1:10, score_te, type = 'o', col = 'red')

# 모델이 복잡할수록 예측력은 올라가지만, (minbucket이 작을수록)
# 너무 복잡해지면 새로운 데이터셋에서는 정확히 수행되지 않을 수 있음 => 과대적합
# 과대적합 : train > test => 발생하지 않는 구간이 필요
# test와 train의 score 격차가 적으면서 test score가 높은 minbucket 찾기
# train score >>> test score : 5% 기준 넘으면 과대적합
# train score <<< test score : 과소적합
# 결론 : overfit(과대적합)이 발생하지 않으면서 test score가 높은 매개변수 선택

# 7-2) maxdepth
# - 설명변수의 재사용가능 횟수
# - 각 트리 내 노드의 분기 시 설명변수의 재사용 횟수 제한
# - maxdepth 값이 작을수록 단순한 트리구조를 갖을 확률 높아짐

# [ 참고 1 : minbucket이 작아짐에 따른 추가 split 생성 확인 ]
cancer_dt2 <- rpart(diagnosis ~ ., data = cancer_tr,
                    control = rpart.control(minbucket = 2))
dev.new()
rpart.plot(cancer_dt2, type = 4)

# 7-3) cp
# - 직접적으로 트리의 size를 조절하는 매개변수
# - 모델 훈련 후 cptable의 cp값과 tree size, error를 비교하면서
#   적절한 cp값으로 튜닝
# - cp값을 시각화하면 모델의 기준 error와 함께 적절한 cp값 찾기 용이

# cptable : 각 split개수마다의 표준오차(rel error) 확인
# nsplit + 1 = tree size
cancer_dt1$cptable

# plotcp : cp값 시각화 (기준 오차 확인)
dev.new()
plotcp(cancer_dt1)    # size of tree = 2일 때 가장 좋아보임..                   # Q1. cp값 수정하기

# rpart.plot : 모델의 시각화
dev.new()
rpart.plot(cancer_dt1, type = 4)

# 8) real predict => 기존 데이터 살짝 변경해서 시도
new_data <- cancer[10, -1] + 0.01
predict(cancer_dt1, newdata = new_data, type = 'class')

# 데이터분석에서 데이터가 완벽하다면 결과도 보통 완벽함 => 데이터 수집, 전처리 과정이 가장 중요함
# ------------------------------------------------------------------------------

# 1.1.2 조건부 추론 나무
# - 기존 decision tree를 보안
# - 분류 기준에 따른 적합성을 통계적으로 해석하는 장치 추가 (변수의 유의성 검정)
install.packages('party')
library(party)

ctree(formula = ,    # Y ~ X
      data = )       # data set

cancer_dt3 <- ctree(diagnosis ~ ., data = cancer_tr)

dev.new()
rpart.plot(cancer_dt3)

dev.new()
plot(cancer_dt3, compress = T)
text(cancer_dt3, cex = 0.8)
# 유의성도 검증해줌
# ------------------------------------------------------------------------------

# 1.1.3 Random Forest
# - Decision Tree의 과대적합 현상을 해결하기 위해 여러 개의 서로 다른 모양의 
#   tree를 구성, 종합하여 최종 결론을 내는 방식
# - Random Forest Classifier와 Random Forest Regressor 존재
# - 분류모델인 경우는 다수결로, 회귀모델인 경우는 평균으로 최종결론
install.packages('randomForest')
library(randomForest)

# 1) data loading & sampling
cancer <- read.csv('cancer.csv')
cancer$id <- NULL

cancer$diagnosis <- factor(cancer$diagnosis)    # 분류모델로 만들기 위해 (not 회귀)
v_rn <- sample(1:nrow(cancer), size = nrow(cancer) * 0.7)
cancer_tr <- cancer[v_rn, ]
cancer_te <- cancer[-v_rn, ]

# 2) modeling
randomForest(fomular = ,
             data = )

cancer_rf1 <- randomForest(diagnosis ~ ., data = cancer_tr)

# Call:
#   randomForest(formula = diagnosis ~ ., data = cancer_tr) 
# Type of random forest: classification    => diagnosis가 factor이므로 분류모델이 됨
# Number of trees: 500                     => elbow point까지만 하면 됨 (속도도 생각해야 하므로)
# No. of variables tried at each split: 5  => 각 트리마다 시도되는 설명변수 개수
#                                          => 각 트리마다 train data를 랜덤 선택함 by 복원추출(replacement)
#                                          => 그럼에도 같은 train data가 들어간다면? 설명변수를 고정해서 진행
#                                          => 5개의 설명변수 만으로 진행


# OOB estimate of  error rate: 4.77%       => 오분류율
# Confusion matrix:
#   Benign Malignant class.error
# Benign       243         7  0.02800000
# Malignant     12       136  0.08108108

# 3) evaluation
pre_te <- predict(cancer_rf1, cancer_te, type = 'class')
sum(cancer_te$diagnosis == pre_te) / nrow(cancer_te) * 100    # 97.7%

cancer_rf1$predicted    # cancer_tr를 predict 함수에 넣은 것과 같음
cancer_rf1$importance   # 각 설명변수의 중요도 표시

# 4) tuning
# 4-1) ntree : 트리의 개수 (default = 500)
randomForest(diagnosis ~ ., cancer_tr, ntree = 10)

# How set # of ntree? find elbow point!
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

# 4-2) mtry 
# - 각 split시 고려되어지는 설명변수의 후보의 개수
# - 서로 다른 트리를 구성하기 위해 생긴 매개변수 형태
# - 값이 작을수록 서로 다른 트리가 형성

# mtry = 1 => 매번 랜덤하게 선택된 변수 선택 -> 서로 다른 트리 구성
#          => 트리가 복잡해지거나 예측력이 낮아질 확률 존재
# mtry = 4 => 똑같은 설명변수 선택 -> 똑같은 트리 (단, 학습되는 데이터 동일 가정)

# mtry 변화에 따른 예측력과 과대적합 여부 확인
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

mtry = 5

# 5) real predict
new_data <- cancer[10, -1] + 0.01
predict(cancer_rf1, newdata = new_data, type = 'class')
# ------------------------------------------------------------------------------

# [ 참고 2 : Random Forest에서의 회귀 ]
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

# [ Y가 연속형인 경우 분석 방법 ]
# 1) 전통 회귀 분석
# - 여러가지 통계적 가정 필요
# - 가정이 성립되지 않으면 예측력이 떨어짐
# - 인과관계 파악이 용이

# 2) 분류모델이 제공하는 회귀 모델
#     (ex. randomForest-classification)
# - 비통계적 모델
# - 특별한 튜닝 없이도 우수한 예측력
# - 인과관계 파악은 불가 (예측에 집중)
# ------------------------------------------------------------------------------

# 1.2 거리기반 모델
# - 찾고자하는 데이터와 유사한 데이터를 찾음
# - ourlier 민감 o => 반드시 제거/수정
# - 범주형 설명변수가 많이 포함될수록 예측력 떨어짐 (거리화 시키기 어려움)
# - 고차원 데이터에 비적합 (설명변수가 많은 데이터)
# - 예측 시간이 오래 걸림 (대신 학습과정 생략, 데이터 들어올 때마다 계산)
# - 연속형 데이터에 대한 scaling(표준화) 필요
# - 모델에 학습되는 설명변수의 조합이 매우 중요한 모델

# 예) 거리기반 모델링 과정
# 소득 직업 지출     A상품구매여부
# 1 400 회사원(1) 200    X
# 2 410 회사원(1) 220    X
# 3 500 자영업(2) 400    O
# 
# 4 420 회사원(1) 180    ?

d41 <- sqrt((420 - 400)^2 + (1 - 1)^2 + (200 - 180)^2) ; d41  # 28.28
d42 <- sqrt((420 - 410)^2 + (1 - 1)^2 + (180 - 220)^2) ; d42  # 41.23
d43 <- sqrt((420 - 500)^2 + (1 - 2)^2 + (180 - 400)^2) ; d43  # 234.1
# => 4번 관측치와 가장 유사한(거리가 가까운) 관측치는 1번이므로 1번의 행동과 같은
#    행동을 할 것으로 예상, 즉 구매하지 않을 것

# [ 거리계산 시 표준화(scaling) 필요 이유 ]
#      x1    x2
# p1   5     100
# p2   6     200
# p3   10    150

# 1) 표준화 없이 일반 거리 계산
d12 <- sqrt((5 - 6)^2 + (100 - 200)^2) ; d12    # 100.005
d13 <- sqrt((5 - 10)^2 + (100 - 150)^2) ; d13    # 50.249
d12 > d13

# 2) 표준화 후 거리 계산
# (x1의 평균 6, 표준편차 0.01)
# (x2의 평균 150, 표준편차 30)

#      x1                 x2
# p1   (5 - 6) / 0.01     (100 - 150) / 30
# p2   (6 - 6) / 0.01     (200 - 150) / 30
# p3   (10 - 6) / 0.01    (150 - 150) / 30

#      x1       x2
# p1   -100     -1.67
# p2   0        1.67
# p3   400      0

d12 <- sqrt((-100 - 0)^2 + (-1.67 - 1.67)^2) ; d12    # 100.005
d13 <- sqrt((-100 - 400)^2 + (-1.67 - 0)^2) ; d13    # 500.002
d12 < d13
# 표준화 이후 위와 결과가 정반대

# 1.2.1 knn(k-nearest neighbor)
# - 예측하고자 하는 관측치로부터 기존관측치와의 거리 계산
# - 계산된 거리 중 k개의 가장 가까운 이웃 관측치 확인
# - k개의 이웃이 갖는 정답(Y)의 평균 혹은 다수결로 최종 결론 내림
# - 설명변수의 scaling 필요
# - Y class의 개수의 배수가 되는 k수는 적절치 않음
# - 모델에 학습되는 설명변수의 조합이 매우 중요한 모델

install.packages('class')
library(class)

knn(train = ,        # 기존 데이터 X
    test = ,         # 예측할 데이터 X
    cl = ,           # 기존 데이터 Y
    k = ,            # 이웃의 수 : k명에게 물어보다
    prob = )         # 확률 추출 여부

# [ 예제 - knn 모델을 사용한 iris & cancer 예측 ]
# 1) sampling
cancer <- read.csv('cancer.csv')
cancer$id <- NULL

v_rn < - sample(1:nrow(cancer), nrow(cancer) * 0.7)
cancer_tr_y <- cancer[v_rn, 1]
cancer_te_y <- cancer[-v_rn, 1]

cancer_tr_x <- cancer[v_rn, -1]
cancer_te_x <- cancer[-v_rn, -1]

vrn <- sample(1:nrow(iris), size = nrow(iris) * 0.7)
iris_tr_y <- iris[vrn, 5]
iris_tr_x <- iris[vrn, -5]

iris_te_y <- iris[-vrn, 5]
iris_te_x <- iris[-vrn, -5]

# 2) modeling & predict 동시 수행
pre_cancer_te <- knn(cancer_tr_x, cancer_te_x, cancer_tr_y, k=3)

pre_te <- knn(iris_tr_x, iris_te_x, iris_tr_y, k = 3, prob = T)
# k = 짝수일 경우 예측력이 떨어질 수 있음 => 50% 확률이 나오면 예측력 떨어질 수 있음

# 3) evaluation
sum(cancer_te_y == pre_cancer_te) / nrow(cancer_te_x) * 100    # 94.1%

sum(iris_te_y == pre_te) / nrow(iris_te_x) * 100    # 91.1%

# 4) turning
# 4-1) k value
score_tr <- c() ; score_te <- c()

for ( i in 1:10) {
  pre_tr <- knn(cancer_tr_x, cancer_tr_x, cancer_tr_y, k=i, prob = T)
  pre_te <- knn(cancer_tr_x, cancer_te_x, cancer_tr_y, k=i, prob = T)
  
  vscore_tr <- sum(cancer_tr_y == pre_tr) / nrow(cancer_tr_x) * 100 
  vscore_te <- sum(cancer_te_y == pre_te) / nrow(cancer_te_x) * 100 
  
  score_tr <- c(score_tr, vscore_tr)
  score_te <- c(score_te, vscore_te)
}

dev.new()
plot(1:10, score_tr, type='o', col='blue', ylim = c(80,100),
     xlab = 'k의 수', ylab = 'score', 
     main = 'k수 변화에 따른 score 변화')
lines(1:10, score_te, type='o', col='red')

axis(1, at=1:10)
legend(8,90, c('train','test'), col = c('blue','red'), lty=1)
# k 값이 너무 커지면 성향이 다른 데이터와의 거리까지 계산하므로 예측력 떨어질 수 있음
# => 보통 3-5를 많이 사용하고 10을 초과하지 않음
# => 여기서는 k = 5가 적절해 보임

# 위 정도로도 훌륭하지만 scaling과 설명변수 평가 후 다시 예측해볼 필요 있음
# 4-2) scaling (k = 5)
# 예) how to do scaling
df1 <- data.frame(col1=c(1,2,4,7,10), col2 = c(10,32,45,35,35))
scale(df1)

mean(df1$col1)  # 4.8
sd(df1$col1)    # 3.7

# cancer 설명변수 scaling
cancer_tr_x_sc <- scale(cancer_tr_x)
cancer_te_x_sc <- scale(cancer_te_x)

# scaling된 data로 모델링 및 평가
pre_cancer_te_sc <- knn(cancer_tr_x_sc, cancer_te_x_sc, 
                        cancer_tr_y, k = 5, prob = T)

sum(cancer_te_y == pre_cancer_te_sc) / nrow(cancer_te_x_sc) * 100    # 95.9%

# 4-3) variable.importance (k = 5)
# 모델의 설명변수의 조합에 따라 결과가 달라질 가능성이 높음 => 중요 설명변수를 선택하고 사용해야함 **
# DT or RF는 설명변수를 자체평가함
# - 교차산점도
cancer$diagnosis <- as.factor(cancer$diagnosis)
plot(cancer[ , 2:10], col = cancer$diagnosis)

# - RF의 feature importance
cancer_rf1 <- randomForest(diagnosis ~ ., cancer)
cancer[ , -1] <- cancer[ , -1][ , cancer_rf1$importance > 5]    # 어떤 설명변수를 고를지는 분석가의 재량
# 위 방식으로 feacture importance를 뽑아내고 전과정을 다시 진행함 => 결과는 크게 다르지는 않음

# 결론 : k value, scaling, variable.importance 후 예측력 향상
# ------------------------------------------------------------------------------