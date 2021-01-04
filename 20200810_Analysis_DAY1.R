# -------------------------------- ANALYSIS ------------------------------------
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

# [ 지도학습 - 분석 1. 분류 분석 ]
# - 지도학습의 일부(Y가 존재)
# - Y가 범주형인 경우
# - 대표적 모델로 트리기반 모델, 거리기반 모델, 확률통계기반 모델 존재

# [ 분류분석 과정 (비통계적) ]
# - 데이터 수집 (Y의 분류에 영향을 미칠 것 같은 X들을 수집)
# - 전처리 (이상치/결측치 제거 및 수정)
# - 모델 선택 (데이터에 맞게)
# - 데이터 분리  (train / test)
# - 모델 학습 (train data set)
# - 모델 평가 (test data set)    --- ***
# (비통계적이기 때문에 모델평가가 어려움 => 몇개 중 몇개를 맞췄는지로 판단)
# (모델의 학습과 평가의 데이터셋은 분리시킴 => 같은 데이터 사용시 확률 높아지는 문제발생)
# - 모델 튜닝
# - 결과 해석 및 실무 적용

# 1.1 트리기반 모델
# 1.1.1 Decision Tree (트리기반 모델)
# - 분류 분석을 수행하는 트리기반 모델의 가장 시초 모델
# - 패턴 학습이 단순하여 패턴을 시각화 할 수 있음
# - 패턴이 Tree 구조를 띔
# - 비통계적 모델이므로 모델 해석이 매우 용이
# - 단일 의사결정이므로 예측력이 불안하거나 과대적합 가능성 있음

# [ 실습 : iris data의 Decision Tree 모델 적용 ]
install.packages('rpart')
library(rpart)

# 1) 모델 학습 (정답이 있는 데이터)
rpart(formula = ,        # Y ~ X
      data = )           # data

iris_dt1 <- rpart(Species ~ . , data = iris)    # .은 앞에 Species를 제외한 나머지

# [ 참고 1 : 결과 해석 ]
# 1) root 150 100 setosa (0.33333333 0.33333333 0.33333333)
# 1번째 노드
# 150 => 전체 개수
# 100 => 대표자가 아닌 데이터 개수 (오분류 개수)
# 대표자는 setosa why? 각 집단이 같은 확률인데 그중 setosa가 제일 먼저 있어서

# 2, 3)으로 쪼갬
# 2) Petal.Length< 2.45 50   0 setosa (1.00000000 0.00000000 0.00000000) *
# 완벽하게 1번째 데이터만 존재
  
# 3) Petal.Length>=2.45 100  50 versicolor (0.00000000 0.50000000 0.50000000)  
# 50:50 동률이므로 셋 중 2번째(둘 중 1번째) 집단 이름이 나옴
# 자식노드의 불순도가 높을수록 해당 질문지는 나쁜 질문

# 6) Petal.Width< 1.75 54   5 versicolor (0.00000000 0.90740741 0.09259259) *
# 7) Petal.Width>=1.75 46   1 virginica (0.00000000 0.02173913 0.97826087) *
# 최종 노드의 개수 = 3 (*로 표시되어 있음)
  
# 2) 모델 확인 (패턴, 모델 학습결과)
iris_dt1

# 3) 시각화
# 3-1)
dev.new()
plot(iris_dt1, compress = T)
text(iris_dt1, cex = 0.8)

# 3-2) 외부 패키지 활용
install.packages('rpart.plot')
library(rpart.plot)

dev.new()
prp(iris_dt1,
    type = 4,        # 그래프 출력 모양
    extra = 2,       # 추가 정보 => 정분류 개수 출력
    digits = 3)      # 출력 숫자의 자리수

# 4) 모델 평가
# 실제 정답을 알고 있는 평가용 데이터셋의 X들만 모델에 학습,
# 리턴되는 예측값(Y, predict value, fitted value)과 실제값을 비교하여 총 몇건 중 
# 몇건을 맞췄느냐로 평가점수를 계산

# 해당 모델의 과정을 통해 Y가 뭔지 예측
v_pre <- predict(iris_dt1,            # 예측값을 얻고자 하는 모델
                 newdata = iris,      # 데이터프레임 구조
                 type = 'class')      # Y의 출력 형태 (생략 시 확률)
# iris_dt1에서 종속변수가 Species이므로 newdata에 종속변수 포함해도 사용안함
# => X값들만 input, Y는 무시됨
# 모델 학습과 모델 평가의 데이터셋이 같으면 데이터를 기억하고 긍정적인 결과를 만듦

sum(iris$Species == v_pre) / nrow(iris) * 100    # 96%

# 5) 실제 예측
new_data <- data.frame(Sepal.Length = 6.1,
                       Sepal.Width = 4.0,
                       Petal.Length = 2.3,
                       Petal.Width = 0.5)

newdata <- data.frame()
predict(iris_dt1, newdata = new_data,
        type = 'class')    # setosa
predict(iris_dt1, newdata = new_data)    # 100%

# 데이터분석에서 데이터가 완벽하다면 결과도 보통 완벽함 => 데이터 수집, 전처리 과정이 가장 중요함

# -------------------------------연 습 문 제------------------------------------
# 연습문제 1. iris data set을 train/test set으로 분리한 뒤 train으로 학습,
#             test로 평가 후 test set의 평가점수 확인
v_rn <- sample(1:nrow(iris), size = nrow(iris) * 0.7)
iris_train <- iris[v_rn, ]
iris_test <- iris[-v_rn, ]

# step 1) 모델 학습
iris_dt2 <- rpart(Species ~ ., iris_train)

# step 2) 모델 확인
iris_dt2

# step 3) 시각화
# 1)
dev.new()
plot(iris_dt2, compress = T)
text(iris_dt2, cex = 0.8)

# 2)
dev.new()
prp(iris_dt2,
    type = 4,
    extra = 2,
    digits = 3)

# step 4) 모델 평가
predict(iris_dt2, newdata = iris_test)
r_pre <- predict(iris_dt2, newdata = iris_test, type = 'class')

sum(r_pre == iris_test$Species) / nrow(iris_test) * 100    # 95.55% => 할때마다 결과 다름

# cross validation(교차검증) : 샘플링 별 결과가 다르므로 여러번 측정 후 평가 점수를 일반화 시키는 기술

# step 5) 모델 튜닝 ***
iris_dt2$control    # 매개변수

# 1) minsplit = minbucket
#    minbucket = round(minsplit / 3)
# - 추가 가지치기 매개변수 (추가로 가지치기를 할지 말지)
# - how? 오분류 개수 > minsbucket인 경우 추가 가지치기 진행
# - minbucket값 보다 오분류개수가 작아질 때까지 가지치기 계속 시도
# - 하지만 추가로 가지치기할 분류 기준이 더이상 없는 경우에는 stop
# - minbucket값이 작을수록 더 복잡한 모델의 생성 확률 높아짐 (예측력)
# - minbucket값이 작을수록 모델이 복잡해져 overfit 현상 발생 확률 증가 (과대적합)
# - 과대적합 : 해당 데이터에만 너무 적합해지고 실제 데이터에는 확률이 떨어지는 현상

# 예제) minbucket값의 변화에 따른 모델의 예측점수 변화 확인 (매개변수 튜닝)
score_tr <- c() ; score_te <- c()

for (i in 2:10) {
  iris_dt <- rpart(Species ~ ., iris_train,
                   control = rpart.control(minbucket = i))
  v_pre_tr <- predict(iris_dt, newdata = iris_train, type = 'class')
  v_pre_te <- predict(iris_dt, newdata = iris_test, type = 'class')
  
  vscore_tr <- sum(iris_train$Species == v_pre_tr) / nrow(iris_train)
  vscore_te <- sum(iris_test$Species == v_pre_te) / nrow(iris_test)
  
  score_tr <- c(score_tr, vscore_tr)
  score_te <- c(score_te, vscore_te)
}

dev.new()
plot(2:10, score_tr, type = 'o', col = 'blue', ylim = c(0.9, 1))
lines(2:10, score_te, type = 'o', col = 'red')

# 모델이 복잡할수록 예측력은 올라가지만,
# 너무 복잡해지면 새로운 데이터셋에서는 정확히 수행되지 않을 수 있음 => 과대적합# Q1. 정확히 어떤 값 선택
# 과대적합 : train > test => 발생하지 않는 구간이 필요
# 결론 : overfit(과대적합)이 발생하지 않으면서 test score가 높은 매개변수 선택

# 2) maxdepth
# - 재사용가능한 설명변수의 횟수
# - 각 트리 내 노드의 분기 시 설명변수의 재사용 횟수 제한
# - maxdepth 값이 작을수록 단순한 트리구조를 갖을 확률 높아짐

# [ 참고 2 : minbucket이 작아짐에 따른 추가 split 생성 확인 ]
iris_dt2 <- rpart(Species ~ ., data = iris_train,
                  control = rpart.control(minbucket = 2))
dev.new()
rpart.plot(iris_dt2, type = 4)

# 3) cp
# - 직접적으로 트리의 size를 조절하는 매개변수
# - 모델 훈련 후 cptable의 cp값과 tree size, error를 비교하면서
#   적절한 cp값으로 튜닝
# - cp값을 시각화하면 모델의 기준 error와 함께 적절한 cp값 찾기 용이

# cptable : 각 split개수마다의 표준오차(rel error) 확인
# nsplit + 1 = tree size
iris_dt2$cptable

# plotcp : cp값 시각화 (기준 오차 확인)
dev.new()
plotcp(iris_dt2)

# rpart.plot : 모델의 시각화
dev.new()
rpart.plot(iris_dt2, type = 4)
# ------------------------------------------------------------------------------