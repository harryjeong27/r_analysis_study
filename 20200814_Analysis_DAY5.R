# 변수선택법(전진, 후진, stepwise)
# 1. 전진선택법(forward selection)
# 가장 중요하다고 생각하는 변수 한개씩 추가하다가 이전보다 설명력이 낮으면 이전모델 선택
Y = X1
Y = X1 + X2
Y = X1 + X2 + X3
 
# 2. 후진선택법(backward selection)
# 가장 의미가 적은 변수만 제거하다가 이전보다 설명력이 낮으면 이전모델 선택
Y = X1 + X2 + X3 + X4 + X5
Y = X1 + X2 + X3 + X4
Y = X1 + X2 + X3
 
# 3. 단계적선택법(stepwise selection)
# 1, 2번 동시 적용 => 변수중요도에 따라 추가/제거 둘다 가능
Y = X1 + X2 + X3 + X4 + X5
Y = X1 + X2 + X3 + X4
Y = X1 + X2 + X3            # 제거된 변수 중 추가할 변수 있는지
Y = X1 + X2 + X3 + X5

# [ 예제 - 보스턴 주택 가격셋을 사용한 변수선택 방법 ]
# 교재 3. R을 사용한 상관과 회귀 (p.36/49)
install.packages('mlbench')
library(mlbench)      # BostonHousing data loading 시 필요
data(BostonHousing)
str(BostonHousing)    # 13개의 설명변수 + 1개의 종속변수 (medv)

m <- lm(medv ~ ., data = BostonHousing)    # 변수중요도
step(m, direction = "both")
step(m, direction = "forward")
step(m, direction = "backward")

# [ 예제 - iris셋을 사용한 변수선택 방법 ]
library(rpart)
m_dt1 <- rpart(Species ~ ., data = iris)
step(m_dt1, direction = 'both')    # Error => 회귀분석용 (Y가 연속형인 경우만)

# 범주형 Y를 위한 함수
library(MASS)
install.packages('klaR')
library(klaR)

stepclass(x,                 # x 데이터
          y,                 # y 데이터 
          method = 'lda',    # 분류방법
          direction = ,      # forward, backward, both
          start.vars = )     # 시작변수

stepclass(iris[ , -5], iris[, 5], 'lda',
          direction = 'both')

# final model : iris[, 5] ~ Petal.Width
#     correctness rate = 0.96 

stepclass(iris[ , -5], iris[, 5], 'lda',
          direction = 'both', start.vars = 'Petal.Width')    # 96%

stepclass(iris[ , -5], iris[, 5], 'lda',
          direction = 'both', start.vars = 'Petal.Length')
# 내가 중요하다고 생각하는 변수가 함수통해 중요하지 않다고 나오는 이유는 변수형태임
# ex) X1/X2가 중요한 변수일 수 있음
# ------------------------------------------------------------------------------

# [ 실습 2  - cancer data를 사용한 군집분석 ]
library(rpart)
library(randomForest)
library(NbClust)

# Answer 1
# 0) data lodaing
cancer <- read.csv('cancer.csv')
cancer$id <- NULL

# 1. 계층적 군집분석 수행
# 1.1 적절한 k수 확인 1 (scaling x, 중요변수 선택 x)
# 1) 모델 생성
d1 <- dist(cancer[ , -1])
m_cl1 <- hclust(d1, 'single')
m_cl2 <- hclust(d1, 'complete')
m_cl3 <- hclust(d1, 'average')

# 2) 모델 시각화 및 평가
dev.new()
par(mfrow = c(1, 3))
plot(m_cl1, hang = -1, main = 'single')
rect.hclust(m_cl1, 2)

plot(m_cl2, hang = -1, main = 'complete')
rect.hclust(m_cl2, 2)

plot(m_cl3, hang = -1, main = 'average')
rect.hclust(m_cl3, 2)

table(cancer$diagnosis)

# 1.2 적절한 k수 확인 2 (scaling o, 중요변수 선택 x)
# 1) 모델 생성
d2 <- dist(scale(cancer[ , -1]))
m_cl_sc1 <- hclust(d2, 'single')
m_cl_sc2 <- hclust(d2, 'complete')
m_cl_sc3 <- hclust(d2, 'average')

# 2) 모델 시각화 및 평가
dev.new()
par(mfrow = c(1, 3))
plot(m_cl_sc1, hang = -1, main = 'single')
rect.hclust(m_cl_sc1, 2)

plot(m_cl_sc2, hang = -1, main = 'complete')
rect.hclust(m_cl_sc2, 2)

plot(m_cl_sc3, hang = -1, main = 'average')
rect.hclust(m_cl_sc3, 2)
# worse then no scaling

# 1.3 적절한 k수 확인 3 (scaling o, 중요변수 선택 o)
# 1.3.1 by. tree 기반 모델링을 통한 변수 선택 혹은 제거
cancer_dt1 <- rpart(cancer$diagnosis ~ ., data = cancer)
var_imp1 <- cancer_dt1$variable.importance
sort(var_imp1)[sort(var_imp1) > 10]

feature1 <- names(sort(var_imp1)[sort(var_imp1) > 10])
cancer_sel <- cancer[ , colnames(cancer) %in% feature1]
# - DT & RF 2개 모두 변수 중요도 체크해서 둘다 높게 분류하는 설명변수 선택하는게 베스트

# 1) 모델 생성
d3 <- dist(scale(cancer_sel))
m_cl_sel_sc1 <- hclust(d3, 'single')
m_cl_sel_sc2 <- hclust(d3, 'complete')
m_cl_sel_sc3 <- hclust(d3, 'average')

# 2) 모델 시각화 및 평가
dev.new()
par(mfrow = c(1, 3))
plot(m_cl_sel_sc1, hang = -1, main = 'single')
rect.hclust(m_cl_sel_sc1, 2)

plot(m_cl_sel_sc2, hang = -1, main = 'complete')
rect.hclust(m_cl_sel_sc2, 2)

plot(m_cl_sel_sc3, hang = -1, main = 'average')
rect.hclust(m_cl_sel_sc3, 2)
# no better with scaling & variable.importance
# 제거한 변수들도 중요한 변수라는 반증
# ex) Y = X1 + X2 + X3 + .... + Xn
# X1, X3간의 상관관계가 높을수도 있으므로 각각의 변수 중요도는 낮다고 해도 
# 전체로 봤을때는 중요할 수 있음

# 1.3.2 by. 변수선택법(전진, 후진, stepwise)에 의한 데이터의 선택
# step : Y가 연속형
# regsebsets : Y가 연속형
# stepclass : Y가 팩터형
stepclass(cancer[ , -1], cancer[ , 1], 'lda',
          direction = 'both')    # concave_points_worst(0.91) 변수만 선택됨

stepclass(cancer[ , -1], cancer[ , 1], 'lda',
          direction = 'both', start.vars = 'concave_points_worst')  # 같은 결과
# why? 'stop criterion: improvement less than 5%.'에서 5%가 너무 높기 때문
# => 5%를 좀 더 낮게 해보자.

stepclass(cancer[ , -1], cancer[ , 1], 'lda',
          direction = 'both', improvement = 0.01) # 3개 변수 선택
# concave_points_worst, radius_worst, texture_worst 

stepclass(cancer[ , -1], cancer[ , 1], 'lda',
          direction = 'both', improvement = 0.01, fold = 10) # 교차검증 10회 => 할때마다 결과 다름
# concave_points_worst, perimeter_worst, texture_worst 

df_cancer <- cancer[ , c('concave_points_worst',
                         'radius_worst',
                         'texture_mean')]

# 1) 모델 생성
d4 <- dist(scale(df_cancer))
m_cl_f1 <- hclust(d4, 'single')
m_cl_f2 <- hclust(d4, 'complete')
m_cl_f3 <- hclust(d4, 'average')

# 2) 모델 시각화 및 평가
dev.new()
par(mfrow = c(1, 3))
plot(m_cl_f1, hang = -1, main = 'single')
rect.hclust(m_cl_f1, 2)

plot(m_cl_f2, hang = -1, main = 'complete')
rect.hclust(m_cl_f2, 2)

plot(m_cl_f3, hang = -1, main = 'average')
rect.hclust(m_cl_f3, 2)
# 여전히 개선 효과가 없음

# 결론 : 연구 중 공통적으로 중요도 높은 변수를 넣거나 변수들에 대한 배경지식 활용이 매우 중요
# # concave_points_worst, radius_worst, texture_mean가 가장 적절해보임 by teacher
# ------------------------------------------------------------------------------

# 5. 비계층적 군집분석 수행(표준화 o, 변수선택 o)
df_cancer <- cancer[ , c('concave_points_worst', 
                         'radius_worst',
                         'texture_mean')]

m1 <- hclust(dist(scale(df_cancer)), 'complete')
c1 <- cutree(m1, 2)
g1 <- cancer$diagnosis
levels(g1) <- c(2, 1)

sum(c1 == g1) / nrow(cancer)    # 93.85%
m_kmeans1 <- kmeans(scale(df_cancer), 2)
# between_SS / total_SS = 48.6% => 매우 적은 설명력

vscore <- c()
for (i in 1:10) {
  m_kmeans1 <- kmeans(scale(df_cancer), i)
  vscore <- c(vscore, m_kmeans1$betweenss /m_kmeans1$totss)
}

plot(1:10, vscore, type = 'o')
# 그래프로 봤을때는 2-4가 적당해 보임 => 현실은 2가 정답

# Answer 2
# cancer.csv로 계층적, 비계층적 군집분석 실행하여 best k 찾기
cancer <- read.csv('cancer.csv')
head(cancer)

# 계층적 군집분석
# 1) 중요 설명변수 체크, scaling, 적절한 k 찾기
# 1-1) 중요 설명변수 체크 by RF
cancer$id <- NULL
cancer$diagnosis <- factor(cancer$diagnosis)
cancer_rf1 <- randomForest(diagnosis ~ ., cancer)
cancer_rfimp <- cancer[ , -1][ , cancer_rf1$importance > 5]    # 변수 12개만 잡음

unique(cancer$diagnosis)
# 1-2) scaling
cancer_rfimp_sc <- scale(cancer_rfimp)

# 1-3) 적절한 k 찾기
cancer_nb1 <- NbClust(cancer_rfimp, min.nc = 2, max.nc = 10, method = 'single')    # 2
cancer_nb2 <- NbClust(cancer_rfimp, min.nc = 2, max.nc = 10, method = 'complete')  # 3
cancer_nb3 <- NbClust(cancer_rfimp, min.nc = 2, max.nc = 10, method = 'average')   # 4

cancer_nb1_sc <- NbClust(cancer_rfimp_sc, min.nc = 2, max.nc = 10, method = 'single')   # 2
cancer_nb2_sc <- NbClust(cancer_rfimp_sc, min.nc = 2, max.nc = 10, method = 'complete') # 3
cancer_nb3_sc <- NbClust(cancer_rfimp_sc, min.nc = 2, max.nc = 10, method = 'average')  # 3

# 2) 군집분석 수행
# 2-1) by 중요 설명변수 체크 by RF 
d1 <- dist(cancer_rfimp)
d1m1 <- hclust(d1, 'single')
d1m2 <- hclust(d1, 'complete')
d1m3 <- hclust(d1, 'average')

dev.new()
par(mfrow = c(1, 3))
plot(d1m1, hang = -1, main = 'single')
rect.hclust(d1m1, 3)
plot(d1m2, hang = -1, main = 'complete')
rect.hclust(d1m2, 3)
plot(d1m3, hang = -1, main = 'average')
rect.hclust(d1m3, 3)
# 개판

# 2-2) by 중요 설명변수 체크 by RF  & scaling
d2 <- dist(cancer_rfimp_sc)
d2m1 <- hclust(d2, 'single')
d2m2 <- hclust(d2, 'complete')
d2m3 <- hclust(d2, 'average')

par(mfrow = c(1, 3))
plot(d2m1, hang = -1, main = 'single')
rect.hclust(d2m1, 3)
plot(d2m2, hang = -1, main = 'complete')
rect.hclust(d2m2, 3)
dev.new()
plot(d2m3, hang = -1, main = 'average')
rect.hclust(d2m3, 3)

# 2-3) by 중요 설명변수 체크 by RF  & scaling & 적절한 k 찾기
g1 <- ifelse(cancer$diagnosis == 'Malignant', 1, 2)
sum(cutree(d2m3, k = 3) == g1) / nrow(cancer) * 100    # 80.3%

# 비계층적 분석
# 1) 군집분석 수행 및 평가
kmeans(cancer[ , -1], 3)    # 설명변수 전체 => 80.3%
kmeans(cancer_rfimp, 3)     # 설명변수 12개 => 80.3%
kmeans(cancer_rfimp_sc, 3)  # 설명변수 12개 & scaling => 65.7%

# 2) k수 변화에 따른 예측력 체크
k1 <- c()
for (i in 1:10) {
  kmean_1 <- kmeans(cancer[ , -1], i)
  k1 <- c(k1, kmean_1$betweenss / kmean_1$totss)
}
dev.new()
plot(1:10, k1, type = 'o', xlab = 'k의 수',
     ylab = 'score', main = '군집 수 변화에 따른 설명력')

k1 <- c()
for (i in 1:10) {
  kmean_1 <- kmeans(cancer_rfimp, i)
  k1 <- c(k1, kmean_1$betweenss / kmean_1$totss)
}
dev.new()
plot(1:10, k1, type = 'o', xlab = 'k의 수',
     ylab = 'score', main = '군집 수 변화에 따른 설명력')
# 2개의 결과 모두 2 혹은 3의 k값이 좋다고 함 => k = 2로 다시 비계층적 군집분석 수행
kmeans(cancer[ , -1], 2)    # 설명변수 전체 => 69.6%
kmeans(cancer_rfimp, 2)     # 설명변수 12개 => 69.6%
kmeans(cancer_rfimp_sc, 2)  # 설명변수 12개 & scaling => 54.6%
# 결과 더 안 좋아짐
# 결론 : 군집분석을 통해 얻을 수 있는 cancer 데이터의 예측력은 최대 80.3%
# ------------------------------------------------------------------------------

# [ 이상치 검정 ]
# 1. Y가 연속형인 경우
# - 회귀모델 적용 후 이상치 검정 수행
# - 통계적 모델을 통한 유의성 검정 가능 (p-value)
# - car::outlierTest

# [ 예제 - 보스톤 주택 가격 데이터의 이상치 검정 ]
install.packages('car')
library(car)

m1 <- lm(medv ~ ., BostonHousing)
outlierTest(m1)

# 2. Y가 범주형인 경우
# - randomForest 모델 적용 후 이상치 검정 수행
# - 각 데이터포인트의 거리(유사성) 기반으로 이상치 확인
# - outlier로 확인 가능

# [ 예제 - iris 데이터의 이상치 검정 ]
m2 <- randomForest(Species ~ ., iris, proximity = T)
outlier(m2)

dev.new()
plot(outlier(m2), type = 'h', col = iris$Species)

col1 <- ifelse(iris$Species == 'setosa', 'red',
               ifelse(iris$Species == 'versicolor', 'blue', 'green'))
col2 <- c('red', 'blue', 'green')[as.numeric(iris$Species)]
# 이상치 제거하고 한번 해보기
