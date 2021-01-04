# [ 비지도학습 - 분석 1. 군집분석 ]
# - 정답이 없는 비지도학습
# - 주어진 데이터들을 유사성에 근거해 비슷한 데이터끼리 묶는 분석기법
# - 초기 데이터에 대한 연구를 위해 사용되거나 클러스터링된 군집에 대한 정보를
#   추가로 활용하기 위해 주로 사용
# - 데이터 축소 테크닉
# - 거리기반 모델*
# - 거리기반 모델의 단점 가지고 있음

# 1.1 계층적 군집분석
# - 거리가 가까운 데이터포인트들끼리 순차적으로 묶는 형식
# - 하나의 데이터가 한 군집에 속하면 군집은 변경되지 않음
# - 군집을 형성하는 기준(군집과 데이터와의 거리를 정의하는 방식)에 따라 
#   여러가지 모델 제공
#   => 최단거리법, 최장거리법, 평균거리법, 중앙거리법으로 나뉨

# [ 군집 형성 과정 (군집과 데이터포인트와의 거리 측정 방식에 따라) ]
# 1) 각 관측치 간 거리 계산
# 2) 가장 가까운 두 관측치를 묶는다 (c1)
# 3) 새로운 클러스터에 넣을지 기존 클러스터(c1)에 소속시킬지 결정
#    by doing 기존 클러스터(c1)와 각 관측치와의 거리 계산 *
# 4) How? 아래 4가지 계산법에 따라 적용
#    - 최단거리법 (single, min)
#    - 최장거리법 (complete, max)
#    - 평균거리법 (average)
#    - 중앙거리법 (median)

# 어떤 군집이 더 좋은가? 군집 평가 매트릭 36가지 있음
# 1) 군집 내 분산은 작을수록, 군집 간 분산은 클수록 좋은 군집

# [ 예제 1 - 각 데이터들끼리의 군집 형성 과정 ]
library(stringr)
v1 <- c(1, 3, 6, 10, 18)
names(v1) <- str_c('p', 1:5)

# 1) 각 관측치 간 거리 계산 (거리행렬)
dist(v1)    # 거리 계산 함수
#     p1 p2 p3 p4
# p2  2         
# p3  5  3      
# p4  9  7  4   
# p5 17 15 12  8

# 2) 위 거리 중 가장 가까운 데이터포인트를 하나의 군집으로 묶음
#    => c1(p1, p2)

# 3) c1과 나머지 데이터포인트끼리의 거리 계산
#    - 최단거리법 (single, min)
dc1p3 = min(dp1p3, dp2p3) = min(5, 3) = 3
dc1p4 = min(dp1p4, dp2p4) = min(9, 7) = 7
dc1p5 = min(dp1p5, dp2p5) = min(17, 15) = 15

#    - 최장거리법 (complete, max)
dc1p3 = max(dp1p3, dp2p3) = max(5, 3) = 5
dc1p4 = max(dp1p4, dp2p4) = max(9, 7) = 9
dc1p5 = max(dp1p5, dp2p5) = max(17, 15) = 17

#    - 평균거리법 (average)
dc1p3 = d(p1p2)p3 = mean(2, 6) = 4
...

# 4) 위에서 구해진 군집과의 거리를 모두 계산, 가장 가까운 거리 파악 (최단거리법)
#     c1 p3 p4 p5
# c1  .         
# p3  3  .      
# p4  7  4 .   
# p5 15 12 8  .
# 3이 가장 작으므로 p3은 c1에 속해짐 => c1(p1, p2, p3)

# 5) 변경된 c1과 나머지 관측치(p4, p5)의 거리 계산
dc1p4 = min(dp1p4, dp2p4, dp3p4) = min(9, 7, 4) = 4
dc1p5 = min(dp1p5, dp2p5, dp3p5) = min(17, 15, 12) = 12

# 6) 
#     c1 p4 p5
# c1  .         
# p4  4  .   
# p5 12  8 .
# 4가 가장 작으므로 p4은 c1에 속해짐 => c1(p1, p2, p3, p4)

# 위 결과를 실제 계층적 군집분석에 의해 시각화
hclust(d,                           # 거리행렬
       method = 'single', 'complete', 'average', 'median')

# 1) 모델 생성 (군집형성)
d1 <- dist(v1)
m_clust1 <- hclust(d1, method = 'single')

# 2) 시각화
dev.new()
par(mfrow = c(1, 2))
plot(m_clust1,
     main = '최단거리법에 의한 군집형성과정')

plot(m_clust1,
     hang = -1,    # 데이터포인트들의 시작을 맨 밑으로 하는 옵션
     main = '최단거리법에 의한 군집형성과정')
rect.hclust(tree = m_clust1,
            k = 2)
# ------------------------------------------------------------------------------

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
# - 적절한 k수 확인
# => 시각화로 확인, 분석가가 원하는 지표, 함수 활용
# - 설명변수 중요도 체크
# => 그래프로 시각화(교차산점도), RF 활용
# - scaling
# ==> 위 3가지를 반복 진행하여 최적화 시켜야 함

# 1.0 적절한 k수 확인 1 => 함수 활용
nc1 <- NbClust(cancer[ , -1], min.nc = 2, max.nc = 10, method = 'single')   # 2
nc2 <- NbClust(cancer[ , -1], min.nc = 2, max.nc = 10, method = 'complete') # 4
nc3 <- NbClust(cancer[ , -1], min.nc = 2, max.nc = 10, method = 'average')  # 4

# 26개 각 지표값 확인
nc1$All.index
# Dindex 지표 : 그룹 내 분산에 의해 계산된 지표
# SDindex 지표 : 그룹 간 분산 및 각 중심의 평균으로부터 계산된 지표

# 군집분석에서의 분산
# 1. 총분산 (total_ss) : 그룹과는 관계없음
# 2. 그룹 내 분산 (within_ss)
#    => 각 클러스터의 중심으로부터 각 클러스터 관측치가 떨어진 정도
# 3. 그룹 간 분산 (between_ss)
#    => 각 클러스터의 중심이 전체중심으로부터 흩어진 정도

# 해석)
# 그룹 내 분산은 작을수록 그룹 간 분산은 클수록 좋은 군집
# => 군집이 많아져도 그룹 내 분산은 작아짐 -> 과연 효과적인가?
# -> 군집이 많아질수록 그룹 내 분산은 당연히 작아짐
# -> 작아지는 기울기가 가장 클 때가 가장 효과적임
# -> How estimate? by Dindex (위 지표에서는 군집이 2일 때)

# -> 군집이 많아질수록 그룹 간 분산도 당연히 커짐
# -> 기울기가 가장 클 때가 가장 효과적임
# -> How estimate? by SDindex (위 지표에서는 군집이 2일 때)
total_ss = between_ss + within_ss    
# 결론 : nc1에서는 군집이 2일때 가장 적절하다고 봄

# 1.1 적절한 k수 확인 1 (scaling x, 중요변수 선택 x) => 시각화로 확인
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
# 시각화로 판별 힘듦

# 1.2 적절한 k수 확인 2 (scaling o, 중요변수 선택 x) => 시각화로 확인
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

# 1.3 적절한 k수 확인 3 (scaling o, 중요변수 선택 o) => 시각화로 확인
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
          direction = 'both', improvement = 0.01)   # 0.95 => 3개 변수 선택
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
# concave_points_worst, radius_worst, texture_mean가 가장 적절해보임 by teacher
# ------------------------------------------------------------------------------

# 5. 비계층적 군집분석 수행(scaling o, 중요변수 선택 o)
# - 거리가 짧은 데이터포인트끼리 묶는 방식은 동일
# - 계층적 군집분석과는 다르게 한번 클러스터에 소속된 관측치도 거리 측정 대상에
#   포함시켜 만약 다른 클러스터와의 거리가 더 짧다면 다른 클러스터로 이동되는 방식
# - 평가 metric 존재 (그룹 내 / 그룹 간 분산에 의한 score)
# - 사용자가 직접 군집의 수를 정하면 해당 군집에 맞게 분류
# 1) dataloading
df_cancer <- cancer[ , c('concave_points_worst', 
                         'radius_worst',
                         'texture_mean')]

m1 <- hclust(dist(scale(df_cancer)), 'complete')
c1 <- cutree(m1, 2)
g1 <- cancer$diagnosis
levels(g1) <- c(2, 1)

sum(c1 == g1) / nrow(cancer)    # 93.85%

# 2) 모델 확인 및 평가
# knn - 분류분석 => Y 예측
# kmeans - 군집분석 => 데이터 세분화
kmeans(x,             # 원본 data (거리행렬 X)
       centers = )    # 군집 수

m_kmeans1 <- kmeans(scale(df_cancer), 2)

# K-means clustering with 2 clusters of sizes 370, 199  # 각 군집의 데이터 개수
# 
# Cluster means:
#   concave_points_worst radius_worst texture_mean
# 1           -0.5851762   -0.5780191   -0.3269797     # n번 군집의 각 컬럼 평균
# 2            1.0880161    1.0747089    0.6079521
# 
# Within cluster sum of squares by cluster:
#   [1] 479.9528 395.2011            # 각 군집의 그룹 내 분산
# (between_SS / total_SS =  48.6 %)  # 매우 적은 설명력

# totss
m_kmeans1$totss           # 1704 => 총분산은 항상 같음

# withinss
sum(m_kmeans1$withinss)   # 875.1539

# betweenss
m_kmeans1$betweenss       # 828.8461

# totss = withinss + betweenss
# scaling 하지 않은 상태에서 위 수치를 비교하기는 무리가 있음 -> 총분산으로 나눔
# => 계산하기 쉽게 커질수록 좋은 betweenss를 사용 (between_SS / total_SS =  48.6 %)

# 3) k수 변화에 따른 between_SS / total_ss의 변화 확인
vscore <- c()
for (i in 1:10) {
        m_kmeans1 <- kmeans(scale(df_cancer), i)
        vscore <- c(vscore, m_kmeans1$betweenss /m_kmeans1$totss)
}

plot(1:10, vscore, type = 'o')
# 군집의 개수가 증가할수록 설명력은 증가할 수 밖에 없지만,
# 그래프로 봤을때는 2-4가 적당해 보임 => 현실은 2가 정답

# [ 비 계층적 군집분석 수행 방식 ]
# 1. 사용자가 지정한 k의 수만큼 랜덤하게 초기 중심 (seed)값 추출
# 2. 위 seed로부터 각 관측치와의 거리 계산
# 3. 거리가 가장 가까운 관측치들을 각 클러스터에 할당
# 4. 변경된 클러스터의 중심을 재계산
# 5. 재계산된 클러스터의 중심으로 전체 데이터의 거리 모두 계산
# 6. 각 클러스터의 중심으로부터 가까운 데이터포인트 소속/이동
# 7. 위 과정을 더이상의 중심이 이동되지 않을때까지 계속 반복
# 8. 그룹 고정

# 결론 : 계층적 이후 비계층적으로 분석해서 비교해야함

# + 중요변수 조절해서 다시 해봐야함 => 계층적 비계층적 결과 비교해서 최종결론
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