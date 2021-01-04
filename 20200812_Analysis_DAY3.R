# [ Y가 연속형인 경우 분석 방법 ]
# 1. 전통 회귀 분석
# - 여러가지 통계적 가정 필요
# - 가정이 성립되지 않으면 예측력이 떨어짐
# - 인과관계 파악이 용이

# 2. 분류모델이 제공하는 회귀 모델
#     (ex. randomForest-classification)
# - 비통계적 모델
# - 특별한 튜닝 없이도 우수한 예측력
# - 인과관계 파악은 불가 (예측에 집중)

# [ 분석 1. 분류 분석 ]
# 1.1 트리기반 모델
# - DT -> RF -> GB -> XGB (발전방향)
# - RF : 각 트리가 독립적, 병렬처리에 유리(cpu당 작업 나눠줌)
# - GB : 앞에 있는 트리를 참고함 (상관적), 적은 수의 트리로도 예측력 높음
#      : 보안에는 좋지만, 앞 트리가 완성될 때까지 기다려야 하므로 속도가 느림
#      : 직렬처리에 유리
# - 데이터포인트 : 각 데이터
# - outlier 민감 x
# - 범주, 연속형 변수 모두 포함 가능
# - 학습시간에 비해 예측시간이 빠름
# - 연속형 데이터에 대한 scaling(표준화) 필요 x
# 
# 1.2 거리기반 모델
# - 찾고자하는 데이터와 유사한 데이터를 찾음
# - ourlier 민감 o => 반드시 제거/수정
# - 범주형 설명 변수가 많이 포함될수록 예측력 떨어짐 (거리화 시키기 어려움)
# - 고차원 데이터에 비적합
# - 예측 시간이 오래 걸림 (학습과정 생략, 데이터 들어올때마다 계산)
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

# 결론) 4번 관측치와 가장 유사한(거리가 가까운) 관측치는 1번이므로 1번의 행동과 같은
# 행동을 할 것으로 예상, 즉 구매하지 않을 것

# [ 거리계산 시 표준화 필요 이유 ]
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

# 1.2.1 knn
# - 예측하고자 하는 관측치로부터 기존관측치와의 거리 계산
# - 계산된 거리 중 k개의 가장 가까운 이웃 관측치 확인
# - k개의 이웃이 갖는 정답(Y)의 평균 혹은 다수결로 최종 결론 내림
# - 설명변수의 스케일링 필요
# - Y class의 개수의 배수가 되는 k수는 적절치 않음
# - 모델에 학습되는 설명변수의 조합이 매우 중요한 모델

install.packages('class')
library(class)

knn(train = ,        # 기존 데이터 X
    test = ,         # 예측할 데이터 X
    cl = ,           # 기존 데이터 Y
    k = ,            # 이웃의 수 : k명에게 물어보다
    prob = )         # 확률 추출 여부

knn()

# [ 예제 - knn 모델을 사용한 iris data 품종 예측 ]
# 1) sampling
vrn <- sample(1:nrow(iris), size = nrow(iris) * 0.7)
iris_tr_y <- iris[vrn, 5]
iris_tr_x <- iris[vrn, -5]

iris_te_y <- iris[-vrn, 5]
iris_te_x <- iris[-vrn, -5]

# 2) modeling & predict 동시 수행
pre_te <- knn(iris_tr_x, iris_te_x, iris_tr_y, k = 3, prob = T)
# k = 짝수일 경우 예측력이 떨어질 수 있음 => 50% 확률이 나오면 예측력 떨어질 수 있음

# 3) evaluation
sum(iris_te_y == pre_te) / nrow(iris_te_x) * 100    # 97.8

# 4) predict
new_data <- data.frame(Sepal.Length = 7,
                       Sepal.Width = 3,
                       Petal.Length = 1,
                       Petal.Width = 1.0)
knn(iris_tr_x, new_data, iris_tr_y, k = 3, prob = T)

# 5) parameter(매개변수)
# k수 변화에 따른 train/test data set score 확인
score_te <- c() ; score_tr <- c()
for (i in 1:10) {
  pre_tr <- knn(iris_tr_x, iris_tr_x, iris_tr_y, k = i, prob = T)
  pre_te <- knn(iris_tr_x, iris_te_x, iris_tr_y, k = i, prob = T)
  
  vscore_tr <- sum(iris_tr_y == pre_tr) / nrow(iris_tr_x) * 100
  vscore_te <- sum(iris_te_y == pre_te) / nrow(iris_te_x) * 100
  
  score_tr <- c(score_tr, vscore_tr)
  score_te <- c(score_te, vscore_te)
}

plot(1:10, score_tr, type = 'o', col = 'blue', ylim = c(80, 100),
     xlab = 'k의 수', ylab = 'score',
     main = 'k수 변화에 따른 score 변화')
lines(1:10, score_te, type = 'o', col = 'red')

axis(1, at = 1:10)
legend(9, 90, c('train', 'test'), col = c('blue', 'red'), lty = 1)
# k 값이 너무 커지면 성향이 다른 데이터와의 거리까지 계산하므로 예측력 떨어질 수 있음
# => 보통 3-5를 많이 사용하고 10을 초과하지 않음

# -------------------------------연 습 문 제------------------------------------
# 연습문제 6. knn을 사용한 cancer data의 종양의 양성 여부 예측
cancer <- read.csv('cancer.csv')
head(cancer)
cancer$id <- NULL
# step 1) sampling
v_rn < - sample(1:nrow(cancer), nrow(cancer) * 0.7)
cancer_tr_y <- cancer[v_rn, 1]
cancer_te_y <- cancer[-v_rn, 1]

cancer_tr_x <- cancer[v_rn, -1]
cancer_te_x <- cancer[-v_rn, -1]

# step 2) modeling & predict with tuning 1(k값)
score_te <- c() ; score_tr <- c()
for (i in 1:10) {
  pre_tr <- knn(cancer_tr_x, cancer_tr_x, cancer_tr_y, k = i, prob = T)
  pre_te <- knn(cancer_tr_x, cancer_te_x, cancer_tr_y, k = i, prob = T)
  
  vscore_tr <- sum(cancer_tr_y == pre_tr) / nrow(cancer_tr_x) * 100
  vscore_te <- sum(cancer_te_y == pre_te) / nrow(cancer_te_x) * 100
  
  score_tr <- c(score_tr, vscore_tr)
  score_te <- c(score_te, vscore_te)
}

# step 3) continue:tuning by visualization
dev.new()
plot(1:10, score_tr, type = 'o', ylim = c(80, 100), col = 'blue')
lines(1:10, score_te, type = 'o', col = 'red')

# step 4) best i = 9
pre_te <- knn(cancer_tr_x, cancer_te_x, cancer_tr_y, k = 3, prob = T)
sum(cancer_te_y == pre_te) / nrow(cancer_te_x) * 100

# step 4) tuning 2(설명변수의 중요도)
# 모델의 설명변수의 조합에 따라 결과가 달라질 가능성이 높음 => 중요 설명변수를 선택하고 사용해야함 **
# DT or RF는 설명변수를 자체가 평가함
# 위 정도로도 훌륭하지만 scaling과 설명변수 평가 후 다시 예측해볼 필요 있음
# 1) 교차산점도
cancer$diagnosis <- as.factor(cancer$diagnosis)
plot(cancer[ , 2:10], col = cancer$diagnosis)

# 2) RF의 feature importance
cancer_rf1 <- randomForest(diagnosis ~ ., cancer)
cancer[ , -1] <- cancer[ , -1][ , cancer_rf1$importance > 5]
# 위 방식으로 feacture importance를 뽑아내고 전과정을 다시 진행함 => 결과는 크게 다르지는 않음

# step 5) tuning 3(scaling) : 설명변수의 스케일 조정 후 모델 학습 (k = 8)
# 예)
df1 <- data.frame(col1 = c(1, 2, 4, 7, 10), col2 = c(10, 32, 45, 35, 35))
scale(df1)        # 평균과 표준편차 확인

mean(df1$col1)    # 4.8 (평균)
sd(df1$col1)      # 3.7 (표준편차)

# cancer 설명변수 scaling
cancer_tr_x_sc <- scale(cancer_tr_x)
cancer_te_x_sc <- scale(cancer_te_x)

# scaling된 data로 모델링
pre_cancer_te_sc <- knn(cancer_tr_x_sc, cancer_te_x_sc, cancer_tr_y, k = 3, prob = T)

sum(cancer_te_y  == pre_cancer_te_sc) / nrow(cancer_te_x_sc) * 100    # 위 결과보다 낮아짐

# 결론 : scaling한 후 예측력이 향상될 확률 존재 (그러지 않을 수도 있음)
# + 만약 설명변수 중 의미 없는, 예측력에 방해되는 변수가 있다면
# 해당 변수를 제거한 후 다시 모델링 필요
# ------------------------------------------------------------------------------

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

# ** 군집 형성 과정 (군집과 데이터포인트와의 거리 측정 방식에 따라)
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

# step 1) 각 관측치 간 거리 계산 (거리행렬)
dist(v1)    # 거리 계산 함수
# p2  2         
# p3  5  3      
# p4  9  7  4   
# p5 17 15 12  8

# step 2) 위 거리 중 가장 가까운 데이터포인트를 하나의 군집으로 묶음
#         => c1(p1, p2)

# step 3) c1과 나머지 데이터포인트끼리의 거리 계산
#    - 최단거리법 (single, min)
dc1p3 = min(dp1p3, dp2p3) = min(5, 3) = 3
dc1p4 = min(dp1p4, dp2p4) = min(9, 7) = 7
dc1p5 = min(dp1p5, dp2p5) = min(17, 15) = 15

#    - 최장거리법 (complete, max)
dc1p3 = max(dp1p3, dp2p3) = max(5, 3) = 5
dc1p4 = max(dp1p4, dp2p4) = max(9, 7) = 9
dc1p5 = max(dp1p5, dp2p5) = max(17, 15) = 17

#    - 평균거리법 (average)
dc1p3 = d(p1p2)p3 = max(2, 6) = 4
...

# step 4) 위에서 구해진 군집과의 거리를 모두 계산,
#         가장 가까운 거리 파악 (최단거리법)
#     c1 p3 p4 p5
# c1  .         
# p3  3  .      
# p4  7  4 .   
# p5 15 12 8  .
# 3이 가장 작으므로 p3은 c1에 속해짐 => c1(p1, p2, p3)

# step 5) 변경된 c1과 나머지 관측치(p4, p5)의 거리 계산
dc1p4 = min(dp1p4, dp2p4, dp3p4) = min(9, 7, 4) = 4
dc1p5 = min(dp1p5, dp2p5, dp3p5) = min(17, 15, 12) = 12

# step 6) 
#     c1 p4 p5
# c1  .         
# p4  4  .   
# p5 12  8 .
# 4가 가장 작으므로 p4은 c1에 속해짐 => c1(p1, p2, p3, p4)

# 위 결과를 실제 계층적 군집분석에 의해 시각화
hclust(d,                           # 거리행렬
       method = 'single',
                'complete',
                'average',
                'median')

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
