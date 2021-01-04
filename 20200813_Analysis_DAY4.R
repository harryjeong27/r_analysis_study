# [ 예제 2 - iris data의 설명변수만으로 군집분석 수행 및 결과 확인 ]
# step 1) 거리행렬 구하기
d1 <- dist(iris[ , -5])

# step 2) 군집분석 수행
# 1) 최단거리법
m1 <- hclust(d1, 'single')

# 2) 최장거리법
m2 <- hclust(d1, 'complete')

# 3) 평균법
m3 <- hclust(d1, 'average')

# step 3) 시각화
dev.new()
par(mfrow = c(1, 3))
    
plot(m1, hang = -1, main = 'single')
rect.hclust(m1, k = 3)

plot(m2, hang = -1, main = 'complete')
rect.hclust(m2, k = 3)

plot(m3, hang = -1, main = 'average')
rect.hclust(m3, k = 3)
# 분석가의 성격에 따라 군집을 구분하면 됨 => 보기엔 average가 가장 효과적으로 보임

# step 4) 평가
# iris 데이터의 정답(Y)을 알고 있기 때문에 분류분석의 평가 metric에 의해 평가 시도
# 정상적인 방식은 아님
# 1) single
cutree(m1, k = 3)    # 모델을 통해 나눠진 군집의 유형
g1 <- ifelse(iris$Species == 'setosa', 1, ifelse(iris$Species == 'versicolor', 2, 3))
sum(cutree(m1, k = 3) == g1) / nrow(iris) * 100    # 68%

# 2) complete
cutree(m2, k = 3)    # 모델을 통해 나눠진 군집의 유형
# => 시각화를 보았을 때 2번보다 3번 클래스가 많다면 -> species명을 바꿔줌
g2 <- ifelse(iris$Species == 'setosa', 1, ifelse(iris$Species == 'versicolor', 3, 2))
sum(cutree(m2, k = 3) == g2) / nrow(iris) * 100    # 84%
# => 이에 따라 single도 순서 변경해보면... 그래도 비슷함
cutree(m1, k = 3)    # 모델을 통해 나눠진 군집의 유형
g1 <- ifelse(iris$Species == 'setosa', 1, ifelse(iris$Species == 'versicolor', 3, 2))
sum(cutree(m1, k = 3) == g1) / nrow(iris) * 100    # 65%

# 3) average
cutree(m3, k = 3)    # 모델을 통해 나눠진 군집의 유형
# => 시각화를 보았을 때 우리가 알던 순서대로 1, 2, 3인듯
g3 <- ifelse(iris$Species == 'setosa', 1, ifelse(iris$Species == 'versicolor', 2, 3))
sum(cutree(m3, k = 3) == g3) / nrow(iris) * 100    # 90%

# 결론
# 1) 시각화를 통해 눈으로 짐작한 결과대로 average가 가장 효과적*
# 2) cutree 사용 시 시각화와 비교하여 순서에 맞게 그룹번호 매칭

# step 5) 적절한 k의 수 찾기
# 1) 시각화로 확인
# 2) 분석가가 원하는 지표
# 3) 함수활용
install.packages('NbClust')
library(NbClust)

NbClust(data = ,                      # 군집분석 수행 데이터 (거리행렬 x)
        distance = 'euclidean',       # 거리 측정 방식
        min.nc = ,                    # 평가할 최소 군집
        max.nc = ,                    # 평가할 최대 군집
        method = )                    # 군집과의 거리 방식

nc1 <- NbClust(iris[ , -5], min.nc = 2, max.nc = 10, method = 'single')
nc2 <- NbClust(iris[ , -5], min.nc = 2, max.nc = 10, method = 'complete')
nc3 <- NbClust(iris[ , -5], min.nc = 2, max.nc = 10, method = 'average')

# step 6) tuning
# -------------------------------연 습 문 제------------------------------------
# 연습문제 7. iris data의 군집분석 효과를 높이기 위한 (정답을 알고 있다는 가정)
#             방법 모색

# 1) 설명변수 중요도 체크
# way 1) 그래프로 시각화
plot(iris[ , -5], col = iris$Species)
# way 2) rf 활용
iris_rf1 <- randomForest(Species ~ ., iris)
iris_rf1$importance
# => Petal 관련 2개 변수가 중요도 훨씬 높음
iris_rfimp <- iris[ , -c(1, 2)]

    # step 1) 군집분석 수행
    d1 <- dist(iris_rfimp[ , -3])
    # 1) 최단거리법
    m1 <- hclust(d1, 'single')
    
    # 2) 최장거리법
    m2 <- hclust(d1, 'complete')
    
    # 3) 평균법
    m3 <- hclust(d1, 'average')
    
    # step 2) 시각화
    par(mfrow = c(1, 3))
    plot(m1, hang = -1, main = 'single')
    rect.hclust(m1, 3)
    plot(m2, hang = -1, main = 'complete')
    rect.hclust(m2, 3)
    plot(m3, hang = -1, main = 'average')
    rect.hclust(m3, 3)

    # step 4) 평가
    # iris 데이터의 정답(Y)을 알고 있기 때문에 분류분석의 평가 metric에 의해 평가 시도
    # 정상적인 방식은 아님
    # 1) single
    cutree(m1, k = 3)    # 모델을 통해 나눠진 군집의 유형
    g1 <- ifelse(iris$Species == 'setosa', 1, ifelse(iris$Species == 'versicolor', 2, 3))
    sum(cutree(m1, k = 3) == g1) / nrow(iris) * 100    # 66%
    
    # 2) complete
    cutree(m2, k = 3)    # 모델을 통해 나눠진 군집의 유형
    # => 시각화를 보았을 때 2번보다 3번 클래스가 많음 -> species명을 바꿔줌
    g2 <- ifelse(iris$Species == 'setosa', 1, ifelse(iris$Species == 'versicolor', 3, 2))
    sum(cutree(m2, k = 3) == g2) / nrow(iris) * 100    # 86%
    # => 이에 따라 single도 순서 변경해보면... 그래도 비슷함
    cutree(m1, k = 3)    # 모델을 통해 나눠진 군집의 유형
    g1 <- ifelse(iris$Species == 'setosa', 1, ifelse(iris$Species == 'versicolor', 3, 2))
    sum(cutree(m1, k = 3) == g1) / nrow(iris) * 100    # 67%
    
    # 3) average
    cutree(m3, k = 3)    # 모델을 통해 나눠진 군집의 유형
    # => 시각화를 보았을 때 우리가 알던 순서대로 1, 2, 3인듯
    g3 <- ifelse(iris$Species == 'setosa', 1, ifelse(iris$Species == 'versicolor', 2, 3))
    sum(cutree(m3, k = 3) == g3) / nrow(iris) * 100    # 96% **

# 2) 설명변수 scaling with 2개의 중요한 설명변수
iris_rfimp_sc <- scale(iris_rfimp[ , -3])

    # step 1) 군집분석 수행
    d1 <- dist(iris_rfimp_sc[ , -3])
    # 1) 최단거리법
    m1 <- hclust(d1, 'single')
    
    # 2) 최장거리법
    m2 <- hclust(d1, 'complete')
    
    # 3) 평균법
    m3 <- hclust(d1, 'average')
    
    # step 4) 평가
    # iris 데이터의 정답(Y)을 알고 있기 때문에 분류분석의 평가 metric에 의해 평가 시도
    # 정상적인 방식은 아님
    # 1) single
    cutree(m1, k = 3)    # 모델을 통해 나눠진 군집의 유형
    g1 <- ifelse(iris$Species == 'setosa', 1, ifelse(iris$Species == 'versicolor', 2, 3))
    sum(cutree(m1, k = 3) == g1) / nrow(iris) * 100    # 67%
    
    # 2) complete
    cutree(m2, k = 3)    # 모델을 통해 나눠진 군집의 유형
    # => 시각화를 보았을 때 2번보다 3번 클래스가 많음 -> species명을 바꿔줌
    g2 <- ifelse(iris$Species == 'setosa', 1, ifelse(iris$Species == 'versicolor', 3, 2))
    sum(cutree(m2, k = 3) == g2) / nrow(iris) * 100    # 83%
    # => 이에 따라 single도 순서 변경해보면... 그래도 비슷함
    cutree(m1, k = 3)    # 모델을 통해 나눠진 군집의 유형
    g1 <- ifelse(iris$Species == 'setosa', 1, ifelse(iris$Species == 'versicolor', 3, 2))
    sum(cutree(m1, k = 3) == g1) / nrow(iris) * 100    # 66%
    
    # 3) average
    cutree(m3, k = 3)    # 모델을 통해 나눠진 군집의 유형
    # => 시각화를 보았을 때 우리가 알던 순서대로 1, 2, 3인듯
    g3 <- ifelse(iris$Species == 'setosa', 1, ifelse(iris$Species == 'versicolor', 2, 3))
    sum(cutree(m3, k = 3) == g3) / nrow(iris) * 100    # 98% ***
    
    # step 5) 적절한 군집의 수 정하기
    iris_nb1 <- NbClust(iris_rfimp_sc, min.nc = 2, max.nc = 10,
                        method = 'average')
    
    # * Among all indices:                                                
    # * 8 proposed 2 as the best number of clusters 
    # * 9 proposed 3 as the best number of clusters 
    # * 1 proposed 4 as the best number of clusters 
    # * 1 proposed 7 as the best number of clusters 
    # * 2 proposed 8 as the best number of clusters 
    # * 1 proposed 9 as the best number of clusters 
    # * 1 proposed 10 as the best number of clusters 
    # 
    # ***** Conclusion *****                            
    #     
    #     * According to the majority rule, the best number of clusters is  3 
    
    # 26개 각 지표값 확인
    iris_nb1$All.index
    # Dindex 지표 : 그룹 내 분산에 의해 계산된 지표
    # SDindex 지표 : 그룹 간 분산 및 각 중심의 평균으로부터 계산된 지표
    
    # 군집분석에서의 분산
    # 1. 총분산 (total_ss) : 그룹과는 관계없음
    # 2. 그룹 내 분산 (within_ss)
    # => 각 클러스터의 중심으로부터 각 클러스터 관측치가 떨어진 정도
    # 3. 그룹 간 분산 (between_ss)
    # => 각 클러스터의 중심이 전체중심으로부터 흩어진 정도
    
    # 해석)
    # 그룹 내 분산은 작을수록 그룹 간 분산은 클수록 좋은 군집
    # => 군집이 많아져도 그룹 내 분산은 작아짐 -> 과연 효과적인가?
    # => 군집이 많아질수록 그룹 내 분산은 당연히 작아짐
    # => 작아지는 기울기가 가장 클 때가 가장 효과적임
    # => How estimate? by Dindex (위 지표에서는 군집이 2->3일 때)
    
    # => 군집이 많아질수록 그룹 간 분산도 당연히 커짐
    # => 기울기가 가장 클 때가 가장 효과적임
    # => How estimate? by SDindex (위 지표에서는 군집이 2->3일 때)
    total_ss = between_ss + within_ss
    
# 최종결론
# 1) 설명변수 중요도 체크 후 average 통해 96% 예측력 나옴
# 2) 설명변수 scaling 후 average 통해 98% 예측력 나옴
# 3) 추가로 이상치에 대한 확인도 필요함
# 4) 군집수는 3개가 적당
# ------------------------------------------------------------------------------
    
# 1.2 비계층적 군집분석
# - 거리가 짧은 데이터포인트끼리 묶는 방식은 동일
# - 계층적 군집분석과는 다르게 한번 클러스터에 소속된 관측치도 거리 측정 대상에
#   포함시켜 만약 다른 클러스터와의 거리가 더 짧다면 다른 클러스터로 이동되는 방식
# - 평가 metric 존재 (그룹 내 / 그룹 간 분산에 의한 score)
# - 사용자가 직접 군집의 수를 정하면 해당 군집에 맞게 분류
    
# [ 예제 - iris 데이터를 사용한 비계층적 군집분석 (k-means) ]
# 1) 군집분석 수행 (모델링)
# knn - 분류분석 => Y 예측
# kmeans - 군집분석 => 데이터 세분화

kmeans(x,             # 원본 data (거리행렬 X)
       centers = )    # 군집 수
kmean1 <- kmeans(iris[ , -5], 3)

# 2) 모델 확인 및 평가
# 랜덤 추출이라 실행마다 결과 다름
kmean1

# K-means clustering with 3 clusters of sizes 62, 38, 50   # 각 군집의 데이터 개수
# 
# Cluster means:
#     Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1     5.901613    2.748387     4.393548    1.433871  # n번 군집의 각 컬럼 평균
# 2     6.850000    3.073684     5.742105    2.071053
# 3     5.006000    3.428000     1.462000    0.246000

# Within cluster sum of squares by cluster:
#     [1] 39.82097 23.87947 15.15100    # 각 군집의 그룹 내 분산
# (between_SS / total_SS =  88.4 %)     # 88.4% 좋다

# totss
kmean1$totss           # 681.3706 => 총분산은 항상 같음

# withinss
sum(kmean1$withinss)   # 78.85144

# betweenss
kmean1$betweenss       # 602.5192

# totss = withinss + betweenss
# scaling 하지 않은 상태에서 위 수치를 비교하기는 무리가 있음 -> 총분산으로 나눔
# => 계산하기 쉽게 커질수록 좋은 betweenss를 사용 (between_SS / total_SS =  88.4 %)
    
# 3) k수 변화에 따른 between_SS / total_ss의 변화 확인
k1 <- c()
for (i in 2:10) {
    kmean_1 <- kmeans(iris[ , -5], i)
    k1 <- c(k1, kmean_1$betweenss / kmean_1$totss)
}
plot(2:10, k1, type = 'o', xlab = 'k의 수',
     ylab = 'score', main = '군집 수 변화에 따른 설명력')
# 군집의 개수가 증가할수록 설명력은 증가할 수 밖에 없지만, 3이나 4정도가 적당해보임 

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

# -------------------------------연 습 문 제------------------------------------
# 연습문제 8. iris data의 더 효과적인 비계층적 군집분석을 위한 방안 모색
# step 1) 설명변수 중요도 체크 후 average 통해 96% 예측력 나옴
iris_rfimp <- iris[ , -c(1, 2)]
kmean1_rfimp <- kmeans(iris_rfimp[ , -3], 3)    # 94.3%

# step 2) 설명변수 scaling 후 average 통해 98% 예측력 나옴
iris_rfimp_sc <- scale(iris_rfimp[ , -3])
kmean1_rfimp_sc <- kmeans(iris_rfimp_sc, 3)    # 94%

# step 4) k수 변화에 따른 between_SS / total_ss의 변화 확인
k1 <- c()
for (i in 1:10) {
    kmean_1 <- kmeans(iris_rfimp_sc, i)
    k1 <- c(k1, kmean_1$betweenss / kmean_1$totss)
}
plot(1:10, k1, type = 'o', xlab = 'k의 수',
     ylab = 'score', main = '군집 수 변화에 따른 설명력')
# k값이 2->3일 때가 가장 좋아보임 => 3

# 결론
# => 중요 설명변수, 표준화 처리 시 예측력 높아짐
# ------------------------------------------------------------------------------