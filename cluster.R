# 내장 데이터 불러오기
data("LifeCycleSavings")

# 데이터 확인
head(LifeCycleSavings)
str(LifeCycleSavings)


# >> 클러스터링
data("LifeCycleSavings")
head(LifeCycleSavings)

df <- LifeCycleSavings[, c("sr", "pop15", "pop75", "dpi", "ddpi")]

df_scaled <- scale(df)

wss <- numeric(10)
for (k in 1:10) {
  wss[k] <- sum(kmeans(df_scaled, centers = k, nstart = 20)$withinss)
}

plot(1:10, wss, type = "b",
     xlab = "k (clusters)", ylab = "Within-cluster SSE")

set.seed(123)
km <- kmeans(df_scaled, centers = 3, nstart = 25)

km$cluster      # 각 국가가 어떤 클러스터인지
km$centers      # 클러스터 중심
table(km$cluster)

library(ggplot2)
library(dplyr)

pca <- prcomp(df_scaled)
pca_df <- data.frame(
  경제력_저축능력 = pca$x[, 1],  
  인구구조 = pca$x[, 2],        
  cluster = factor(km$cluster,
                   levels = c(1, 2, 3),
                   labels = c("고저축형", "인구구조특수형", "저저축형"))
)
ggplot(pca_df, aes(x = 경제력_저축능력, y = 인구구조, color = cluster)) +
  geom_point(size = 3) +
  labs(
    x = "경제력·저축능력",
    y = "인구구조",
    color = "cluster"
  ) +
  theme_minimal()



#클러스터 결과 추가
df_clustered <- cbind(LifeCycleSavings, cluster = km$cluster)
df_clustered$country <- rownames(df_clustered)

#예외국가 찾기
#성장률↑ + 저축률↓ 예외 국가
high_ddpi_low_sr <- subset(df_clustered,
                           ddpi > mean(ddpi) & sr < mean(sr))
print("성장률 ↑ + 저축률 ↓ 예외 국가 (클러스터 포함):")
print(high_ddpi_low_sr[, c("country", "cluster", "ddpi", "sr")])


#예외국가 클러스터에 표시
library(ggplot2)
library(dplyr)
library(ggrepel)  # 겹치지 않는 라벨용

# PCA 데이터에 국가명 추가
pca_df <- data.frame(
  country = rownames(df_scaled),
  경제력_저축능력 = pca$x[, 1],  
  인구구조 = pca$x[, 2],        
  cluster = factor(km$cluster,
                   levels = c(1, 2, 3),
                   labels = c("고저축형", "인구구조특수형", "저저축형"))
)

# 예외국가 선택
exception_countries <- c("Korea", "Jamaica", "Libya", "Malaysia")
exceptions <- pca_df %>% filter(country %in% exception_countries)

# 그래프
ggplot(pca_df, aes(x = 경제력_저축능력, y = 인구구조, color = cluster)) +
  geom_point(size = 3) +  # 모든 국가
  geom_point(data = exceptions, color = "red", size = 4) +  # 예외국가 강조
  geom_text_repel(data = exceptions, aes(label = country),
                  color = "red", size = 4) +  # 예외국가 라벨
  labs(
    x = "경제력·저축능력",
    y = "인구구조",
    color = "cluster",
    title = "PCA 결과와 클러스터별 국가"
  ) +
  theme_minimal()


