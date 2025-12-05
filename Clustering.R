library(ggplot2)
library(dplyr)
library(ggrepel)

data("LifeCycleSavings")
head(LifeCycleSavings)

# 분석에 사용할 변수 선택
df <- LifeCycleSavings[, c("sr", "pop15", "pop75", "dpi", "ddpi")]

# 스케일링
df_scaled <- scale(df)

# -------------------------
# 1) 엘보우 메소드
# -------------------------
wss <- numeric(10)
for (k in 1:10) {
  wss[k] <- sum(kmeans(df_scaled, centers = k, nstart = 20)$withinss)
}

plot(1:10, wss, type = "b",
     xlab = "k (clusters)", ylab = "Within-cluster SSE")

set.seed(123)
km <- kmeans(df_scaled, centers = 3, nstart = 25)

# -------------------------
# 2) K-means 클러스터링
# -------------------------

set.seed(123)
km <- kmeans(df_scaled, centers = 3, nstart = 25)
km$cluster      # 각 국가가 어떤 클러스터인지
km$centers      # 클러스터 중심
table(km$cluster)


# 클러스터 이름 매핑
cluster_labels <- c(
  "1" = "저소득·저성장 + 젊은층 많은 국가",
  "2" = "고성장 + 고저축 국가",
  "3" = "고소득 + 고령화 국가"
)

# 클러스터 번호 추가
df_clustered <- cbind(df, cluster = km$cluster)
df_clustered$country <- rownames(LifeCycleSavings)

# -------------------------
# 3) 클러스터 특징 정량화
# -------------------------

cluster_summary <- df_clustered %>%
  group_by(cluster) %>%
  summarise(
    평균_sr = mean(sr),
    평균_pop15 = mean(pop15),
    평균_pop75 = mean(pop75),
    평균_dpi = mean(dpi),
    평균_ddpi = mean(ddpi)
  )

print("===== 클러스터별 평균값(특징 정량화) =====")
print(cluster_summary)


# -------------------------
# 4) PCA 차원축소 + 시각화
# -------------------------

pca <- prcomp(df_scaled)
pca_df <- data.frame(
  country = rownames(df),
  PC1 = pca$x[, 1],
  PC2 = pca$x[, 2],
  cluster = as.factor(km$cluster)
)

pca_df$cluster <- factor(
  pca_df$cluster,
  levels = c("1", "2", "3"),
  labels = c("저소득·저성장 + 젊은층 많은 국가", "고성장 + 고저축 국가", "고소득 + 고령화 국가")
)


ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = country), size = 3) +
  labs(
    title = "PCA 기반 국가 분포",
    x = "경제력·저축능력",
    y = "인구구조",
    color = "국가 유형"
  ) +
  theme_minimal()

# -------------------------
# 5) 성장률↑ + 저축률↓ 예외국가
# -------------------------

mean_ddpi <- mean(df$ddpi)
mean_sr <- mean(df$sr)

exceptions <- subset(df_clustered, ddpi > mean_ddpi & sr < mean_sr)

print("===== 성장률↑ + 저축률↓ 예외국가 =====")
print(exceptions[, c("country", "cluster", "ddpi", "sr")])

# -------------------------
# 6) 예외국가 PCA에서 강조
# -------------------------

exception_list <- exceptions$country

pca_df$exception <- ifelse(pca_df$country %in% exception_list, TRUE, FALSE)

ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  geom_point(data = pca_df %>% filter(exception == TRUE),
             color = "#000000", size = 4) +
  geom_text_repel(data = pca_df %>% filter(exception == TRUE),
                  aes(label = country),
                  size = 4, color = "#000000") +
  labs(
    title = "예외국가를 포함한 PCA 시각화",
    x = "경제력·저축능력",
    y = "인구구조",
    color = "국가 유형"
  ) +
  theme_minimal()


