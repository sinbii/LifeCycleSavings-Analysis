# ========================
#   LifeCycleSavings 클러스터 분석
# ========================

install.packages("ggrepel")
library(ggplot2)
library(dplyr)
library(ggrepel)

# 데이터 불러오기
data("LifeCycleSavings")

# 분석에 사용할 변수 선택
df <- LifeCycleSavings[, c("sr", "pop15", "pop75", "dpi", "ddpi")]

# 스케일링
df_scaled <- scale(df)

# -------------------------
# 1) K-means 클러스터링
# -------------------------

set.seed(123)
km <- kmeans(df_scaled, centers = 3, nstart = 25)


# 클러스터 이름 매핑
cluster_labels <- c(
  "1" = "고저축·고성장형",
  "2" = "인구구조 특수형",
  "3" = "저저축·저성장형"
)

# 클러스터 번호 추가
df_clustered <- cbind(df, cluster = km$cluster)
df_clustered$country <- rownames(LifeCycleSavings)


# -------------------------
# 2) PCA 차원축소 + 시각화
# -------------------------

pca <- prcomp(df_scaled)
pca_df <- data.frame(
  country = rownames(df),
  PC1 = pca$x[, 1],
  PC2 = pca$x[, 2],
  cluster = as.factor(km$cluster)
)

# 클러스터 범례 이름 변경
pca_df$cluster <- factor(
  pca_df$cluster,
  levels = c("1", "2", "3"),
  labels = c("고저축·저성장형", "저저축·고성장형", "중간형")
)


# PCA 그래프
ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = country), size = 3) +
  labs(
    title = "PCA 기반 국가 분포",
    x = "경제력·저축능력(PC1)",
    y = "인구구조 패턴(PC2)",
    color = "국가 유형"
  ) +
  theme_minimal()

# -------------------------
# 3) 성장률↑ + 저축률↓ 예외국가
# -------------------------

mean_ddpi <- mean(df$ddpi)
mean_sr <- mean(df$sr)

exceptions <- subset(df_clustered, ddpi > mean_ddpi & sr < mean_sr)

print("===== 성장률↑ + 저축률↓ 예외국가 =====")
print(exceptions[, c("country", "cluster", "ddpi", "sr")])

# -------------------------
# 4) 예외국가 PCA에서 강조
# -------------------------

exception_list <- exceptions$country

pca_df$exception <- ifelse(pca_df$country %in% exception_list, TRUE, FALSE)

ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  geom_point(data = pca_df %>% filter(exception == TRUE),
             color = "red", size = 4) +
  geom_text_repel(data = pca_df %>% filter(exception == TRUE),
                  aes(label = country),
                  size = 4, color = "red") +
  labs(
    title = "PCA 시각화 (예외국가 강조)",
    x = "PC1 (경제력·저축능력)",
    y = "PC2 (인구구조 패턴)",
    color = "국가 유형"
  ) +
  theme_minimal()

# -------------------------
# 5) 클러스터 특징 정량화
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

# 클러스터 중심 (표준화된 값)
print("===== 클러스터 중심(표준화된 값, 어떤 특성에 강한지) =====")
print(km$centers)

