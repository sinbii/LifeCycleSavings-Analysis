# 내장 데이터 불러오기
data("LifeCycleSavings")
View(LifeCycleSavings)
length(LifeCycleSavings)
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

pca <- prcomp(df_scaled)
pca_df <- data.frame(pca$x[, 1:2], cluster = factor(km$cluster))

ggplot(pca_df, aes(PC1, PC2, color = cluster)) +
  geom_point(size = 3) +
  theme_minimal()

# 빨 - 소득, 성장률, 저축률이 높은 국가
# 초 - 인구 구조가 특이한 국가 (고령화 등)
# 파 - 소득, 성장률, 저축률이 낮은 국가

# >> 성장률은 높은데 저축률이 낮은 국가
cbind(LifeCycleSavings, cluster = km$cluster) |>
  subset(ddpi > median(ddpi) & sr < median(sr)) |>
  head()

# >> 예외적인 국가 찾기
# 평균 대비 예외적 국가, “경제 성장률 대비 저축률이 낮은” 예외적 사례
high_ddpi_low_sr <- subset(LifeCycleSavings, ddpi > mean(ddpi) & sr < mean(sr))
high_ddpi_low_sr
library(ggplot2)

# 예외 국가 표시
high_ddpi_low_sr <- subset(LifeCycleSavings, ddpi > mean(ddpi) & sr < mean(sr))

ggplot(LifeCycleSavings, aes(x = ddpi, y = sr)) +
  geom_point(color = "blue") +  # 전체 국가
  geom_point(data = high_ddpi_low_sr, aes(x = ddpi, y = sr), color = "red", size = 3) + # 예외 국가
  geom_smooth(method = "lm", se = FALSE, color = "black") + # 회귀선
  geom_text(data = high_ddpi_low_sr, aes(label = rownames(high_ddpi_low_sr)), 
            vjust = -1, color = "red", size = 3.5) + # 국가 이름 라벨
  labs(title = "경제 성장률(ddpi) vs 저축률(sr) (예외 국가 강조)",
       x = "경제 성장률 (ddpi)",
       y = "저축률 (sr)")


# >> 인구 구조가 원인인가?
cor(high_ddpi_low_sr$pop15, high_ddpi_low_sr$sr)
cor(high_ddpi_low_sr$pop75, high_ddpi_low_sr$sr)

lm_sr <- lm(sr ~ ddpi + pop15 + pop75, data = LifeCycleSavings)
summary(lm_sr)