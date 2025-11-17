# MAC 한글 설정
par(family = "NanumGothic")

# 내장 데이터셋 불러오기
data("LifeCycleSavings")

# 1960년대 50개국의 경제적 지표로,
# 각 나라의 저축률, 소득 수준, 인구 성장률, 연령 구조 변화 등을 포함한다.


# 데이터 미리보기
head(LifeCycleSavings)

# 구조와 변수 타입 확인
str(LifeCycleSavings)

# 기초 요약 통계량
summary(LifeCycleSavings)

'
| 변수명  | 의미                           
| `sr`    | 개인 저축률
| `pop15` | 15세 미만 인구 비율               
| `pop75` | 75세 이상 인구 비율               
| `dpi`   | 1인당 실질 가처분소득 달러 단위  
| `ddpi`  | 실질 가처분소득 증가율     
'


# 원본 데이터 복사
df <- LifeCycleSavings[, c("sr", "pop15", "pop75")]

# 변수명 바꾸기
colnames(df) <- c("저축률", "젊은층비율", "노년층비율")


# 경제성장률과 저축률의 관계 확인
plot(LifeCycleSavings$ddpi, LifeCycleSavings$sr,
     xlab = "경제성장률", ylab = "저축률",
     main = "경제성장률과 저축률의 관계",
     pch = 19, col = "steelblue")

# 상관관계 보기
cor(LifeCycleSavings$ddpi, LifeCycleSavings$sr)   # 0.3047872

'
점들이 오른쪽 위로 올라감 : 양의 상관관계
→ 성장률 높을수록 저축률 높음
'

# 저축률↔젊은층, 저축률↔노년층, 젊은층↔노년층 관계 확인
pairs(df,
      main = "저축률과 인구 구조 간 관계",
      pch = 19, col = "steelblue")


# 상관관계 행렬 계산
cor_matrix <- cor(df)
round(cor_matrix, 3)  # 소수점 3자리까지만 보기


'
점이 왼쪽 위 → 오른쪽 아래로 기움 : 음의 상관 관계
젊은층 비율이 높을수록 저축률이 낮다 -> 생애주기 가설과 일치

저축률 ↔ 젊은층비율	-0.456	젊은층이 많을수록 저축률이 낮아지는 경향 (음의 상관)
저축률 ↔ 노년층비율 +0.317 노년층이 많을수록 저축률이 조금 높아지는 경향 (약한 양의 상관)
젊은층비율 ↔ 노년층비율 -0.908 젊은층이 많으면 노년층은 적은, 매우 강한 음의 상관 (거의 반비례)
'

##젊은층과 저축률

# (1) 선형 모형
model_linear <- lm(sr ~ pop15, data = LifeCycleSavings)

# (2) 비선형 모형 (2차항 추가)
model_quad <- lm(sr ~ poly(pop15, 2, raw = TRUE), data = LifeCycleSavings)

# 결과 비교
summary(model_linear)
summary(model_quad)

# 산점도
plot(LifeCycleSavings$pop15, LifeCycleSavings$sr,
     pch = 19, col = "steelblue",
     xlab = "젊은층 비율 (%)",
     ylab = "저축률 (%)",
     main = "젊은층 비율과 저축률의 관계 (비선형 확인)")

# 예측선 (2차 곡선)
x_seq <- seq(min(LifeCycleSavings$pop15), max(LifeCycleSavings$pop15), length.out = 100)
y_pred <- predict(model_quad, newdata = data.frame(pop15 = x_seq))
lines(x_seq, y_pred, col = "red", lwd = 2)



# 노년층과 저축률

# 선형 vs 비선형 모형
model75_linear <- lm(sr ~ pop75, data = LifeCycleSavings)
model75_quad <- lm(sr ~ poly(pop75, 2, raw = TRUE), data = LifeCycleSavings)

# 비교
summary(model75_linear)
summary(model75_quad)

# 시각화
plot(LifeCycleSavings$pop75, LifeCycleSavings$sr,
     pch = 19, col = "steelblue",
     xlab = "노년층 비율 (%)",
     ylab = "저축률 (%)",
     main = "노년층 비율과 저축률의 관계 (비선형 확인)")

x_seq <- seq(min(LifeCycleSavings$pop75), max(LifeCycleSavings$pop75), length.out = 100)
y_pred <- predict(model75_quad, newdata = data.frame(pop75 = x_seq))
lines(x_seq, y_pred, col = "red", lwd = 2)

