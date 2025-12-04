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


# 변수명  | 의미                           
# `sr`    | 개인 저축률
# `pop15` | 15세 미만 인구 비율               
# `pop75` | 75세 이상 인구 비율               
# `dpi`   | 1인당 실질 가처분소득 달러 단위
# `ddpi`  | 실질 가처분소득 증가율  


#핵심 변수만 선택
df <- LifeCycleSavings[, c("sr", "dpi", "ddpi", "pop15", "pop75")]

# 변수명 바꾸기
colnames(df) <- c("저축률", "가처분소득", "가처분소득 증가율" ,"젊은층비율", "노년층비율")

# 1) 상관계수 계산
cor_matrix <- cor(df)

# 2) 상관계수 출력
print(round(cor_matrix, 3))

ggcorrplot(
  cor_matrix,
  method = "circle",
  type = "lower",
  lab = TRUE,
  lab_size = 3.0,          # 숫자 글자 크기 조금 줄임
  tl.cex = 9,             # 축 글씨 크기
  tl.srt = 30,             # 축 글씨 기울기 조절 (겹침 방지)
  colors = c("#6D9EC1", "white", "#E46726"),
  title = "LifeCycleSavings: 상관관계 분석",
  ggtheme = ggplot2::theme_minimal(base_size = 14)
) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    plot.margin = margin(20, 20, 20, 20),  # 여백 넉넉하게
    
    #범례 크기 줄이기
    legend.key.size = unit(0.4, "cm"),   # 막대 폭 줄임
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )



'
📌 분석 결론

1. sr ↔ dpi (저축률 vs 가처분소득)
➡️ 상관계수: 약 +0.30 내외 (약한 양의 상관)

가처분소득이 높을수록 저축률이 조금 올라가는 경향이 있지만 상관이 강하진 않음
즉, 부유한 나라라고 반드시 저축률이 높은 건 아니다.
소득 자체보다는 소득 증가율이 저축률에 훨씬 더 큰 영향을 준다는 게 중요 포인트.


2. sr ↔ ddpi (저축률 vs 실질 소득 증가율)
➡️ 상관계수: 약 +0.55 ~ +0.60 (가장 강한 양의 상관)

경제성장이 빠른 나라일수록 저축률이 높다.
미래 소득이 증가할 거라는 기대 → 현재 소비를 미루고 저축 증가.
이 데이터에서 가장 강한 상관관계로 나타나며 경제학 내용과도 일치.
✔ 사실상 "성장률이 높은 나라 → 저축률도 높다" 는 결론이 명확하게 나타남.


3. sr ↔ pop15 (저축률 vs 15세 이하 인구 비율)
➡️ 상관계수: 약 –0.40 ~ –0.50 (중간 정도의 음의 상관)

젊은층 비중이 많으면 저축률이 떨어지는 경향이 있다.
   (이유: 부양해야 할 아이가 많으면 지출이 많아지고 → 가구 저축 감소)


4. sr ↔ pop75 (저축률 vs 75세 이상 인구 비율)
➡️ 상관계수: 약 –0.2 내외 (약한 음의 상관)

노년층 비중이 높아질수록 저축률은 줄어드는 경향이 있으나 젊은층에 비해서는 훨씬 약함
고령층의 저축 감소 영향은 분명 있지만 데이터상 강하지 않은 편.

**노년층의 영향보다 젊은층의 영향이 강함

''
