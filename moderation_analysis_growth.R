# MAC 한글 설정
install.packages("showtext")
library(showtext)

showtext_auto(enable = TRUE)
font_add(family = "NanumGothic", regular = "/System/Library/Fonts/AppleSDGothicNeo.ttc")


# 내장 데이터셋 불러오기
data("LifeCycleSavings")
df <- LifeCycleSavings

# 확인
summary(df)

'
| 변수명  | 의미                           
| `sr`    | 개인 저축률
| `pop15` | 15세 미만 인구 비율               
| `pop75` | 75세 이상 인구 비율               
| `dpi`   | 1인당 실질 가처분소득 달러 단위
| `ddpi`  | 실질 가처분소득 증가율  
'


# 양(+) → 성장률이 높을수록 00층의 저축률 감소효과가 약해짐
# 음(-) → 성장률이 높을수록 00층의 저축률 감소효과가 더 커짐


# 상호작용 모형 1 (젊은층 × 성장률)
model_interact_15 <- lm(sr ~ pop15 * ddpi, data = df)
summary(model_interact_15)


# 상호작용 모형 2 (노년층 × 성장률)
model_interact_75 <- lm(sr ~ pop75 * ddpi, data = df)
summary(model_interact_75)



#시각화
library(ggplot2)

# 성장률을 2구간으로 나눔
df$ddpi_group <- cut(df$ddpi,
                     breaks = 2,
                     labels = c("성장률 낮은 나라", "성장률 높은 나라"))


# 젊은층 × 성장률 상호작용 시각화
ggplot(df, aes(x = pop15, y = sr, color = ddpi_group)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "경제성장률이 젊은층-저축률 관계에 미치는 조절효과",
       x = "젊은층 비율",
       y = "저축률",
       color = "경제성장률 구간") +
  theme_minimal(base_family = "NanumGothic")  # 한글 폰트 지정

'
# 노년층 × 성장률 상호작용 시각화
ggplot(df, aes(x = pop75, y = sr, color = ddpi_group)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "경제성장률이 노년층-저축률 관계에 미치는 조절효과",
       x = "노년층 비율",
       y = "저축률",
       color = "경제성장률 구간") +
  theme_minimal(base_family = "NanumGothic")  # 한글 폰트 지정
'


'
경제성장률이 낮은 나라는 젊은층의 비율이 많아질수록 저축률이 떨어짐
경제성장률이 높은 나라는 젊은층의 비율이 많아도 저축률의 감소폭이 완화됨
노년층이 저축률이 높다는 전제이기 때문에 노년층은 시각화가 필요하지 않음
경제성장률이 높을수록, 젊은층이 많아도 저축률이 크게 떨어지지 않는다는 가설을 입증
'
