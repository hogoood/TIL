library(tidyverse)
install.packages("psych")

library(psych)

getwd()
setwd(dir = './data')
list.files(pattern = 'RDS')
df <- readRDS(file = 'Toyota.RDS')

# 선형회귀모형 실습

glimpse(x = df)
describe(x = df) # 기술통계량 출력

## 분석데이터 분할
set.seed(seed = 1234)
index <- sample(x = nrow(x = df), size = nrow(x = df) * 0.7, replace = FALSE)


## 'index'를 기준으로 훈련셋과 시험셋으로 분리
trainSet <- df %>% slice(index)
testSet <-  df %>% slice(-index)

## 훈련용 및 시험용 데이터셋의 목표변수 평균을 비교
trainSet$Price %>% mean()
testSet$Price %>% mean()

## 단순선형회귀모형 적합 및 결과 확인
fit1 <- lm(formula = Price ~ Age, data = trainSet)
summary(object = fit1)

#### F-statistic:  3573 on 1 and 990 DF,  p-value: < 2.2e-16 회귀모형 유의성 검정 통과! (0.05 보다 작다)

#### Coefficients:  회귀계수 t-검정 유의확률이 작은지 확인. 유의성 검정 통과!
#### Estimate Std. Error t value Pr(>|t|)    
#### (Intercept) 19805.941    163.215  121.35   <2e-16 ***
####  Age          -163.819      2.741  -59.77   <2e-16 ***
####  ---
####  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#### Multiple R-squared:  0.783  확인

## 잔차 가정 검정 그래프
par(mfrow = c(2,2)) # 그래프를 4개로 나눠 보여줘라
plot(x = fit1)

par(mfrow = c(1,1)) # Plots 창 분할 원복

## 잔차의 정규성 검정
hist(x = fit1$residuals, freq = FALSE)
lines(x = density(x = fit1$residuals), col = 'red', lwd = 2)

shapiro.test(x = fit1$residuals) #0.05보다 커야 귀무가설(정규성 검정의 귀무가설은 정규분포한다) 기각 안됨

## 잔차 가정 검정 관련 함수
install.packages('car')
library(car)

### 등분산검정
ncvTest(model = fit1) #귀무가설이 등분산하다 이므로 0.05보다 커야함
durbinWatsonTest(model = fit1) # 상관계수가 0이면 자기상관이 없다... 무슨소리야
crPlots(model = fit1) # 분홍색 실선이 U자형 곡선으로 그려지면 2차항을 추가해야하지만 그보다 입력변수를 늘리는게 낫다. _강사님
influencePlot(model = fit1) # 이상치 확인 - 쿡의 거리

### 2차항을추가한 회귀모형 적합
fit2 <- lm(formula = Price ~ Age + I(x = Age^2), data = trainSet)
summary(object = fit2)
crPlots(model = fit2) # 기술적으로 맞출수는 있으나 의미는 없다. 현업에서 어떻게 쓸거냐~
vif(mode = fit2) # 분산팽창지수 확인 -> 다중공산성에 문제가 됨

### 2차항을 편차제곱으로 바꾸면 다중공산성 문제 해결됨
avg <- mean(x = trainSet$Age)
fit3 <- lm(formula = Price ~ Age + I(x = (Age-avg)^2, data = trainSet))
rm(fit2)

### 추정값 산출 (잔차과정까지 모두 통과되면 글로벌하게 모형활용가능하지만 안되면 제한적으로만 사용가능)
real <- testSet$Price
pred1 <- predict(object = fit1, newdata = testSet, type = 'response')
error1 <- real - pred1

### 회귀모형의 성능 측정
error1^2 %>% mean()
error1^2 %>% mean() %>% sqrt()
error1 %>% abs() %>% mean()
(abs(error1) / abs(real)) %>% mean()
# mean(abs(error1) / abs(real))


# k - 교차검증 
x <- 'Age'
y <- 'Price'
str_glue('{y} ~ {x}')
parse(text = str_glue('{y} ~ {x}'))
eval(expr = parse(text = str_glue('{y} ~ {x}')))


crossValidation <- function(x, y, data, k = 5, seed = 1234){
  set.seed(seed = seed)
  index <- sample(x = 1:k, size = nrow(x = data), replace = TRUE)
  formula <- str_glue('{y} ~ {x}') %>% parse(text = .) %>% eval()
  errors <- c()
  for(i in 1:k) {
    train <- data %>% filter(index != i)
    valid <- data %>% filter(index == i)
    fit <- lm(formula = formula, data = train)
    real <- str_glue('valid${y}') %>% parse(text = .) %>% eval()
    pred <- predict(object = fit, newdata = valid, type = 'response')
    errors[i] <- (real ~ pred)^2 %>% mean() %>% sqrt()
  }
  return(mean(x = errors))
}

crossValidation(x = 'Age', y = 'Price', data = trainSet)
crossValidation(x = 'KM', y = 'Price', data = trainSet)
crossValidation(x = 'CC', y = 'Price', data = trainSet)
crossValidation(x = 'Doors', y = 'Price', data = trainSet)




# 다중선형회귀분석
full <-  lm(formula = Price ~ ., data = trainSet)
null <-  lm(formula = Price ~ 1, data = trainSet)

fit2 <- step(object = null,
             scope = list(lower = null, upper = full),
             direction = 'both')

summary(fit2)

par(mfrow = c(2,2))
plot(x = fit2)
par(mfrow = c(1,1))

shapiro.test(x = fit2$residuals) #정규성
ncvTest(model = fit2) #등분산
durbinWatsonTest(model = fit2) 
crPlots(model = fit2) #선형성
influencePlot(model = fit2) #이상치확인
