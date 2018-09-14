#============
#18.1 요약 통계
#============

x <- sample(x = 1:100, size = 100, replace = TRUE)

mean(x)

mean # 함수 내용을 알아보고자 할 때
methods(mean) # 클래스에 따른 함수들
mean.default

y <- x
y[sample(x = 1:100, size = 20, replace = FALSE)] <- NA

mean(y)

mean(y, na.rm=TRUE)

grades <- c(95, 72,87, 65)
weights <- c(1/2, 1/4, 1/8, 1/8)
mean(grades)
weighted.mean(x = grades, w = weights)

weighted.mean
methods(weighted.mean) # 별표가 있는 함수는 바로 접근할 수 없다.
getAnywhere(weighted.mean)  # 별표가 있는 함수를 접근하는 방법

var(x)
sum((x-mean(x))^2)/(length(x)-1)

sqrt(var(x))
sd(x)
sd(y)
sd(y, na.rm=T)

min(x)
max(x)
median(x)
mean(x, trim=0.5)

min(y)
min(y, na.rm=T)

summary(x)
summary(y)

quantile(x, probs=c(0.25, 0.75)) # quantile : 분위수
quantile(y, probs=c(0.25, 0.75))

z <- quantile(x, probs=c(0.25, 0.75)); str(z) # quantile의 반환값의 구조
z["25%"] # 벡터의 각 원소에 붙여진 이름을 활용하는 방법

quantile(y, probs = c(0.25, 0.75), na.rm=T)
quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.99))

# 요약 통계치를 구할 수 있는 여러 가지 함수들
#summary
#psych::describe
#Hmisc::describe
#pastecs::stat.desc

lapply(mtcars, mean) # 각 열에 대해 평균값 구하기
lapply(mtcars, mean, na.rm=T) 
lapply(mtcars, function(x) mean(x, na.rm=T))
lapply(mtcars, function(x) c(mean=mean(x), median=median(x)))
data.frame(lapply(mtcars, function(x) c(mean=mean(x), median=median(x))))

#===============
#18.2 상관과 공분산
#===============

library(ggplot2)
head(economics)

cor(economics$pce, economics$psavert)
with(economics, cor(pce, psavert))

xPart <- economics$pce - mean(economics$pce)
yPart <- economics$psavert - mean(economics$psavert)
nMinusOne <- (nrow(economics)-1)
xSD <- sd(economics$pce)
ySD <- sd(economics$psavert)
sum(xPart*yPart)/(nMinusOne*xSD*ySD)

# scale 함수를 활용하여 상관계수 구하기
xPart <- scale(economics$pce, scale=F)
yPart <- scale(economics$psavert, scale=F)
sum(xPart*yPart)/(nMinusOne*xSD*ySD)
sum(scale(economics$pce)*scale(economics$psavert))/nMinusOne

cor(economics[, c(2, 4:6)]) 
cor(economics[, c(2, 4, 5, 6)])
cor(economics[, colnames(economics) != "date"])

GGally::ggpairs(economics[, c(2, 4:6)]) + 
  ggplot2::theme(axis.text = ggplot2::element_text(size=2))

library(reshape2)
library(scales)

econCor <- cor(economics[, c(2, 4:6)])
econMelt <- melt(econCor, varnames = c("x", "y"), value.name = "Correlation")
econMelt

ggplot(econMelt, aes(x=x, y=y)) + 
  geom_tile(aes(fill = Correlation)) + 
  scale_fill_gradient2(low=muted("red") , mid="white", high="steelblue",
  guide=guide_colourbar(ticks=F, barheight=10),
  limits=c(-1,1)) + 
  theme_minimal() + 
  labs(x=NULL, y=NULL)

ggplot(econMelt, aes(x=x, y=y)) + 
  geom_tile(aes(fill = Correlation)) + 
  scale_fill_gradient2(low=muted("red") , mid="white", high="steelblue",  
  limits=c(-1,1)) + 
  theme_minimal() + 
  labs(x=NULL, y=NULL)

ggplot(econMelt, aes(x=x, y=y)) + 
  geom_tile(aes(fill = Correlation)) + 
  scale_fill_gradient2(low=muted("red") , mid="white", high="steelblue",  
  guide = "none",
  limits=c(-1,1)) + 
  theme_minimal() + 
  labs(x=NULL, y=NULL)

# cor 함수의 use 인자로 쓸 수 있는 값들:
#   "all.bos", "complete.obs", "pairwise.complete.obs", "everything",
#   "na.or.completed"

library(RXKCD) # install.packages("RXKCD")
getXKCD(which="552")

cat("Press enter to see the next picture, or 'z' to exit\n")
i = 1
repeat {
  getXKCD(i)
  i = i + 1
  scan()
}


#==========
#18.3 t-검정
#==========
library(reshape2)
head(tips)
unique(tips$sex)
tips$sex[!duplicated(tips$sex)] # duplicated
unique(tips$day)

t.test(tips$tip, alternative = "two.sided", mu=2.50) # two.sided, less, greater

t.test(tip ~ sex , data=tips)
t.test(x=tips$tip[tips$sex=="Male"], y=tips$tip[tips$sex=="Female"])

randT <- rt(30000, df = NROW(tips)-1)
tipTTest <- t.test(tips$tip, alternative = 'two.sided', mu=2.5)

ggplot(data.frame(x=randT)) + 
  geom_density(aes(x=x), fill="grey", color="grey") + 
  geom_vline(xintercept = tipTTest$statistic) + 
  geom_vline(xintercept = mean(randT) + c(-2, 2) *sd(randT), linetype=2)

aggregate(tip ~ sex, data = tips, var)

shapiro.test(tips$tip)


#===========
#19.2 다중회귀
#===========
library(dplyr)
housing <- read.table('https://www.jaredlander.com/data/housing.csv',
sep=',', header=T, stringsAsFactors = F)

summary(housing)
table(housing$Boro)
lapply(housing %>% select(Neighborhood, Building.Classification, Boro), table)

names(housing) <- c('Neightborhood', 'Class', 'Units', 'YearBuilt',
'SqFt', 'Income', 'IncomePerSqFt', 'Expense', 
'ExpensePerSqFt', 'NetIncome', 'Value', 
'ValuePerSqFt', 'Boro')

ggplot(housing, aes(x=ValuePerSqFt)) + geom_histogram(binwidth=10) +
  labs(x="Value per Square Foot", title="NYC Open Data: NY Condo 2011-2012")

ggplot(housing, aes(x=ValuePerSqFt, fill=Boro)) +
  geom_histogram(binwidth=10) + labs(x="Value per Square Foot")

ggplot(housing, aes(x=ValuePerSqFt, fill=Boro)) + 
  geom_histogram(binwidth = 10) + labs(x="Value per Square Foot") +
  facet_wrap(~Boro)

ggplot(housing, aes(x=ValuePerSqFt, fill=Boro)) + 
  geom_histogram(binwidth = 10) + labs(x="Value per Square Foot") +
  facet_grid(.~Boro)

ggplot(housing, aes(x=ValuePerSqFt, fill=Boro)) + 
  geom_histogram(binwidth = 10) + labs(x="Value per Square Foot") +
  facet_grid(Boro~.) + guides(fill="none")

ggplot(housing, aes(x=SqFt)) + geom_histogram()

ggplot(housing, aes(x=Units)) + geom_histogram()

ggplot(housing[housing$Units < 1000, ], aes(x=SqFt)) + geom_histogram()

ggplot(housing[housing$Units < 1000, ], aes(x=Units)) + geom_histogram()

p11 <- ggplot(housing, aes(x=SqFt)) + geom_histogram()
p22 <- ggplot(housing, aes(x=Units)) + geom_histogram()
p33 <- ggplot(housing[housing$Units < 1000, ], aes(x=SqFt)) + geom_histogram()
p44 <- ggplot(housing[housing$Units < 1000, ], aes(x=Units)) + geom_histogram()
gridExtra::grid.arrange(p11, p22, p33, p44, nrow=2)

p11 <- ggplot(housing, aes(x=SqFt)) + geom_histogram()
p22 <- ggplot(housing, aes(x=Units)) + geom_histogram()
p33 <- ggplot(housing[housing$Units < 1000, ], aes(x=SqFt)) + geom_histogram() 
p44_ <- ggplot(housing, aes(x=Units)) + geom_histogram(binwidth = 25) + coord_cartesian(xlim=c(0, 1000))
gridExtra::grid.arrange(p11, p22, p33, p44_, nrow=2)

ggplot(housing, aes(x=SqFt, y=ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x=Units, y=ValuePerSqFt)) + geom_point()

ggplot(housing[housing$Units < 1000, ], aes(x=SqFt, y=ValuePerSqFt)) + 
  geom_point()
ggplot(housing %>% filter(Units<1000), aes(x=SqFt, y=ValuePerSqFt)) + 
  geom_point()

# 생략

ggplot(housing, aes(x=SqFt, y=ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x=log(SqFt), y=ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x=SqFt, y=ValuePerSqFt)) + geom_point() + coord_trans(x="log") +
  scale_x_continuous(breaks=c(1,10,100,1000,10000, 10e5, 10e6, 10e7))

house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data = housing)
summary(house1)
str(house1)
class(house1)
plot(house1)

house1$coefficient
coefficients(house1)
coef(house1)

house2 <- lm(ValuePerSqFt ~ Units * SqFt + Boro, data = housing)
house3 <- lm(ValuePerSqFt ~ Units : SqFt + Boro, data = housing)

coefplot::multiplot(house1, house2, house3)

housingNew <- read.table('http://www.jaredlander.com/data/housingNew.csv',
              sep=',', header=T, stringsAsFactors = F)
housePredict <- predict(house1, newdata=housingNew, se.fit=T,
                interval = 'prediction', level=.95)

summary(housePredict)
housePredict$fit
housePredict$se.fit

