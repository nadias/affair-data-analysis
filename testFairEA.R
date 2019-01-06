library(Ecdat)
library(corrplot)
library(ggplot2)
library(car)
library(MASS)

df <-Ecdat::Fair
str(df)
#Prever o time, tempo de vida sem reincidencias (em dias).

df$religious <- as.factor(df$religious)
df$occupation <- as.factor(df$occupation)
df$rate <- as.factor(df$rate)

table(df$occupation)

levels(df$occupation)
levels(df$occupation) <- c("1", "2.5", "3", "4", "5", "6.5", "7")

df$occupation[df$occupation == 3] <- as.factor(2.5)
df$occupation[df$occupation == 7] <- as.factor(6.5)
df$occupation <- factor(df$occupation)
table(df$occupation)

table(df$sex)
table(df$child)
table(df$nbaffairs)

summary(df)

hist(df$age,main='The Histogram of Age',
     xlab='Age Values',prob=T)        # DEVE TER PROB=T?
lines(density(df$age,na.rm=T))
rug(df$age)

hist(df$ym,main='The Histogram of Years Married',
     xlab='Years Married',prob=T)        # DEVE TER PROB=T?
lines(density(df$ym,na.rm=T))
rug(df$ym)

hist(df$education,main='The Histogram of Education',
     xlab='Education (years)',prob=T)        # DEVE TER PROB=T?     COUNT?
lines(density(df$education,na.rm=T))
rug(df$education)

hist(df$nbaffairs,main='The Histogram of the Number of Affairs',
     xlab='Number of Affairs',prob=T)        # DEVE TER PROB=T?
lines(density(df$nbaffairs,na.rm=T))
rug(df$nbaffairs)

par(mfrow=c(1,4))
boxplot(df$age, main="Age")
boxplot(df$ym, main="Years Married")
boxplot(df$education, main="Education")
boxplot(df$nbaffairs, main="Number of Affairs")

par(mfrow=c(1,1))
boxplot(df$nbaffairs ~ df$sex, df, main="Number of affairs by sex")
boxplot(df$nbaffairs ~ df$child, df, main="Number of affairs by having children or not")  # ver p confundimento. -> Do modelo completo: childyes  coeficiente: -0.13303
boxplot(df$nbaffairs ~ df$religious, df, main="Number of affairs by level of religiosity")
boxplot(df$nbaffairs ~ df$occupation, df, main="Number of affairs by level of occupation")
boxplot(df$nbaffairs ~ df$rate, df, main="Number of affairs by level of self rating of marriage")
boxplot(df$nbaffairs ~ df$ym, df, main="Number of affairs by years married")


ggplot(df,
       aes(x=age, y=nbaffairs,
           colour=sex
       )
) + geom_point()

ggplot(df,
       aes(x=religious, y=nbaffairs,
           colour=sex
       )
) + geom_point()

table(df$nbaffairs)
ggplot(df,aes(x=nbaffairs)) + geom_bar() + facet_wrap(~ sex) + ggtitle("Distribuition of affairs for individuals of different sexs")
table(df$sex)
ggplot(df,aes(x=nbaffairs)) + geom_bar() + facet_wrap(~ child) + ggtitle("Distribuition of affairs for individuals with and without children")
table(df$child)
ggplot(df,aes(x=nbaffairs)) + geom_bar() + facet_wrap(~ religious) + ggtitle("Distribuition of affairs for individuals of different religious levels")
table(df$religious)
ggplot(df,aes(x=nbaffairs)) + geom_bar() + facet_wrap(~ occupation) + ggtitle("Distribuition of affairs for individuals of different occupation levels")
table(df$occupation)
ggplot(df,aes(x=nbaffairs)) + geom_bar() + facet_wrap(~ rate) + ggtitle("Distribuition of affairs for different ratings of marriage")
table(df$rate)

dfChild <- df[as.character(df$child) == "yes", ]
table(dfChild$nbaffairs)

corMatrix <- cor(df[, c("age", "ym", "education", "nbaffairs")])
corMatrix
corrplot(corMatrix, method="circle")

mod <- lm(nbaffairs ~ ., df)
summary(mod)

drop1(mod, test = "F")


mod1 <- lm(nbaffairs ~ . -sex, df)
summary(mod1)

mod2 <- lm(nbaffairs ~ . -sex -age, df)
summary(mod2)  # Piorou

mod3 <- lm(nbaffairs ~ . -sex -child, df)
summary(mod3)

mod4 <- lm(nbaffairs ~ . -sex -child -education, df)
summary(mod4)

res = residuals(mod4)
boxplot(res)

RSS = sum(res^2)


predict(mod4, df, interval='c') # IC para a media

predict(mod4, df, interval='p') # IC de predicao. e' mais largo que o da media


res = rstandard(mod4)
boxplot(res)

plot(fitted.values(mod4), rstandard(mod4))






plot(df$ym,df$nbaffairs)
reg<-lm(nbaffairs ~ ym, data = df)
abline(reg, col="blue")


colnames(df)
mod5 <- lm(nbaffairs ~ . -sex -child -education -occupation +ym:age, df)
summary(mod5)

# An interaction that improved the adjusted R2 by 4 was religious:rate
mod6 <- lm(nbaffairs ~ . -sex -child -education -occupation +religious:rate, df)
summary(mod6)

mod7 <- lm(nbaffairs ~ . -sex -child -education -occupation +religious:age, df)
summary(mod7)

mod8 <- lm(nbaffairs ~ . -sex -child -education -occupation +religious:age +religious:rate, df)
summary(mod8)

modStep <- step(mod8, direction = c("backward"))
summary(modStep)

mod9 <- lm(nbaffairs ~ . -sex -child -education -occupation +religious:age +religious:rate, df)
summary(mod9)
drop1(mod9, test = "F")

mod10 <- lm(nbaffairs ~ . -age -sex -child -education -occupation +religious:age +religious:rate, df)
summary(mod10)
drop1(mod10, test = "F")
AIC(mod5)
extractAIC(mod5, k=2)




res = residuals(mod10)
boxplot(res)
RSS = sum(res^2)

predict(mod10, df, interval='c') # IC para a media
predict(mod10, df, interval='p') # IC de predicao. e' mais largo que o da media


res = rstandard(mod10)
boxplot(res)

plot(fitted.values(mod10), rstandard(mod10))


plot(hatvalues(mod10))

plot(rstandard(mod10))
plot(fitted.values(mod10))

qqPlot(rstandard(mod10))

pred1 <- predict(mod10,df)
pred1
plot(pred1)













str(df)

modStep <- step(mod, direction = c("backward"))
summary(modStep)



##############

df2 <- df
df2$nbaffairs <- df$nbaffairs != 0
df2$nbaffairs <- as.numeric(df2$nbaffairs)
df2$nbaffairs <- as.factor(df2$nbaffairs)

mod <- glm(nbaffairs ~ . , family = binomial(), df2)
summary(mod)

drop1(mod, test = "Chisq")

mod1 <- glm(nbaffairs ~ . -sex , family = binomial(), df2)
summary(mod1)

anova(mod1, mod)

drop1(mod1, test = "Chisq")


mod2 <- glm(nbaffairs ~ . -sex -education, family = binomial(), df2)
summary(mod2)

anova(mod2, mod1)

drop1(mod2, test = "Chisq")


mod3 <- glm(nbaffairs ~ . -sex -education -occupation, family = binomial(), df2)
summary(mod3)

anova(mod3, mod2)

drop1(mod3, test = "Chisq")


mod4 <- glm(nbaffairs ~ . -sex -education -occupation -child, family = binomial(), df2)
summary(mod4)

anova(mod4, mod3)

drop1(mod4, test = "Chisq")


# faz sentido remover a age pois deve estar mt correlacionada c o ym
mod5 <- glm(nbaffairs ~ . -sex -education -occupation -child -age, family = binomial(), df2)
summary(mod5)   # AIC piora

anova(mod5, mod4)

drop1(mod5, test = "Chisq")


mod5 <- glm(nbaffairs~.-sex -education -occupation -child -age+religious*ym, family = binomial(), data=df2)
summary(mod5) 

anova(mod5, mod4)

drop1(mod5, test = "Chisq")


summary()


mod6 <- glm(nbaffairs~.+religious:ym, family = binomial(), data=df2)
summary(mod6)

anova(mod6, mod)  # Nao e significativa??

# analisar normalidade residuos.



chisq.test(table(df$sex, df$age))
