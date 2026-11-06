# Linear Models with R
# Julian F. Faraway
# 01 - Introduction

# Packages ----

library(tidyverse)

# Initial data analysis ----

data(pima, package = "faraway")
head(pima)

summary(pima)

sort(pima$diastolic)

pima$diastolic[pima$diastolic == 0] <- NA
pima$glucose[pima$glucose == 0] <- NA
pima$triceps[pima$triceps == 0] <- NA
pima$insulin[pima$insulin == 0] <- NA
pima$bmi[pima$bmi == 0] <- NA

pima$test <- factor(pima$test)
summary(pima$test)

levels(pima$test) <- c("negative", "positive")

summary(pima)

hist(pima$diastolic,xlab="Diastolic",main="")

plot(density(pima$diastolic,na.rm=TRUE),main="")

plot(sort(pima$diastolic),ylab="Sorted Diastolic")

plot(diabetes ~ diastolic,pima)
plot(diabetes ~ test,pima)

ggplot(pima,aes(x=diastolic))+
  geom_histogram()

ggplot(pima,aes(x=diastolic))+
  geom_density()

ggplot(pima,aes(x=diastolic,y=diabetes))+
  geom_point()

ggplot(pima,aes(x=diastolic,y=diabetes,shape=test))+
  geom_point()+
  theme(legend.position ="top", legend.direction="horizontal")

ggplot(pima,aes(x=diastolic,y=diabetes))+
  geom_point(size=1)+
  facet_grid(~ test)

# History ----

data(manilius, package="faraway")
head(manilius)

(moon3 <- aggregate(manilius[,1:3],list(manilius$group), sum))

manilius |> 
  summarise(
    arc = sum(arc),
    sinang = sum(sinang),
    cosang = sum(cosang),
    .by = group
  )

# Mayer's solution:
# arc = beta + alpha * sinang + gamma * cosang
solve(cbind(9,moon3$sinang,moon3$cosang), moon3$arc)

# alpha = -1.49
# beta  = 14.5 
# gamma = 0.13

# Adrien Marie Legendre introduces least squares in 1805
# arc = beta + alpha * sinang + gamma * cosang + epsilon

lmod <- lm(arc ~ sinang + cosang, manilius)
coef(lmod)
