mysample
# Visited, Rehomed, Health, Breed, Age, Reason, Returned

table(mysample$Breed) # Mixed Breed, Shih Tzu, Staffordshire Bull Terrier
table(mysample$Age) # Fully grown, Puppy
table(mysample$Reason) # Dangerous, Health condition, Neglect, Stray, Unwanted
table(mysample$Returned) # No, Unknown, Yes

# Missing values
colSums(is.na(mysample))
nrow(mysample)

# Breed: 6
mysample <- mysample[!(is.na(mysample$Breed)), ]
nrow(mysample)

# Rehomed null with 99999
mysample <- mysample[!mysample$Rehomed == 99999, ]
nrow(mysample)


# Analysing for each breed
table(mysample$Breed) # Mised 710, Shih Tzu 24, Staffordshire 207

# Mixed
df_MixedBreed <- mysample[mysample$Breed=="Mixed Breed", ]
summary(df_MixedBreed)
table(df_MixedBreed$Age)
table(df_MixedBreed$Reason)
table(df_MixedBreed$Returned)

#Shih Tzu
df_ShihTzu <- mysample[mysample$Breed=="Shih Tzu", ]
summary(df_ShihTzu)
table(df_ShihTzu$Age)
table(df_ShihTzu$Reason)
table(df_ShihTzu$Returned)

# Staffordshire
df_Staffordshire <- mysample[mysample$Breed=="Staffordshire Bull Terrier", ]
summary(df_Staffordshire)
table(df_Staffordshire$Age)
table(df_Staffordshire$Reason)
table(df_Staffordshire$Returned)


# Analysis for each variable
# Age
par(mfrow = c(2, 2))
boxplot(Rehomed ~ Age, data=df_MixedBreed)
boxplot(Rehomed ~ Age, data=df_ShihTzu)
boxplot(Rehomed ~ Age, data=df_Staffordshire)
par(mfrow = c(2, 2))
barplot(table(df_MixedBreed$Age))
barplot(table(df_ShihTzu$Age))
barplot(table(df_Staffordshire$Age))

# Reason
par(mfrow = c(2, 2))
boxplot(Rehomed ~ Reason, data=df_MixedBreed)
boxplot(Rehomed ~ Reason, data=df_ShihTzu)
boxplot(Rehomed ~ Reason, data=df_Staffordshire)
par(mfrow = c(2, 2))
barplot(table(df_MixedBreed$Reason))
barplot(table(df_ShihTzu$Reason))
barplot(table(df_Staffordshire$Reason))

# Returned
par(mfrow = c(2, 2))
boxplot(Rehomed ~ Returned, data=df_MixedBreed)
boxplot(Rehomed ~ Returned, data=df_ShihTzu)
boxplot(Rehomed ~ Returned, data=df_Staffordshire)
par(mfrow = c(2, 2))
barplot(table(df_MixedBreed$Returned))
barplot(table(df_ShihTzu$Returned))
barplot(table(df_Staffordshire$Returned))

# Visited
par(mfrow = c(2, 2))
plot(Rehomed ~ Visited, data=df_MixedBreed)
plot(Rehomed ~ Visited, data=df_ShihTzu)
plot(Rehomed ~ Visited, data=df_Staffordshire)
par(mfrow = c(1, 1))
boxplot(df_MixedBreed$Visited, df_ShihTzu$Visited, df_Staffordshire$Visited)
par(mfrow = c(2, 2))
hist(df_MixedBreed$Visited, freq = FALSE)
hist(df_ShihTzu$Visited, freq = FALSE)
hist(df_Staffordshire$Visited, freq = FALSE)

# Health
par(mfrow = c(2, 2))
plot(Rehomed ~ Health, data=df_MixedBreed)
plot(Rehomed ~ Health, data=df_ShihTzu)
plot(Rehomed ~ Health, data=df_Staffordshire)
par(mfrow = c(1, 1))
boxplot(df_MixedBreed$Health, df_ShihTzu$Health, df_Staffordshire$Health)
par(mfrow = c(2, 2))
hist(df_MixedBreed$Health, freq = FALSE)
hist(df_ShihTzu$Health, freq = FALSE)
hist(df_Staffordshire$Health, freq = FALSE)

# Rehomed
par(mfrow = c(1, 1))
boxplot(df_MixedBreed$Rehomed, df_ShihTzu$Rehomed, df_Staffordshire$Rehomed)
hist(df_MixedBreed$Rehomed, freq = FALSE, xlab="Rehomed", main="Mixed Breed")
hist(df_ShihTzu$Rehomed, freq = FALSE, xlab="Rehomed", main="Shih Tzu")
hist(df_Staffordshire$Rehomed, freq = FALSE, xlab="Rehomed", main="Staffordshire Bull Terrier")

cor(df_MixedBreed$Rehomed, df_MixedBreed$Visited)
cor(df_MixedBreed$Rehomed, df_MixedBreed$Health)
cor(df_ShihTzu$Rehomed, df_ShihTzu$Visited)
cor(df_ShihTzu$Rehomed, df_ShihTzu$Health)
cor(df_Staffordshire$Rehomed, df_Staffordshire$Visited)
cor(df_Staffordshire$Rehomed, df_Staffordshire$Health)

library(moments)
summary(df_MixedBreed$Rehomed)
mean(df_MixedBreed$Rehomed)
var(df_MixedBreed$Rehomed)
sd(df_MixedBreed$Rehomed)
quantile(df_MixedBreed$Rehomed, type=1)
skewness(df_MixedBreed$Rehomed)

summary(df_ShihTzu$Rehomed)
mean(df_ShihTzu$Rehomed)
var(df_ShihTzu$Rehomed)
sd(df_ShihTzu$Rehomed)
quantile(df_ShihTzu$Rehomed, type=1)
skewness(df_ShihTzu$Rehomed)

summary(df_Staffordshire$Rehomed)
mean(df_Staffordshire$Rehomed)
var(df_Staffordshire$Rehomed)
sd(df_Staffordshire$Rehomed)
quantile(df_Staffordshire$Rehomed, type=1)
skewness(df_Staffordshire$Rehomed)


# Analysis on Rehomed
# MixedBreed
# Normal distribution
par(mfrow = c(1, 1))
hist(df_MixedBreed$Rehomed, freq=FALSE)
x <- seq(from = min(df_MixedBreed$Rehomed), to = max(df_MixedBreed$Rehomed), by = 0.1) 
lines(x, dnorm(x, mean = 18.94, sd = 9.51), lwd = 2, col = "blue")

# QQ plot
mu <- mean(df_MixedBreed$Rehomed)
sigma <- sd(df_MixedBreed$Rehomed)
qqnorm(df_MixedBreed$Rehomed, pch=16, main="Mixed Breed")
abline(a = mu, b = sigma, col = "red")

# CDF
Fn <- ecdf(df_MixedBreed$Rehomed)
plot(Fn, verticals = TRUE, pch = NA, main="Mixed Breed")
Fn(30)
sum(df_MixedBreed$Rehomed<=30)/length(df_MixedBreed$Rehomed) #0.8746479

G <- function(x){
  return(pnorm(x, mean=mu, sd=sigma))
}
G(30)
x <- 1:50
lines(x, G(x), col = "red")

# Hypothesis testing for normality
# H0 : our sample comes from a normal distribution
# H1 : our sample does not come from a normal distribution

# Kolmogorov-Smirnov test, based on CDF
# H0 : F=G
# H1 : F!=G
ks.test(x = df_MixedBreed$Rehomed, y = "pnorm", mean = mu, sd = sigma)

# Shapiro-Wilk test, based on QQ plot
shapiro.test(df_MixedBreed$Rehomed)

# Chi-squared goodness of fit test, based on hist
hist(df_MixedBreed$Rehomed, freq = TRUE)
library(nortest)
pearson.test(df_MixedBreed$Rehomed)

# Estimating mu
mu <- mean(df_MixedBreed$Rehomed)
sigma <- sd(df_MixedBreed$Rehomed)
muhat1 <- rep(NA, 1000)
muhat2 <- rep(NA, 1000)

for (i in 1:1000){
  x <- rnorm(n=10, mean=mu, sd=sigma)
  muhat1[i] <- mean(x)
  muhat2[i] <- quantile(x, type=1)[3]
}

par(mfrow=c(1, 2))
hist(muhat1, xlim=range(c(muhat1, muhat2)))
abline(v=mu, col="red3", lwd=3)
abline(v=mean(muhat1), col="blue", lty=2, lwd=3)

hist(muhat2, xlim=range(c(muhat1, muhat2)))
abline(v=mu, col="red3", lwd=3)
abline(v=mean(muhat2), col="blue", lty=2, lwd=3)

# Estimating sigma^2
sigma2hat1 <- rep(NA, 1000)
sigma2hat2 <- rep(NA, 1000)

for (i in 1:1000){
  x <- rnorm(n=10, mean=mu, sd=sigma)
  sigma2hat1[i] <- sd(x)^2
  sigma2hat2[i] <- (9/10)*sd(x)^2
}

par(mfrow=c(1, 2))
hist(sigma2hat1, xlim=range(c(sigma2hat1, sigma2hat2)))
abline(v=sigma^2, col="red3", lwd=3)
abline(v=mean(sigma2hat1), col="blue", lty=2, lwd=3)

hist(sigma2hat2, xlim=range(c(sigma2hat1, sigma2hat2)))
abline(v=sigma^2, col="red3", lwd=3)
abline(v=mean(sigma2hat2), col="blue", lty=2, lwd=3)

# Confidence Interval
n <- nrow(df_MixedBreed)
sigma <- sd(df_MixedBreed$Rehomed)
xbar <- mean(df_MixedBreed$Rehomed)
c <- qnorm(0.975) # 5% level test
CI <- xbar + c(-1, 1)*c*sqrt(sigma^2/n)
CI

z <- (xbar-27)/sqrt(sigma^2/n)
2*pnorm(z) # p-value

# Shih Tzu
# Normal distribution
par(mfrow = c(1, 1))
hist(df_ShihTzu$Rehomed, freq=FALSE, xlab="Rehomed", main="Shih Tzu")
x <- seq(from = min(df_ShihTzu$Rehomed), to = max(df_ShihTzu$Rehomed), by = 0.1) 
lines(x, dnorm(x, mean = 19.5, sd = 8.83), lwd = 2, col = "blue")

# QQ plot
mu <- mean(df_ShihTzu$Rehomed)
sigma <- sd(df_ShihTzu$Rehomed)
qqnorm(df_ShihTzu$Rehomed, pch=16, main="Shih Tzu")
abline(a = mu, b = sigma, col = "red")

# CDF
Fn <- ecdf(df_ShihTzu$Rehomed)
plot(Fn, verticals = TRUE, pch = NA, main="Shih Tzu")
Fn(30)
sum(df_ShihTzu$Rehomed<=30)/length(df_ShihTzu$Rehomed) #0.5416667

G <- function(x){
  return(pnorm(x, mean=mu, sd=sigma))
}
G(30)
x <- 1:50
lines(x, G(x), col = "red")

# Hypothesis testing for normality
# H0 : our sample comes from a normal distribution
# H1 : our sample does not come from a normal distribution

# Kolmogorov-Smirnov test, based on CDF
# H0 : F=G
# H1 : F!=G
ks.test(x = df_ShihTzu$Rehomed, y = "pnorm", mean = mu, sd = sigma)
ks.test(x = df_ShihTzu$Rehomed, y = "pexp", rate=1) # for exponential

# Shapiro-Wilk test, based on QQ plot
shapiro.test(df_ShihTzu$Rehomed)

# Chi-squared goodness of fit test, based on hist
hist(df_ShihTzu$Rehomed, freq = TRUE)
library(nortest)
pearson.test(df_ShihTzu$Rehomed)

# Confidence Interval
n <- nrow(df_ShihTzu)
s <- sd(df_ShihTzu$Rehomed)
xbar <- mean(df_ShihTzu$Rehomed)
t <- qt(p=0.975, df=n-1)
CI <- xbar + c(-1, 1)*t*sqrt(s^2/n)
CI

t <- (xbar-27)/sqrt(s^2/n)
2*(1-pt(q=abs(t), df=n-1)) # p-value

# Staffordshire
# Normal distribution
par(mfrow = c(1, 1))
hist(df_Staffordshire$Rehomed, freq=FALSE, xlab="Rehomed", main="Staffordshire Bull Terrier")
x <- seq(from = min(df_Staffordshire$Rehomed), to = max(df_Staffordshire$Rehomed), by = 0.1) 
lines(x, dnorm(x, mean = 19.34, sd = 10.56), lwd = 2, col = "blue")

# QQ plot
mu <- mean(df_Staffordshire$Rehomed)
sigma <- sd(df_Staffordshire$Rehomed)
qqnorm(df_Staffordshire$Rehomed, pch=16, main="Staffordshire Bull Terrier")
abline(a = mu, b = sigma, col = "red")

# CDF
Fn <- ecdf(df_Staffordshire$Rehomed)
plot(Fn, verticals = TRUE, pch = NA, main="Staffordshire Bull Terrier")
Fn(30)
sum(df_Staffordshire$Rehomed<=30)/length(df_Staffordshire$Rehomed) #0.8357488

G <- function(x){
  return(pnorm(x, mean=mu, sd=sigma))
}
G(30)
x <- 1:50
lines(x, G(x), col = "red")

# Hypothesis testing for normality
# H0 : our sample comes from a normal distribution
# H1 : our sample does not come from a normal distribution

# Kolmogorov-Smirnov test, based on CDF
# H0 : F=G
# H1 : F!=G
ks.test(x = df_Staffordshire$Rehomed, y = "pnorm", mean = mu, sd = sigma)

# Shapiro-Wilk test, based on QQ plot
shapiro.test(df_Staffordshire$Rehomed)

# Chi-squared goodness of fit test, based on hist
hist(df_Staffordshire$Rehomed, freq = TRUE)
library(nortest)
pearson.test(df_Staffordshire$Rehomed)

# Estimating mu
mu <- mean(df_Staffordshire$Rehomed)
sigma <- sd(df_Staffordshire$Rehomed)
muhat1 <- rep(NA, 1000)
muhat2 <- rep(NA, 1000)

for (i in 1:1000){
  x <- rnorm(n=10, mean=mu, sd=sigma)
  muhat1[i] <- mean(x)
  muhat2[i] <- quantile(x, type=1)[3]
}

par(mfrow=c(1, 2))
hist(muhat1, xlim=range(c(muhat1, muhat2)))
abline(v=mu, col="red3", lwd=3)
abline(v=mean(muhat1), col="blue", lty=2, lwd=3)

hist(muhat2, xlim=range(c(muhat1, muhat2)))
abline(v=mu, col="red3", lwd=3)
abline(v=mean(muhat2), col="blue", lty=2, lwd=3)

# Estimating sigma^2
sigma2hat1 <- rep(NA, 1000)
sigma2hat2 <- rep(NA, 1000)

for (i in 1:1000){
  x <- rnorm(n=10, mean=mu, sd=sigma)
  sigma2hat1[i] <- sd(x)^2
  sigma2hat2[i] <- (9/10)*sd(x)^2
}

par(mfrow=c(1, 2))
hist(sigma2hat1, xlim=range(c(sigma2hat1, sigma2hat2)))
abline(v=sigma^2, col="red3", lwd=3)
abline(v=mean(sigma2hat1), col="blue", lty=2, lwd=3)

hist(sigma2hat2, xlim=range(c(sigma2hat1, sigma2hat2)))
abline(v=sigma^2, col="red3", lwd=3)
abline(v=mean(sigma2hat2), col="blue", lty=2, lwd=3)

# Confidence Interval
n <- nrow(df_Staffordshire)
sigma <- sd(df_Staffordshire$Rehomed)
xbar <- mean(df_Staffordshire$Rehomed)
c <- qnorm(0.975) # 5% level test
CI <- xbar + c(-1, 1)*c*sqrt(sigma^2/n)
CI

z <- (xbar-27)/sqrt(sigma^2/n)
2*pnorm(z) # p-value


# Plotting confidence intervals
analysis = c("Mixed Breed", "Shih Tzu", "Staffordshire Bull Terrier")

estimate = c(18.9380, 19.5000, 19.3382) 
upper = c(19.63783, 23.22725, 20.77724)
lower = c(18.23823, 15.77275, 17.89909)
pval = c(6.874979e-113, 0.0003754547, 1.713764e-25)

par(mfrow=c(1,1))
par(mar=c(6,6,1,6))
plot(x=0, xlim=c(10,30), ylim=c(0,5),
     type="n", xaxt="n", yaxt="n",
     xlab=NULL, ylab=NULL, ann=FALSE, bty="n")

axis(side=1, cex.axis=1)
mtext("95% confidence interval for each sample", 
      side = 1, line = 4) 

for (i in c(10, 15, 20, 25, 30)){
  lines(c(i, i), c(0, 5), lty=2, col="gray53")
}

verticalpos=1:3
mtext(text=analysis, at=verticalpos, side=2, line=5, 
      outer=FALSE, las=1, adj=0)

points(estimate, verticalpos, pch=16)

for(i in 1:3){
  lines(c(lower[i], upper[i]), c(verticalpos[i], verticalpos[i]))
  lines(c(lower[i], lower[i]), c(verticalpos[i]+0.2, verticalpos[i]-0.2))
  lines(c(upper[i], upper[i]), c(verticalpos[i]+0.2, verticalpos[i]-0.2))
}


# Paired samples
# Mixed - Shih Tzu
x1 <- df_MixedBreed$Rehomed
x2 <- df_ShihTzu$Rehomed
n1 <- nrow(df_MixedBreed)
n2 <- nrow(df_ShihTzu)
t.test(x = x1, y = x2, mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)

meandiff <- mean(x1)-mean(x2)
t <- qt(0.975, df=n1+n2-2)
sp <- sqrt(((n1-1)*sd(x1)^2+(n2-2)*sd(x2)^2)/(n1+n2-2))
meandiff + c(-1, 1)*t*sp*sqrt(1/n1+1/n2) # -4.427648  3.303705

# Shih Tzu-Staffordshire
x1 <- df_ShihTzu$Rehomed
x2 <- df_Staffordshire$Rehomed
n1 <- nrow(df_ShihTzu)
n2 <- nrow(df_Staffordshire)
t.test(x = x1, y = x2, mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)

meandiff <- mean(x1)-mean(x2)
t <- qt(0.975, df=n1+n2-2)
sp <- sqrt(((n1-1)*sd(x1)^2+(n2-2)*sd(x2)^2)/(n1+n2-2))
meandiff + c(-1, 1)*t*sp*sqrt(1/n1+1/n2) # -4.247978  4.571650

# Mixed-Staffordshire
x1 <- df_MixedBreed$Rehomed
x2 <- df_Staffordshire$Rehomed
n1 <- nrow(df_MixedBreed)
n2 <- nrow(df_Staffordshire)
t.test(x = x1, y = x2, mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)

meandiff <- mean(x1)-mean(x2)
t <- qt(0.975, df=n1+n2-2)
sp <- sqrt(((n1-1)*sd(x1)^2+(n2-2)*sd(x2)^2)/(n1+n2-2))
meandiff + c(-1, 1)*t*sp*sqrt(1/n1+1/n2) # -1.912189  1.111917

# Plotting confidence intervals for the paired samples
analysis = c("M-Sh", "Sh-St", "M-St")

estimate = c(-0.5619718, 0.1618357, -0.4001361) 
upper = c(3.303705, 4.571650, 1.111917)
lower = c(-4.427648, -4.247978, -1.912189)
pval = c(0.7755, 0.9425, 0.6039)

par(mfrow=c(1,1))
par(mar=c(6,6,1,6))
plot(x=0, xlim=c(-5,5), ylim=c(0,5),
     type="n", xaxt="n", yaxt="n",
     xlab=NULL, ylab=NULL, ann=FALSE, bty="n")

axis(side=1, cex.axis=1)
mtext("95% confidence intervals for paired samples", 
      side = 1, line = 4) 

for (i in c(-5, 0, 5)){
  lines(c(i, i), c(0, 5), lty=2, col="gray53")
}

verticalpos=1:3
mtext(text=analysis, at=verticalpos, side=2, line=5, 
      outer=FALSE, las=1, adj=0)

points(estimate, verticalpos, pch=16)

for(i in 1:3){
  lines(c(lower[i], upper[i]), c(verticalpos[i], verticalpos[i]))
  lines(c(lower[i], lower[i]), c(verticalpos[i]+0.2, verticalpos[i]-0.2))
  lines(c(upper[i], upper[i]), c(verticalpos[i]+0.2, verticalpos[i]-0.2))
}

