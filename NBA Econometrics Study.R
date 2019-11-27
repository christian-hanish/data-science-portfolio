# NBA Econometrics Study
# Determining which team statistics are most indicative of championship-caliber success.

# NBA Data Set
NBA_Data <- read.csv("/Users/christianhanish/Desktop/Econometrics/NBA Study/Econometrics NBA Dataset - Sheet1.csv")
View(NBA_Data)

# _________________________________________________________________________________________________________________________

# Bivariate Regression
reg1 <- lm(championship ~ trueShooting, NBA_Data)
summary(reg1)

# _________________________________________________________________________________________________________________________

# Multivariate Regression (Basic)
reg2 <- lm(championship ~ trueShooting + assistToTurnover + reboundingRate + pace + oppTrueShooting + oppTurnoverRate, NBA_Data)
summary(reg2)

# _________________________________________________________________________________________________________________________

# Create The Natural Log of a Variable
log_trueShooting = log(NBA_Data$trueShooting)

# Multivariate Regression w/ Natural Log
reg2 <- lm(championship ~ log_trueShooting + assistToTurnover + reboundingRate + pace + oppTrueShooting + oppTurnoverRate, NBA_Data)
summary(reg2)

# F Test (Without assistToTurnover & pace)
reg3 <- lm(championship ~ log_trueShooting + reboundingRate + oppTrueShooting + oppTurnoverRate, NBA_Data)
summary(reg3)

# _________________________________________________________________________________________________________________________

# Linear Probability Model
reg4 <- lm(championship ~ trueShooting + assistToTurnover + reboundingRate + pace + oppTrueShooting + oppTurnoverRate, NBA_Data)
summary(reg4) 

min(reg4$fitted.values)
max(reg4$fitted.values)

# Probit Model Regression
Probit1 <- glm(championship ~ trueShooting + assistToTurnover + reboundingRate + pace + oppTrueShooting + oppTurnoverRate, family = binomial(link = "probit"), NBA_Data)
summary(Probit1)

min(Probit1$fitted.values)
max(Probit1$fitted.values)

P5 <- pnorm(Probit1$coef[1] + Probit1$coef[2]*NBA_Data$trueShooting + Probit1$coef[3]*NBA_Data$assistToTurnover + Probit1$coef[4]*NBA_Data$reboundingRate + Probit1$coef[5]*NBA_Data$pace + Probit1$coef[6]*NBA_Data$oppTrueShooting + Probit1$coef[7]*NBA_Data$oppTurnoverRate)
sd(NBA_Data$trueShooting, na.rm=TRUE)

TrueShootingPlus <- NBA_Data$trueShooting + 0.02020654

P6 <- pnorm(Probit1$coef[1] + Probit1$coef[2]*TrueShootingPlus + Probit1$coef[3]*NBA_Data$assistToTurnover + Probit1$coef[4]*NBA_Data$reboundingRate + Probit1$coef[5]*NBA_Data$pace + Probit1$coef[6]*NBA_Data$oppTrueShooting + Probit1$coef[7]*NBA_Data$oppTurnoverRate)
mean(P6-P5, na.rm = TRUE)

# Table
stargazer(reg1, reg2, Probit1, type="text")

stargazer(NBA_Data$trueShooting, NBA_Data$assistToTurnover,NBA_Data$reboundingRate, NBA_Data$pace, NBA_Data$oppTrueShooting, NBA_Data$oppTurnover,  type="text")
