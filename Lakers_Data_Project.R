# NBA Playmaking Project
# Optimizing the way in which prospect evaluation for the NBA Draft is conducted.

NBA_Assists <- read.csv("/Users/christianhanish/Desktop/Los Angeles Lakers/NBA Playmaking/NBA Playmaking Dataset.csv")
View(NBA_Assists)

summary(NBA_Assists)

NBA_Assists_No_Outliers <- read.csv("/Users/christianhanish/Desktop/Los Angeles Lakers/NBA Playmaking/NBA Playmaking Dataset (No Outliers).csv")
View(NBA_Assists_No_Outliers)

# Bivariate Regression - Assist-to-Turnover Ratio
BasicRegression <- lm(nbaAssistToTurnoverRatio ~ ncaaAssistToTurnoverRatio, NBA_Assists)
summary(BasicRegression)

sd(NBA_Assists$nbaAssistToTurnoverRatio)
mean(NBA_Assists$nbaAssistToTurnoverRatio)

sd(NBA_Assists$ncaaAssistToTurnoverRatio)
mean(NBA_Assists$ncaaAssistToTurnoverRatio)


BasicRegression <- lm(nbaAssistToTurnoverRatio ~ ncaaAssistToTurnoverRatio, NBA_Assists_No_Outliers)
summary(BasicRegression)

# Bivariate Regression - Assist Percentage-to-Usage Percentage Ratio
BasicRegression0 <- lm(nbaAssistToUsageRatio ~ ncaaAssistToUsageRatio, NBA_Assists)
summary(BasicRegression0)

# Bivariate Regression - Assist Percentage
BasicRegression1 <- lm(nbaAssistPercentage ~ ncaaAssistPercentage, NBA_Assists)
summary(BasicRegression1)

# Bivariate Regression - Turnover Percentage
BasicRegression2 <- lm(nbaTurnoverPercentage ~ ncaaTurnoverPercentage, NBA_Assists)
summary(BasicRegression2)

# Bivariate Regression - Assists Per Game
BasicRegression3 <- lm(nbaAPG ~ ncaaAPG, NBA_Assists)
summary(BasicRegression3)

# Predicted APG
BasicRegression3 <- lm(nbaAPG ~ ncaaAPG + ncaaMPG + height + ncaaGP + ncaaSOS + ncaaExperience, NBA_Assists)
summary(BasicRegression3)

# Predicted TPG
BasicRegression3 <- lm(nbaTPG ~ ncaaTPG + ncaaMPG + height + ncaaGP + ncaaSOS + ncaaExperience, NBA_Assists)
summary(BasicRegression3)

# Predicted AST/TOV
BasicRegression3 <- lm(nbaAssistToTurnoverRatio ~ ncaaAPG + ncaaTPG + ncaaMPG + height + ncaaGP + ncaaSOS + ncaaExperience, NBA_Assists)
summary(BasicRegression3)

# Predicted AST%
BasicRegression3 <- lm(nbaAssistPercentage ~ ncaaAssistPercentage + ncaaUsagePercentage + height + ncaaGP + ncaaSOS + ncaaExperience, NBA_Assists)
summary(BasicRegression3)

# Predicted TOV%
BasicRegression3 <- lm(nbaTurnoverPercentage ~ ncaaTurnoverPercentage + ncaaUsagePercentage + height + ncaaGP + ncaaSOS + ncaaExperience, NBA_Assists)
summary(BasicRegression3)

# Predicted AST/TOV
BasicRegression3 <- lm(nbaAssistToTurnoverRatio ~ ncaaAssistToTurnoverRatio + ncaaUsagePercentage + height + ncaaGP + ncaaSOS + ncaaExperience, NBA_Assists)
summary(BasicRegression3)

# Predicted AST/USG
BasicRegression3 <- lm(nbaAssistToUsageRatio ~ ncaaAssistToUsageRatio + ncaaTurnoverPercentage + height + ncaaGP + ncaaSOS + ncaaExperience, NBA_Assists)
summary(BasicRegression3)

# Bivariate Regression - Turnovers Per Game
BasicRegression4 <- lm(nbaTPG ~ ncaaTPG, NBA_Assists)
summary(BasicRegression4)

sd(NBA_Assists$nbaTPG)
mean(NBA_Assists$nbaTPG)
sd(NBA_Assists$ncaaTPG)
mean(NBA_Assists$ncaaTPG)

# Multiple Linear Regression - Per Game Statistcs
PerGameRegression <- lm(nbaAssistToTurnoverRatio ~ ncaaAPG + ncaaTPG + ncaaMPG + height + ncaaGP + ncaaSOS, NBA_Assists)
summary(PerGameRegression)

### Multiple Linear Regression - Per Game Statistcs
PerGameRegression <- lm(nbaAssistToTurnoverRatio ~ ncaaAPG + ncaaTPG + ncaaAssistPercentage + ncaaTurnoverPercentage + ncaaMPG + height + ncaaGP + ncaaSOS, NBA_Assists)
summary(PerGameRegression)

# Multiple Linear Regression - Percentage Statistcs
PercentageRegression <- lm(nbaAssistToTurnoverRatio ~ ncaaAssistPercentage + ncaaUsagePercentage + + height + ncaaGP + ncaaSOS, NBA_Assists)
summary(PercentageRegression)

# Multiple Linear Regression - Percentage Statistcs
Percentage1Regression <- lm(nbaAssistToTurnoverRatio ~ ncaaAssistPercentage + ncaaTurnoverPercentage + ncaaMPG + height + ncaaGP + ncaaSOS, NBA_Assists)
summary(Percentage1Regression)

# Multiple Linear Regression - Total Statistcs
TotalRegression <- lm(nbaAssistToTurnoverRatio ~ ncaaAST + ncaaTOV + ncaaMPG + height + ncaaGP + ncaaSOS, NBA_Assists)
summary(TotalRegression)

# Multiple Linear Regression - Per 40 Statistcs
Per40Regression <- lm(nbaAssistToTurnoverRatio ~ ncaaAssistsPer40 + ncaaTurnoversPer40 + height + ncaaGP + ncaaSOS, NBA_Assists)
summary(Per40Regression)

# Multiple Linear Regression - Solving for NBA AST/TOV
Per40Regression <- lm(nbaAssistToTurnoverRatio ~ ncaaAssistsPer40 + ncaaTurnoversPer40 + ncaaAssistPercentage + ncaaTurnoverPercentage + height + ncaaGP + ncaaSOS, NBA_Assists)
summary(Per40Regression)

# Multiple Linear Regression - Solving for NBA AST/TOV
Per40Regression <- lm(nbaAssistToTurnoverRatio ~ ncaaAssistsPer40 + ncaaTurnoversPer40 + ncaaAssistPercentage + ncaaTurnoverPercentage + height + ncaaGP + ncaaSOS, NBA_Assists)
summary(Per40Regression)

### Multiple Linear Regression - Per Game Statistcs
PerGameRegression <- lm(nbaAssistToTurnoverRatio ~ ncaaAPG + ncaaTPG + ncaaAssistPercentage + ncaaTurnoverPercentage + ncaaUsagePercentage + height + ncaaGP + ncaaSOS, NBA_Assists)
summary(PerGameRegression)

### Multiple Linear Regression - Per Game Statistcs
PerGameRegression <- lm(nbaAssistToTurnoverRatio ~ ncaaAssistToTurnoverRatio + NBA_Assists$ncaaAssistToUsageRatio + height + ncaaGP + ncaaSOS, NBA_Assists)
summary(PerGameRegression)


library(datasets)
library(ggplot2)

data(NBA_Assists)
NBA_Assists

ggplot(NBA_Assists, aes(x = ncaaAssistToUsageRatio, y = nbaAssistToUsageRatio)) + 
  geom_point() +
  labs(x = "NCAA AST/TOV Ratio", y = "NBA AST/TOV Ratio", title = "Correlation Between NBA & NCAA AST/TOV Ratio") +
  geom_smooth(method = "lm", se = FALSE)

str(NBA_Assists)

### Multiple Linear Regression - Per Game Statistcs
PerGameRegression <- lm(nbaAssistToTurnoverRatio ~ ncaaAPG + ncaaTPG + ncaaAssistPercentage + ncaaTurnoverPercentage + ncaaMPG + height + ncaaGP + ncaaSOS, NBA_Assists)
summary(PerGameRegression)

### Multiple Linear Regression - Per Game Statistcs
PerGameRegression <- lm(nbaAssistToTurnoverRatio ~ ncaaAssistToTurnoverRatio + ncaaUsagePercentage + height + ncaaGP + ncaaSOS, NBA_Assists)
summary(PerGameRegression)

### Multiple Linear Regression - Per Game Statistcs
PerGameRegression <- lm(nbaAssistToTurnoverRatio ~ ncaaAssistPercentage + ncaaTurnoverPercentage + ncaaUsagePercentage + height + ncaaExperience + ncaaSOS, NBA_Assists)
summary(PerGameRegression)

### Multiple Linear Regression - Per Game Statistcs
PerGameRegression <- lm(nbaAssistToTurnoverRatio ~ NBA_Assists$nbaAST + NBA_Assists$nbaTOV + NBA_Assists$nbaAssistPercentage + NBA_Assists$nbaTurnoverPercentage + NBA_Assists$ncaaSOS + NBA_Assists$ncaaExperience + NBA_Assists$height, NBA_Assists)
summary(PerGameRegression)

#trainingSet <- NBA_Assists(1:36, 

NBA_KNN_Dataset <- read.csv("/Users/christianhanish/Desktop/Los Angeles Lakers/NBA Playmaking/NBA KNN Dataset.csv")
View(NBA_KNN_Dataset)

str(NBA_KNN_Dataset)

table(NBA_KNN_Dataset$Type)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

NBA_KNN_Dataset_n <- as.data.frame(lapply(NBA_KNN_Dataset[3:9], normalize))
summary(NBA_KNN_Dataset_n$nbaAssistToUsageRatio)

trainingSet <- NBA_KNN_Dataset_n[1:36,]
testingSet <- NBA_KNN_Dataset_n[37:47,]

trainingOutcomes <- NBA_KNN_Dataset[1:36, 2]
testingOutcomes <- NBA_KNN_Dataset[37:47, 2]

install.packages("class")
library(class)
predictions <- knn(train = trainingSet, test = testingSet, cl = trainingOutcomes, k = 7)

library(gmodels)
CrossTable(x = testingOutcomes, y = predictions, prop.chisq = FALSE)

predictions

table(testingOutcomes, predictions)


NBA_SVM_Dataset <- read.csv("/Users/christianhanish/Desktop/Los Angeles Lakers/NBA Playmaking/NBA KNN Dataset.csv")
View(NBA_SVM_Dataset)


set.seed(10111)
x = matrix(rnorm(40), 20, 2)
y = rep(c(-1, 1), c(10, 10))
x[y == 1,] = x[y == 1,] + 1
plot(x, col = y + 3, pch = 19)

library(e1071)

dat = data.frame(x, y = as.factor(y))
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)

plot(svmfit, data)


library(e1071)
plot(iris)
