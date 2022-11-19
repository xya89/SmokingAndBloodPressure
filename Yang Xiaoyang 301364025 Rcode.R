library("ggplot2")
library("stringr")
rawdata <- read.csv("Yang Xiaoyang 301364025 Data.csv")


# histogram for the continuous response vairable - systolic blood pressure
hist(rawdata$BP, main = ("Histogram of Systolic blood pressure"),
     xlab = "Systolic blood pressure (mmHg)", ylab = "Frequency",
     breaks = 11, xaxt = 'n')
axis(side = 1, at=seq(110, 170, 5), labels = seq(110, 170, 5))


# distribution of categorical explanatory vairable
barplot(table(rawdata$Smoking), main = "Bar chart of the Frequency of Smokers",
        xlab = "Smoking", ylab="Frequency")


# histogram for quantitative explanatory variable - amount of exercise
hist(rawdata$Exercise, main = ("Histogram of amount of exercise (min)"), 
     xlab = "Minutes", ylab = "Frequency",
     breaks = 10, xaxt = 'n', yaxt = 'n')
# axis adjustment
axis(side = 1, at=seq(0,230,20), labels = seq(0,230,20), pos = 0)
axis(side = 2, at=seq(0,14,2), pos = 0)

# resonse ~ quantitative explanatory variable plot
# sys_bp - quantitative response variable
sys_bp <- as.character(rawdata$BP)
sys_bp <- str_remove_all(sys_bp, pattern = "/[0-9]*$")
sys_bp <- as.integer(sys_bp)

# amount of exercise - quantitative explanatory variable
ame <- rawdata$Exercise

q_relation <- data.frame(exercise = ame, bp = sys_bp)
ggplot(q_relation, aes(x=exercise, y=sys_bp)) +
  geom_point(size = 1) +
  xlab("exercise (min)") + ylab("systolic blood pressure") +
  ggtitle("Amount of exercise \n versus systolic blood") +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = 'red')+
  geom_smooth(method = 'loess', formula = y ~ x, se= FALSE, color = 'blue')

# fit a model and summarize
datal.lm <- lm(q_relation$bp ~ q_relation$exercise, data = q_relation)
summary(datal.lm)

# residual plots
q_relation$rstandard = rstandard(datal.lm)
q_relation$pred = predict(datal.lm)

ggplot(q_relation, aes(x=pred, y=rstandard)) +
  xlim(min(q_relation$pred), max(q_relation$pred)) + ylim(-4, 3)+
  xlab("Predicted Values") + ylab("Standardized Residuals") + 
  ggtitle("Residual Plot for exercise versus systolic blood pressure \n using Stadardized Residuals") +
  geom_point(size = 1) +
  geom_hline(yintercept = 0, color = 'red') +
  geom_hline(yintercept = -2, color = 'blue', linetype = 'dashed') +
  geom_hline(yintercept = 2, color = 'blue', linetype = 'dashed') +
  geom_smooth(method = 'loess', formula = y ~ x, se = FALSE, color = 'blue')

# Normal Q-Q plot
qq.plot = lm(q_relation$bp ~ q_relation$exercise, data = q_relation)
plot(qq.plot, which=2)



# try transformation

# transformation - log y
q_relation$bp.log <- log(q_relation$bp)
# fit the regression for the transformed output
logY.lm <- lm(q_relation$bp.log ~ q_relation$exercise)
# Standardized residual plots for models with each tranformed variable
# from new regression
q_relation$rstandard.log <- rstandard(logY.lm)
q_relation$pred.log <- predict(logY.lm)


# transformation - 1 / Y
q_relation$bp.inv <- 1/q_relation$bp
# fit the regression for the transformed output
invY.lm <- lm(q_relation$bp.inv ~ q_relation$exercise)
# Standardized residual plots for models with each transfromed variable
# from new regression
q_relation$rstandard.inv <- rstandard(invY.lm)
q_relation$pred.inv <- predict(invY.lm)


# transformation - sqrt Y
q_relation$bp.sqrt <- sqrt(q_relation$bp)
sqrtY.lm <- lm(q_relation$bp.sqrt ~ q_relation$exercise)
q_relation$rstandard.sqrt <- rstandard(sqrtY.lm)
q_relation$pred.sqrt <- predict(sqrtY.lm)

# plot them out
plot(q_relation$pred.log, q_relation$rstandard.log)
plot(q_relation$pred.inv, q_relation$rstandard.inv)
plot(q_relation$pred.sqrt, q_relation$rstandard.sqrt)

# the result turns out to be the same
# try transform both x and y

# transform x

# nah these following two yield -Inf or Inf, so abort
q_relation$exercise.log <- log(q_relation$exercise)
q_relation$exercise.inv <- 1/q_relation$exercise

# have 0 
q_relation$exercise.sqrt <- sqrt(q_relation$exercise)

sqrtXY.lm <- lm(q_relation$bp.sqrt ~ q_relation$exercise.sqrt)
q_relation$rstandardXY.sqrt <- rstandard(sqrtXY.lm)
q_relation$predXY.sqrt <- predict(sqrtXY.lm)

plot(q_relation$predXY.sqrt, q_relation$rstandardXY.sqrt)

# try sqrtX inv Y
sqrtXinvY.lm <- lm(q_relation$bp.inv ~ q_relation$exercise.sqrt)
q_relation$rstandard.sqrtXinvY <- rstandard(sqrtXinvY.lm)
q_relation$pred.sqrtXinvY <- predict(sqrtXinvY.lm)
plot(q_relation$pred.sqrtXinvY, q_relation$rstandard.sqrtXinvY)

# i dont think the transformation is needed

# So the *final* model will be datal.lm

# plot the final model, which is the original model
# get confidence interval
pr = predict(object = datal.lm, newdata = as.data.frame(q_relation$exercise), interval = 'confidence')
CI = cbind(as.data.frame(q_relation$exercise),pr )

# get prediction confidence interval
prd = predict(object = datal.lm, newdata = as.data.frame(q_relation$exercise), interval = 'predict', level = 0.95)
PI = cbind(as.data.frame(q_relation$exercise), prd)


ggplot(q_relation, aes(x=exercise, y=bp)) +
  geom_point(size = 1) +
  xlab("exercise (min)") + ylab("systolic blood pressure") +
  ggtitle("Amount of exercise \n versus systolic blood") +
  geom_smooth(method = lm, formula = y ~ x, se = TRUE, color = 'blue')+
  geom_line(aes(y=PI$lwr), color='red', linetype='dashed')+
  geom_line(aes(y=PI$upr), color='red', linetype='dashed')


