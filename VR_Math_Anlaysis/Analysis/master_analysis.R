# TO DO:
# presentation of ressults from different batteries  
# relationship between posture stability and manual dexterity 

#######################################
# MODEL COMPARISON
#######################################
library(car)
library(psych)
library(ggplot2)

data <- read.csv("/Users/newxjy/Desktop/master_concat_data.csv")
data$boxed <- data$boxed_conjunction_12 + data$boxed_conjunction_4 + data$boxed_feature_4 + data$boxed_feature_12
data$brt <- data$BRT_left + data$BRT_left_thumb + data$BRT_right + data$BRT_right_thumb
data$filter <- data$filter_R2B0 + data$filter_R2B2 + data$filter_R2B4 + data$filter_R4B0 + data$filter_R4B2 

scatterplotMatrix(~ boxed + brt + filter + value.Predict + value.Random + tug_value, data=data)
scatterplotMatrix(~ boxed + brt + filter + balance_vision + balance_novision + balance_oscillate, data=data)
scatterplotMatrix(~ value.Predict + value.Random + tug_value + balance_vision + balance_novision + balance_oscillate, data=data)

# command to make scatterplots
scatterplot(brt ~ value.Predict | sex, data=data) # significant for male 
scatterplot(brt ~ value.Random | sex, data=data) # significant for male 
scatterplot(brt ~ tug_value | sex, data=data) # not significant for either gender 

scatterplot(boxed ~ value.Predict | sex, data=data) # not significant
scatterplot(boxed ~ value.Random | sex, data=data) # not significant 
scatterplot(boxed ~ tug_value | sex, data=data) # more predictable for female 

scatterplot(filter ~ value.Predict | sex, data=data) # significant for female 
scatterplot(filter ~ value.Random | sex, data=data) # significant for male 
scatterplot(filter ~ tug_value | sex, data=data) # not significant for either gender 

# Generate the summary statement for a full model 
# mod.full <- lm(brt ~  value.Predict + value.Random + tug_value + age + sex, data=data)
# summary(mod.full)

#######################################
# DESCRIPTIVE STATISTICS
#######################################
library(psych)
alpha(data)
describe(data$brt)
hist(data$brt,
     main="Histogram for BRT Score", 
     xlab="BRT Score", 
     border="blue", 
     col="green")

describe(data$filter)
hist(data$filter,
     main="Histogram for Filter Score", 
     xlab="Filter Score", 
     border="blue", 
     col="green")

plot(data$sex)

describe(data$tug_value)
hist(data$tug_value,
     main="Histogram for TUG", 
     xlab="TUG Value", 
     border="blue", 
     col="green")

describe(data$value.Random)
hist(data$value.Random,
     main="Histogram for Random Aiming", 
     xlab="Random Aiming Value", 
     border="blue", 
     col="green")

#######################################
# BIVARIATE REGRESSION
#######################################
mod1 <- lm(boxed ~ value.Random, data = data)
summary(mod1)

mod2 <- lm(brt ~ value.Random, data = data)
summary(mod2)

mod3 <- lm(filter ~ value.Random, data = data) # better model than the other models 
summary(mod3)
confint(mod3, 'value.Random', level=0.95)

mod4 <- lm(boxed ~ balance_oscillate, data = data)
summary(mod4)

mod5 <- lm(brt ~ balance_oscillate, data = data)
summary(mod5)

mod6 <- lm(filter ~ balance_oscillate, data = data)
summary(mod6)

mod7 <- lm(boxed ~ tug_value, data = data)
summary(mod7)

mod8 <- lm(brt ~ tug_value, data = data) # better model than the other models
summary(mod8)
confint(mod8, 'tug_value', level=0.95)

mod9 <- lm(filter ~ tug_value, data = data)
summary(mod9)

mod10 <- lm(boxed ~ value.Predict, data = data)
summary(mod10)

mod11 <- lm(brt ~ value.Predict, data = data)
summary(mod11)

mod12 <- lm(brt ~ value.Predict, data = data)
summary(mod12)

pt(1.96-1.329, 17, lower.tail = F)
# need to calculate CI 95%

#######################################
# EXPLORATORY ANALYSIS
#######################################

# multiple regression without interaction
mod13 <- lm(brt ~ tug_value + sex, data = data) # better model than the other models
summary(mod13)
mod13$coefficients[1]
mod13$coefficients[1] + mod13$coefficients[3]
mod13$df.residual
confint(mod13, '(Intercept)', level=0.95)
confint(mod13, 'sexMale', level=0.95)
confint(mod13, 'tug_value', level=0.95)
equation1=function(x){coef(mod13)[2]*x+coef(mod13)[1]}
equation2=function(x){coef(mod13)[2]*x+coef(mod13)[1]+coef(mod13)[3]}
ggplot(data,aes(y=brt,x=tug_value,color=sex))+geom_point()+
  stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])+
  xlab("TUG Value") + 
  ylab("BRT Score") 
#ggPredict(mod13,se=TRUE,interactive=TRUE)

# cognitive ability and sex
mod14 <- lm(brt ~ sex, data = data)
summary(mod14)
mod14$coefficients[1]
mod14$coefficients[1] + mod13$coefficients[3]
confint(mod14, '(Intercept)', level=0.95)
confint(mod14, 'sexMale', level=0.95)
# plot(data$brt, pch = 20, col = data$sex)
# abline(h = coef(mod14)[1], lwd = 1, col = 'red')
# abline(h = coef(mod14)[1] + coef(mod2)[2], lwd = 1, col = 'black')
mean.brt <- tapply(data$brt, INDEX = data$sex, FUN = mean, na.rm = T)
barplot(tapply(data$brt, INDEX = data$sex, FUN = mean, na.rm = T), xlab = "Sex", ylab = "BRT Score", ylim = c(0, 15))
barplot(mean.brt)

boxplot(brt ~ sex,
        data=data,
        main="BRT Score by Sex",
        xlab="Sex",
        ylab="BRT Score",
        col="orange",
        border="brown"
)
stripchart(brt ~ sex, vertical = TRUE, data = data, 
           method = "jitter", add = TRUE, pch = 20, col = 'black')

#######################################
# MODERATE MULTIPLE REGRESSION
#######################################

# multiple regression with interaction 
mod17 <- lm(brt ~ tug_value + sex + tug_value*sex, data = data)
summary(mod17)
mod17$coefficients[1]
mod17$coefficients[1] + mod17$coefficients[3]
confint(mod17, '(Intercept)', level=0.95)
confint(mod17, 'sexMale', level=0.95)
confint(mod17, 'tug_value', level=0.95)
confint(mod17, 'tug_value:sexMale', level=0.95)

ggplot(mod17,aes(y=brt,x=tug_value,color=sex))+geom_point()+stat_smooth(method="lm") +
  xlab("TUG Value") + 
  ylab("BRT Score")

library(dplyr) 
data2 <- data %>% mutate(sex = relevel(sex, ref = "Male"))
mod18 <- lm(brt ~ tug_value + sex + tug_value*sex, data = data)
summary(mod18)
confint(mod18, 'tug_value:sexFemale', level=0.95)

mod19 <- lm(brt ~  tug_value + sex, data = data2)
summary(mod19)
confint(mod19, 'sexFemale', level=0.95)

#######################################
# OLD
#######################################
# no_pred <- lm(brt ~ value.Random + tug_value + age + sex, data=mod.full$model)
# no_ran <- lm(brt ~  value.Predict + tug_value + age + sex, data=mod.full$model)
# no_tug <- lm(brt ~  value.Predict + value.Random + age + sex, data=mod.full$model)
# anova(no_pred, mod.full)
# anova(no_ran, mod.full)
# anova(no_tug, mod.full)
# 
# # Conclusion:  ?
# summary(lm(brt ~ age, data = data)) # most significant with age 
# lm(filter ~ age, data = data)
# lm(boxed ~ age, data = data)
# 
# #######################################
# # CROSS VALIDATION 
# #######################################
# 
# nCV <- 10
# n = 20
# MSE_no_pred <- numeric(nCV)
# MSE_no_ran <- numeric(nCV)
# 
# folds <- cut(sample(n),breaks=nCV,labels=FALSE)
# 
# #Perform n.folds fold cross validation
# i <- 1
# for(i in 1:nCV){
#   
#   #Segement your data by fold using the which() function 
#   testIndexes <- which(folds==i,arr.ind=TRUE)
#   testData <- mod.full$model[testIndexes, ]
#   trainData <- mod.full$model[-testIndexes, ]
#   
#   # The two models
#   mod_no_pred <- lm(brt ~ value.Random + tug_value + age + sex, data=trainData)
#   mod_no_ran <- lm(brt ~  value.Predict + tug_value + age + sex, data=trainData)
#   
#   # Get predictions
#   pred_no_pred <- predict(mod_no_pred, newdata = testData)
#   pred_no_ran <- predict(mod_no_ran, newdata = testData)
#   
#   # Calculate MSE
#   MSE_no_pred[i] <- mean((testData$brt - pred_no_pred)^2)
#   MSE_no_ran[i] <- mean((testData$brt - pred_no_ran)^2)
# }
# # mean MSEs
# mean(MSE_no_pred) 
# mean(MSE_no_ran)
# 
# # get differences
# diffs <- MSE_no_pred - MSE_no_ran
# 
# # get 95% CIs
# meandiff <- mean(diffs)
# sddiff <- sd(diffs)
# c(meandiff-2*sddiff, meandiff+2*sddiff)
