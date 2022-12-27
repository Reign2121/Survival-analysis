library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
library(survival)
library(survminer)
setwd("/Users/reign/Downloads") 
surv = read.csv("survivalDataExercise (1).csv")
surv %>%
  summary()
surv = surv %>% mutate(event = ifelse(boughtAgain==1, 0, 1),
                       gender = as.factor(gender),
                       voucher = as.factor(voucher),
                       returned = as.factor(returned),
                       boughtAgain = as.factor(boughtAgain))

#visuallization (EDA)
surv %>%
  group_by(gender) %>%
  tally(event) %>%
  ggplot(aes(x=gender, y=n, fill = gender)) +
  geom_bar(width = 0.2, stat = 'identity', position = 'stack') +
  ylab("COUNT") +
  xlab("GENDER") +
  ggtitle("GENDER")

surv %>%
  ggplot(aes(x=gender, y=daysSinceFirstPurch, fill = gender)) +
  geom_bar(width = 0.2, stat = 'identity', position = 'stack') +
  ylab("Days since first purchase") +
  xlab("GENDER") +
  ggtitle("DAYS BY GENDER")

surv %>%
  group_by(voucher) %>%
  tally(event) %>%
  ggplot(aes(x=voucher, y=n, fill = voucher)) +
  geom_bar(width = 0.2, stat = 'identity', position = 'stack') +
  ylab("COUNT") +
  xlab("VOUCHER") +
  ggtitle("VOUCHER")

surv %>%
  group_by(returned) %>%
  tally(event) %>%
  ggplot(aes(x=returned, y=n, fill = returned)) +
  geom_bar(width = 0.2, stat = 'identity', position = 'stack') +
  ylab("COUNT") +
  xlab("RETURNED") +
  ggtitle("RETURNED")

###linear regression###
lm1 = lm(surv$daysSinceFirstPurch~. ,data = surv)
summary(lm1)
lm2 = lm(surv$shoppingCartValue~. ,data = surv)
summary(lm2)
glm3 = glm(surv$voucher~. ,data = surv, family = "binomial")
summary(glm3)

#km######## 카플란 마이어에서 비례위험 가정을 확인한다.
survObj = Surv(surv$daysSinceFirstPurch, surv$event)
survObj #이게 콕스모델에서의 종속변수.
#curve
km.model <- survfit(survObj ~ 1, data = surv, type="kaplan-meier")
summary(km.model)
plot(km.model,
     conf.int = TRUE, 
     xlab = "days since first purchase", 
     ylab = "Survival probability", 
     main = "Kaplan-Meier Survival Model")

ggsurvplot(km.model,
           risk.table = "nrisk_cumcensor", # Add risk table
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#2E9FDF"),
           title ='Kaplan-Meier Survival Model', 
           legend = 'none')

surv_plots <- list()
surv_plots[[1]] = ggsurvplot(survfit(Surv(daysSinceFirstPurch, event) ~ gender, data = surv), conf.int = TRUE,
                             title = 'Kaplan-Meier for Gender', xlab = 'Time')
ggsurvplot(survfit(Surv(daysSinceFirstPurch, event) ~ gender, data = surv), conf.int = TRUE,
           title = 'Kaplan-Meier for Gender', xlab = 'Time')
surv_plots[[2]] = ggsurvplot(survfit(Surv(daysSinceFirstPurch,event) ~ voucher, data = surv), conf.int = TRUE,
                             title = 'Kaplan-Meier  for voucher', xlab = 'Time')
surv_plots[[3]] = ggsurvplot(survfit(Surv(daysSinceFirstPurch,event) ~ returned, data = surv), conf.int = TRUE,
                             title = 'Kaplan-Meier  for returned', xlab = 'Time')
#surv_plots[[4]] = ggsurvplot(survfit(Surv(tenure, event) ~ Dependents, data = df), conf.int = TRUE,
#title = 'Kaplan-Meier  for Dependents', xlab = 'Time')

arrange_ggsurvplots(surv_plots, nrow = 2, ncol = 2)
#비례위험 가정에 문제가 있는 것 같다..!!! 주의

#logrank

survdiff(Surv(daysSinceFirstPurch, event) ~ gender, data = surv)
survdiff(Surv(daysSinceFirstPurch, event) ~ voucher, data = surv)
survdiff(Surv(daysSinceFirstPurch, event) ~ returned, data = surv)

#두 그래프는 세 변수에서 모두 유의미한 차이를 보인다는 것이 검증됐다.

#해석 ~

#####cox hazard##########

cx.model = coxph(Surv(daysSinceFirstPurch, event) ~ gender+voucher+returned,data=surv)
summary(cx.model)

# Using ggforest 
ggforest(cx.model)

require("rms")
cph.model <- cph(Surv(daysSinceFirstPurch, event) ~ gender+voucher+returned,data=surv,
                 x = TRUE, y = TRUE, surv = TRUE)
print(cph.model)
summary(cph.model)

exp(cph.model$coefficients)

#plot(summary(cph.model), log = TRUE)

ggplot(Predict(cph.model, gender, voucher, returned))

#비례위험가정 검토 (위험비는 시간에 따라 일정하다. 위험의 비는 고정, 시간에 비례해서 움직일 뿐)
sub_plot2 = list()
sub_plot2[[1]] = plot(cox.zph(cx.model), var = 1)
sub_plot2[[2]] = plot(cox.zph(cx.model), var = 2)
sub_plot2[[3]] = plot(cox.zph(cx.model), var = 3)

cox.zph(cx.model) #시간과 잔차 간의 독립성 검정

library(KMsurv)
library(survival)


#km.model2 <- survfit(survObj ~ gender, data = surv, type="kaplan-meier")
#summary(km.model2)

plot(km.model2, mark.time=F, fun='cloglog',lty=2:3,col=c('red','blue'),xlab='time (days)',ylab="loglogSurvival")
legend("bottomright",lty=2:3,legend=c("female","male"),bty="n", text.font=2, lwd=2, col=c('red','blue'))
plot(km.model2,  mark.time=F, fun='cumhaz',lty=2:3,col=c('red','blue'),xlab='time (days)',ylab="Cumulative Hazard")
legend("bottomright",lty=2:3,legend=c("female","male"),bty="n", text.font=2, lwd=2, col=c('red','blue'))




#비례위험가정 균열 발견
#교정이 필요함
cx.model2 = coxph(Surv(daysSinceFirstPurch, event) ~ voucher+returned+gender*daysSinceFirstPurch,data=surv)
#위 분석결과는 교호작용을 통제했을 때의 결과임

cx.model3 = coxph(Surv(daysSinceFirstPurch, event) ~ strata(gender)+voucher+returned,data=surv)

summary(cx.model2)
#시간과 남자성별의 유의미한 교호작용 발견
#남자의 시간과의 교호작용은 여자에 비해 위험율이 1%감소한다. 여자가 시간에 더 많은 영향을 받는다.

#층화작업
#범주간의 위험비를 보는 것이 목적이니까, 연속형 변수는 배제하고,, 층화작업이 더 적절한 피드백이겠다.
co1 =  coxph(Surv(daysSinceFirstPurch, event) ~ voucher+returned, data=surv[surv$gender== "female",])
co2 =  coxph(Surv(daysSinceFirstPurch, event) ~ voucher+returned, data=surv[surv$gender== "male",])
summary(co1)
summary(co2)
coef(co1)
coef(co2)
coef(cx.model2)
coef(cx.model3)
summary(lm(daysSinceFirstPurch ~ gender, data=surv))