m12 <- lm(war_total ~ raptor_total, data = RAPTOR_CLEAN_RS)
summary(m12)

m13 <- lm(war_total ~ pace_impact, data = RAPTOR_CLEAN_RS)
summary(m13)

m14 <- lm(war_total ~ raptor_total + pace_impact, data = RAPTOR_CLEAN_RS)
summary(m14)

m18 <- lm(war_total ~ mp + poss + raptor_total, data = RAPTOR_CLEAN_RS)
summary(m18)

m19 <- lm(war_total ~ mp + poss + pace_impact, data = RAPTOR_CLEAN_RS)
summary(m19)

m20 <- lm(war_total ~ mp + poss + raptor_total + pace_impact, data = RAPTOR_CLEAN_RS)
summary(m20)

#--------N-Folder----------
set.seed(123)
runif(1)

d <- RAPTOR_CLEAN_RS[,-c(1,2,3,4,5,8,9,12,13,14,15,16,18,19,20,21,22,23)]

partition <- sample(2, nrow(d), replace = TRUE, prob = c(0.80,0.20))
train <- d[partition == 1 ,]
test <- d[partition == 2 ,]

model <- lm(war_total ~ ., data =train)

prediction <- predict(model, test)
actual = test$war_total

cor(prediction, actual)
plot(prediction, actual)

#-------------------------------
library(DAAG)

help(cv.lm)

out <- cv.lm(data = d, form.lm = formula(war_total~ .), m=9)

s#----------Backward and forward ----
library(MASS)
d <- RAPTOR_CLEAN_RS[,-c(1,2,3,4,5,8,9,12,13,14,15,16,18,19,20,21,22,23)]
model_full <- lm(war_total ~ ., data = d)

step <- stepAIC(model_full, direction="backward")
step$anova #display results

d <- RAPTOR_CLEAN_RS[,-c(1,2,3,4,5,8,9,12,13,14,15,16,18,19,20,21,22,23)]
model_full <- lm(war_total ~ ., data =d)
model_empty <- lm(war_total ~ 1, data =d)
summary(model_empty)

step <- stepAIC(model_empty, direction = "forward", scope = list(upper=model_full, lower=model_empty))
summary(step)

#--------------------multicollinearity
cor(d)
plot(d)
