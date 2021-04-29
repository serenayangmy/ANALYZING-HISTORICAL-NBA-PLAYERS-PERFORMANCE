m15 <- lm(war_total ~ raptor_total, data = RAPTOR_CLEAN_PO)
summary(m15)

m16 <- lm(war_total ~ pace_impact, data = RAPTOR_CLEAN_PO)
summary(m16)

m17 <- lm(war_total ~ raptor_total + pace_impact, data = RAPTOR_CLEAN_PO)
summary(m17)

m21 <- lm(war_total ~ mp + poss + raptor_total, data = RAPTOR_CLEAN_PO)
summary(m21)

m22 <- lm(war_total ~ mp + poss + pace_impact, data = RAPTOR_CLEAN_PO)
summary(m22)

m23 <- lm(war_total ~ mp + poss + raptor_total + pace_impact, data = RAPTOR_CLEAN_PO)
summary(m23)

#--------N-Folder----------
set.seed(123)
runif(1)

d <- RAPTOR_CLEAN_PO[,-c(1,2,3,4,5,8,9,12,13,14,15,16,18,19,20,21,22,23)]

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
d <- RAPTOR_CLEAN_PO[,-c(1,2,3,4,5,8,9,12,13,14,15,16,18,19,20,21,22,23)]
model_full <- lm(war_total ~ ., data = d)

step <- stepAIC(model_full, direction="backward")
step$anova #display results

d <- RAPTOR_CLEAN_PO[,-c(1,2,3,4,5,8,9,12,13,14,15,16,18,19,20,21,22,23)]
model_full <- lm(war_total ~ ., data =d)
model_empty <- lm(war_total ~ 1, data =d)
summary(model_empty)

step <- stepAIC(model_empty, direction = "forward", scope = list(upper=model_full, lower=model_empty))
summary(step)

#--------------------multicollinearity
cor(d)
plot(d)

#----second-order-------------------------------
m30 <- lm(war_total ~ mp + poss + I(raptor_total^2) + pace_impact, data = RAPTOR_CLEAN_PO)
summary(m30)

m31 <- lm(war_total ~ mp + poss + raptor_total + I(pace_impact^2), data = RAPTOR_CLEAN_PO)
summary(m31)

m32 <- lm(war_total ~ mp + poss + raptor_total + pace_impact + raptor_total + pace_impact + raptor_total*pace_impact + I(raptor_total^2) + I(pace_impact^2), data = RAPTOR_CLEAN_PO)
summary(m32)

#------add offense & defense--------

m33 <- lm(war_total ~ mp + poss + I(raptor_offense^2) + I(raptor_defense^2) + pace_impact, data = RAPTOR_CLEAN_PO)
summary(m33)

m34 <- lm(war_total ~ mp + poss + raptor_offense + raptor_defense + I(pace_impact^2), data = RAPTOR_CLEAN_PO)
summary(m34)

m35 <- lm(war_total ~ mp + poss + raptor_offense + raptor_defense + pace_impact + raptor_offense*raptor_defense*pace_impact + raptor_offense*pace_impact + raptor_defense*pace_impact  + I(raptor_offense^2) + I(raptor_defense^2) + I(pace_impact^2), data = RAPTOR_CLEAN_PO)
summary(m35)

#--------N-Folder----------
set.seed(123)
runif(1)

d1 <- RAPTOR_CLEAN_PO[,-c(1,2,3,4,5,10,12,13,14,15,16,18,19,20,21,22,23,24)]

partition <- sample(2, nrow(d1), replace = TRUE, prob = c(0.80,0.20))
train <- d1[partition == 1 ,]
test <- d1[partition == 2 ,]

model <- lm(war_total ~ ., data =train)

prediction <- predict(model, test)
actual = test$war_total

cor(prediction, actual)
plot(prediction, actual)

#-------------------------------
library(DAAG)

help(cv.lm)

out <- cv.lm(data = d1, form.lm = formula(war_total~ .), m=9)

s#----------Backward and forward ----
library(MASS)

RAPTOR_CLEAN_PO$raptor_offensedefense <- RAPTOR_CLEAN_PO$raptor_offense * RAPTOR_CLEAN_PO$raptor_defense
RAPTOR_CLEAN_PO$raptor_offensepace <- RAPTOR_CLEAN_PO$raptor_offense * RAPTOR_CLEAN_PO$pace_impact
RAPTOR_CLEAN_PO$raptor_defensepace <- RAPTOR_CLEAN_PO$raptor_defense * RAPTOR_CLEAN_PO$pace_impact
RAPTOR_CLEAN_PO$raptor_offensesqft <- RAPTOR_CLEAN_PO$raptor_offense * RAPTOR_CLEAN_PO$raptor_offense
RAPTOR_CLEAN_PO$raptor_defensesqft <- RAPTOR_CLEAN_PO$raptor_defense * RAPTOR_CLEAN_PO$raptor_defense
RAPTOR_CLEAN_PO$pace_impactsqft <- RAPTOR_CLEAN_PO$pace_impact * RAPTOR_CLEAN_PO$pace_impact
RAPTOR_CLEAN_PO$opd <- RAPTOR_CLEAN_PO$raptor_offense * RAPTOR_CLEAN_PO$raptor_defense * RAPTOR_CLEAN_PO$pace_impact

d1 <- RAPTOR_CLEAN_PO[,-c(1,2,3,4,5,10,12,13,14,15,16,18,19,20,21,22,23,24)]
model_full1 <- lm(war_total ~ ., data = d1)

step <- stepAIC(model_full1, direction="backward")
step$anova #display results

d1 <- RAPTOR_CLEAN_PO[,-c(1,2,3,4,5,10,12,13,14,15,16,18,19,20,21,22,23,24)]
model_full1 <- lm(war_total ~ ., data =d1)
model_empty1 <- lm(war_total ~ 1, data =d1)
summary(model_empty1)

step1 <- stepAIC(model_empty1, direction = "forward", scope = list(upper=model_full1, lower=model_empty1))
summary(step1)

#--------------------multicollinearity -----------------
cor(d1)
plot(d1)
