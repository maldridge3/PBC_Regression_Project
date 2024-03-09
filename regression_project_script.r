#load packages and import data

library(MASS)
library(car)

df <- read.csv(file.choose())

#Check for missing values

sum(is.na(df))

summary(df)

#remove instance where response = 0

df_filt <- df[!df$BetaPlasma == 0,]

#create box plots for continuous variables

par(mfrow = c(4,2), mar = c(0.5,5,1,1))

boxplot(df_filt$BetaPlasma, ylab = "PBC (ng/ml)", cex = 1.1)
mtext("A",adj = 0.025, padj = 1.5,)

boxplot(df_filt$Age, ylab = "Age")
mtext("B",adj = 0.025, padj = 1.5)

boxplot(df_filt$Quetelet, ylab = "BMI", cex = 1.1)
mtext("C",adj = 0.025, padj = 1.5)

boxplot(df_filt$Calories, ylab = "Daily Calories", cex = 1.1)
mtext("D",adj = 0.025, padj = 1.5)

boxplot(df_filt$Fat, ylab = "Fat (g/day)", cex = 1.1)
mtext("E",adj = 0.025, padj = 1.5)

boxplot(df_filt$Alcohol, ylab = "Weekly Alcohol", cex = 1.1)
mtext("F",adj = 0.025, padj = 1.5)

boxplot(df_filt$Cholesterol, ylab = "CHL (mg/day)", cex = 1.1)
mtext("G",adj = 0.025, padj = 1.5)

boxplot(df_filt$BetaDiet, ylab = "DBC (mcg/day)", cex = 1.1)
mtext("H",adj = 0.025, padj = 1.5)

#create tables for categorical variables

table(df_filt$Sex)

table(df_filt$VitaminUse)

table(df_filt$PriorSmoke)

#create cross-tabulations for categorical variables

table(df_filt$Sex, df_filt$VitaminUse)

table(df_filt$Sex, df_filt$PriorSmoke)

table(df_filt$VitaminUse, df_filt$PriorSmoke)

#create pairs figure for continuous

pairs(df_filt[c(2:10)])

#create plot for just calories and fat

plot(df_filt$Calories, df_filt$Fat, xlab = "Daily calories", ylab = "Fat", pch = 16, cex = 0.9)

#fit initial model

mod_full <- lm(BetaPlasma ~ Age + Quetelet + Calories + Fat + Fiber + Alcohol + Cholesterol + BetaDiet + factor(Sex) + factor(VitaminUse) + factor(PriorSmoke), data = df_filt)

summary(mod_full)

#diagnostics

dev.off()

plot(mod_full$fitted.values, studres(mod_full)) #fitted values vs studres

par(mfrow = c(4,2), mar = c(4,2,1,1))

for (ind in 2:9){ #predictors vs studres
  plot(df_filt[,ind], studres(mod_full), xlab = colnames(df_filt)[ind])
  abline(h = 0, col = "red")
}

par(mfrow = c(1,3))

for (ind in 11:13){
  plot(factor(df_filt[,ind]), studres(mod_full), xlab = colnames(df_filt)[ind], ylab = "")
}

dev.off() #reset par

par(mar = c(4.2,4.5,0.3,1))
plot(mod_full, 4) #cook's distance

dev.off()

qqnorm(mod_full$residuals, main = "") #qqplot of normal residuals
qqline(mod_full$residuals)

vif(mod_full) #gvif

#reduce model due to multicollinearity

mod_red <- lm(BetaPlasma ~ Age + Quetelet + Fat + Fiber + Alcohol + Cholesterol + BetaDiet + factor(Sex) + factor(VitaminUse) + factor(PriorSmoke), data = df_filt)

summary(mod_red)

#next round of diagnostics

plot(mod_red$fitted.values, studres(mod_red)) #fitted values vs studres

par(mfrow = c(4,2), mar = c(4,2,1,1))

for (ind in 2:9){ #predictors vs studres
  plot(df_filt[,ind], studres(mod_red), xlab = colnames(df_filt)[ind])
  abline(h = 0, col = "red")
}

par(mfrow = c(1,3))

for (ind in 11:13){
  plot(factor(df_filt[,ind]), studres(mod_red), xlab = colnames(df_filt)[ind], ylab = "")
}

dev.off() #reset par

par(mar = c(4.2,4.5,0.4,1))
plot(mod_red, 4) #cook's distance

dev.off()

qqnorm(mod_red$residuals, main = "") #qqplot of normal residuals
qqline(mod_red$residuals)

vif(mod_red) #gvif

#create log(response) due to non-normality in residuals, refit model

df$log.BetaPlasma <- log(df$BetaPlasma)

mod_red_log <- lm(log.BetaPlasma ~ Age + Quetelet + Fat + Fiber + Alcohol + Cholesterol + BetaDiet + factor(Sex) + factor(VitaminUse) + factor(PriorSmoke), data = df_filt)

summary(mod_red_log)

#next round of diagnostics

plot(mod_red_log$fitted.values, studres(mod_red)) #fitted values vs studres

par(mfrow = c(4,2), mar = c(4,2,1,1))

for (ind in 2:9){ #predictors vs studres
  plot(df_filt[,ind], studres(mod_red_log), xlab = colnames(df_filt)[ind])
  abline(h = 0, col = "red")
}

par(mfrow = c(1,3))

for (ind in 11:13){
  plot(factor(df_filt[,ind]), studres(mod_red_log), xlab = colnames(df_filt)[ind], ylab = "")
}

dev.off() #reset par

par(mar = c(4.2,4.5,0.4,1))
plot(mod_red_log, 4) #cook's distance

dev.off()

qqnorm(mod_red_log$residuals, main = "") #qqplot of normal residuals
qqline(mod_red_log$residuals)

vif(mod_red_log) #gvif

#model building

fit0 <- lm(log.BetaPlasma ~ 1, data = df_filt)

step(fit0, log.BetaPlasma ~ Age + Quetelet + Fat + Fiber + Alcohol + Cholesterol + BetaDiet + factor(Sex) + factor(VitaminUse) + factor(PriorSmoke), direction = "both", trace = F, data = df_filt)

fitmain <- lm(formula = log.BetaPlasma ~ Quetelet + factor(VitaminUse) + 
                Fiber + Fat + factor(PriorSmoke) + BetaDiet + Age + factor(Sex), 
              data = df_filt)

step(fitmain, scope = .~.^2, data = df_filt, trace = F)

fitint <- lm(formula = log.BetaPlasma ~ Quetelet + factor(VitaminUse) + 
                 Fiber + Fat + factor(PriorSmoke) + BetaDiet + Age + factor(Sex) + 
                 factor(VitaminUse):BetaDiet + factor(PriorSmoke):BetaDiet + 
                 factor(VitaminUse):factor(PriorSmoke), data = df_filt)

write.csv(summary(fitint)$coefficients, "model_table_final.csv") #save t-test table

write.csv(anova(fitint), "typeI_anova_final.csv") #save type I anova table

#final round of diagnostics

dev.off()

plot(fitint$fitted.values, studres(fitint), xlab = "Fitted values", ylab = "Residuals") #fitted values vs studres
abline(h = 0, col = "red")

colnames(df_filt)[colnames(df_filt) %in% c("Quetelet", "BetaDiet")] <- c("BMI", "DBC") #change column names to match syntax in paper

par(mfrow = c(2,4), mar = c(4,2,1,1))
for (ind in 2:9){ #predictors vs studres
  plot(df_filt[,ind], studres(fitint), xlab = colnames(df_filt)[ind])
  abline(h = 0, col = "red")
}

colnames(df_filt)[colnames(df_filt) %in% c("VitaminUse", "PriorSmoke")] <- c("Vitamin", "Smoking") #change column names to match syntax in paper

df_filt$Smoking[df_filt$Smoking == 1] <- rep("Never", length(df_filt$Smoking[df_filt$Smoking == 1])) #change variable values to match syntax in paper
df_filt$Smoking[df_filt$Smoking == 2] <- rep("Former", length(df_filt$Smoking[df_filt$Smoking == 2]))
df_filt$Smoking[df_filt$Smoking == 3] <- rep("Current", length(df_filt$Smoking[df_filt$Smoking == 3]))
df_filt$Vitamin[df_filt$Vitamin == "No"] <- rep("None", length(df_filt$Vitamin[df_filt$Vitamin == "No"]))

par(mfrow = c(1,3), mar = c(5,2,1,2))

for (ind in 11:13){
  plot(factor(df_filt[,ind]), studres(fitint), xlab = colnames(df_filt)[ind], ylab = "")
}

dev.off() #reset par

par(mar = c(4.2,4.5,0.4,1))
plot(fitint, 4) #cook's distance

dev.off()

qqnorm(fitint$residuals, main = "") #qqplot of normal residuals
qqline(fitint$residuals)

gvif_final <- vif(fitint) #gvif

gvif_final[,3]**2
