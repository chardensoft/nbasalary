##Version 2 of model
library(tidyverse) #for graphs
library(ggfortify) #for some fancy graph stuff
library(car) #for analysis
library(readxl) #to read specific sheets
library(corrplot) #for correlation stuff


master_var <- master[,-1]
master_var_clean <- master_var[is.finite(rowSums(master_var)),]
master_var_reg <- master_var_clean

### Initial Linear Model with all variables
nbasalary_lm <- lm(MaxElo ~ ., data = master_var_reg)
summary(nbasalary_lm)

master_var_clean$residuals <- nbasalary_lm$residuals
master_var_clean$fitted.values <- nbasalary_lm$fitted.values

## Assumption Checking

### No multicollinearity
bf.vifs <- vif(nbasalary_lasso_lm)
bf.vifs
mean(bf.vifs)

### Linearity

#### Residuals vs. Fitted Values plot
salary.resid.plot <- autoplot(nbasalary_lasso_lm, which = 1, ncol = 1, nrow = 1) +
  theme_bw() + 
  theme(aspect.ratio = 1)
salary.resid.plot

#### Added-Variable plots
avPlots(nbasalary_lm)

### Normality of residuals and centered at zero

#### Boxplot of residuals
salary.box <- ggplot(data = master_var_clean, mapping = aes(y = residuals)) +
  geom_boxplot() +
  theme(aspect.ratio = 1)
salary.box

#### Histogram of residuals
salary.hist <- ggplot(data = master_var_clean, mapping = aes(x = residuals)) +
  geom_histogram(mapping = aes(y = ..density..), binwidth = 50) +
  stat_function(fun = dnorm, color = "red", size = 2, 
                args = list(mean = mean(master_var_clean$residuals), sd = sd(master_var_clean$residuals))) +
  theme_bw() + 
  theme(aspect.ratio = 1)

salary.hist

#### QQ plot
bf.qq.plot <- autoplot(nbasalary_lm, which = 2, ncol = 1, nrow = 1) +
  theme_bw() + 
  theme(aspect.ratio = 1)
bf.qq.plot

#### Shapiro-Wilke test 
shapiro.test(master_var_clean$residuals)

### The residuals have equal/constant variance across all values of X

salary.resid.plot

### The model describes all observations

#### DFFITS
master_var_clean$dffits <- dffits(nbasalary_lm)

ggplot(data = master_var_clean) + 
  geom_point(mapping = aes(x = as.numeric(rownames(master_var_clean)), 
                           y = abs(dffits))) +
  theme_bw() +
  ylab("Absolute Value of DFFITS for Y") +
  xlab("Observation Number") +
  geom_hline(mapping = aes(yintercept = 2 * sqrt(length(nbasalary_lm$coefficients) /
                                                   length(dffits))),
             color = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(0, dim(master_var_clean)[1])) +
  scale_y_continuous(limits = (c(0, max(dffits(nbasalary_lm)) + 0.5))) +
  theme(aspect.ratio = 1) 

#### Variable Selection

library(bestglm)

#### Organizing variables
master_var_reg_selection <- master_var_reg[, c(2:dim(master_var_reg)[2], 1)]

##### LASSO
library(glmnet)
salary_x <- as.matrix(master_var_reg_selection[, 1:(dim(master_var_reg_selection)[2]-1)])
salary_y <- master_var_reg_selection[, dim(master_var_reg_selection)[2]]

salary_lasso_cv <- cv.glmnet(x = salary_x,
                             y = salary_y, 
                             type.measure = "mse", 
                             alpha = 1)  

c_lasso <- coef(salary_lasso_cv,s='lambda.1se',exact=TRUE)
inds_lasso <- which(c!=0)
variables_lasso <- row.names(c)[inds_lasso]
variables_lasso <- variables_lasso[-1]
fmla_lasso <- as.formula(paste("MaxElo ~ ", paste(variables_lasso, collapse= "+")))

nbasalary_lasso_lm <- lm(fmla_lasso, data = master_var_reg )
summary(nbasalary_lasso_lm)

#### Ridge Regression
salary_ridge_cv <- cv.glmnet(x = salary_x,
                             y = salary_y, 
                             type.measure = "mse", 
                             alpha = 0)  

c_ridge <- coef(salary_ridge_cv,s='lambda.1se',exact=TRUE)
inds_ridge <- which(c!=0)
variables_ridge <- row.names(c)[inds_ridge]
variables_ridge <- variables_ridge[-1]
fmla_ridge <- as.formula(paste("MaxElo ~ ", paste(variables_ridge, collapse= "+")))

nbasalary_ridge_lm <- lm(fmla_ridge, data = master_var_reg )
summary(nbasalary_ridge_lm)

##### Elastic NET
salary_elastic_net_cv <- cv.glmnet(x = salary_x,
                                   y = salary_y, 
                                   type.measure = "mse", 
                                   alpha = 0.5)

c_en<-coef(salary_elastic_net_cv,s='lambda.1se',exact=TRUE)
inds_en<-which(c!=0)
variables_en<-row.names(c)[inds_en]
variables_en<-variables_en[-1]
fmla_en <- as.formula(paste("MaxElo ~ ", paste(variables_en, collapse= "+")))

nbasalary_elastic_lm <- lm(fmla_en, data = master_var_reg )
summary(nbasalary_elastic_lm)
