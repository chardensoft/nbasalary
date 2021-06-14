##Version 2 of model
library(tidyverse) #for graphs
library(ggfortify) #for some fancy graph stuff
library(car) #for analysis
library(readxl) #to read specific sheets
library(corrplot) #for correlation stuff
library(bestglm)
library(glmnet)
library(gridExtra)
library(randomForest)
set.seed(seed = 3412)
rm(list = ls())

name <- "master.csv" #adjust to match the name of your data sheet, 
#put the id in column 1, the data you're trying to predict in column 2 and explanatory variables after

master <- read.csv(file = name)

master_clean <- master[is.finite(rowSums(master[,-1])),]
master_var_reg <- master_clean[,-1]

## Split into training and testing data
train_rows <- sample(1:dim(master_var_reg)[1], floor(.8*dim(master_var_reg)[1]), replace = FALSE)

train <- master_var_reg[train_rows,]
test <- master_var_reg[-train_rows,]

### Initial Linear Model with all variables
nbasalary_all_lm <- lm(MaxElo ~ ., data = train)
summary(nbasalary_all_lm)

# master_var_clean$residuals <- nbasalary_all_lm$residuals
# master_var_clean$fitted.values <- nbasalary_all_lm$fitted.values

#### Plots
plots <- list()
for (i in 2:length(train)) {
plots[[i-1]] <- ggplot(data = train, mapping = aes_string(y = names(train)[i], 
                                                                   x = names(train)[1])) +
  geom_point()
}

# do.call(grid.arrange, plots)

# ggplot(data = train, mapping = aes(y = tot_sal, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = tot_spMP, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = tot_spWS, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = tot_spBPM, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = tot_spPTS, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = tot_spPER, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = tot_spVORP, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = tot_spUSG, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# 
# ggplot(data = train, mapping = aes(y = avg_sal, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = avg_spMP, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = avg_spWS, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = avg_spBPM, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = avg_spPTS, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = avg_spPER, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = avg_spVORP, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = avg_spUSG, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# 
# ggplot(data = train, mapping = aes(y = max_sal, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = max_spMP, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = max_spWS, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = max_spBPM, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = max_spPTS, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = max_spPER, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = max_spVORP, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = max_spUSG, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# 
# ggplot(data = train, mapping = aes(y = min_sal, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = min_spMP, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = min_spWS, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = min_spBPM, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = min_spPTS, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = min_spPER, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = min_spVORP, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = min_spUSG, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# 
# ggplot(data = train, mapping = aes(y = std_sal, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = std_spMP, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = std_spWS, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = std_spBPM, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = std_spPTS, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = std_spPER, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = std_spVORP, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = std_spUSG, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# 
# ggplot(data = train, mapping = aes(y = sal_C, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = sal_PF, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = sal_PG, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = sal_SF, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = sal_SG, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = sal_Bench, x = MaxElo)) +
#   geom_point() +
#   theme_bw()
# ggplot(data = train, mapping = aes(y = sal_Start, x = MaxElo)) +
#   geom_point() +
#   theme_bw()


##### MultiCollinearity

corrplot(cor(train), type = "upper")

#### Variable Selection

#### Organizing variables
master_var_reg_selection <- train[, c(2:dim(train)[2], 1)]


##### LASSO
salary_x <- as.matrix(master_var_reg_selection[, 1:(dim(master_var_reg_selection)[2]-1)])
salary_y <- master_var_reg_selection[, dim(master_var_reg_selection)[2]]

salary_lasso_cv <- cv.glmnet(x = salary_x,
                             y = salary_y, 
                             type.measure = "mse", 
                             alpha = 1)  

c_lasso <- coef(salary_lasso_cv,s='lambda.1se',exact=TRUE)
inds_lasso <- which(c_lasso!=0)
variables_lasso <- row.names(c_lasso)[inds_lasso]
variables_lasso <- variables_lasso[-1]
fmla_lasso <- as.formula(paste(names(train)[1], " ~ ", paste(variables_lasso, collapse= "+")))

nbasalary_lasso_lm <- lm(fmla_lasso, data = train)
summary(nbasalary_lasso_lm)

#### Ridge Regression
salary_ridge_cv <- cv.glmnet(x = salary_x,
                             y = salary_y, 
                             type.measure = "mse", 
                             alpha = 0)  

c_ridge <- coef(salary_ridge_cv,s='lambda.1se',exact=TRUE)
inds_ridge <- which(c_ridge!=0)
variables_ridge <- row.names(c_ridge)[inds_ridge]
variables_ridge <- variables_ridge[-1]
fmla_ridge <- as.formula(paste(names(train)[1], " ~ ", paste(variables_ridge, collapse= "+")))

nbasalary_ridge_lm <- lm(fmla_ridge, data = train )
summary(nbasalary_ridge_lm)

##### Elastic NET
salary_elastic_net_cv <- cv.glmnet(x = salary_x,
                                   y = salary_y, 
                                   type.measure = "mse", 
                                   alpha = 0.5)

c_en<-coef(salary_elastic_net_cv,s='lambda.1se',exact=TRUE)
inds_en<-which(c_en!=0)
variables_en<-row.names(c_en)[inds_en]
variables_en<-variables_en[-1]
fmla_en <- as.formula(paste(names(train)[1], " ~ ", paste(variables_en, collapse= "+")))

nbasalary_elastic_lm <- lm(fmla_en, data = train )
summary(nbasalary_elastic_lm)

###New data set with lasso variables

selected_vars <- master_var_reg_selection[c(which(names(master_var_reg_selection) %in% variables_lasso), 48)]

selected_plots <- list()
for (i in 2:length(selected_vars)) {
  selected_plots[[i-1]] <- ggplot(data = selected_vars, mapping = aes_string(y = names(selected_vars)[i], 
                                                                             x = names(train)[1])) +
    geom_point()
}

#### Multicollinearity

corrplot(cor(selected_vars), type = "upper")


##Eliminate 5,9,12,13,10,15,3,
##due to multicollinearity and added back sal_Bench, sal_PG, sal_C, max_sal, tot_sal to take a closer

# selected_vars <- selected_vars[-c(3,5,10)]
# selected_vars <- cbind(tot_sal = train$tot_sal, selected_vars[1:15], max_sal = train$max_sal,
#                        sal_C = train$sal_C,
#                        selected_vars[16:20], sal_Bench = train$sal_Bench,
#                        selected_vars[21])
# 
# corrplot(cor(selected_vars), type = "upper")
# # 
# # ggplot(data = train, mapping = aes(y = max_sal, x = std_sal)) +
# #     geom_point() +
# #     theme_bw()
# # ggplot(data = train, mapping = aes(y = std_sal, x = MaxElo)) +
# #   geom_point() +
# #   theme_bw()
# # ggplot(data = train, mapping = aes(y = max_sal, x = MaxElo)) +
# #   geom_point() +
# #   theme_bw()
# # ggplot(data = train, mapping = aes(y = sal_Bench, x = MaxElo)) +
# #   geom_point() +
# #   theme_bw()
# # ggplot(data = train, mapping = aes(y = sal_Start, x = MaxElo)) +
# #   geom_point() +
# #   theme_bw()
# # 
# # ##Further remove avg_spPTS, min_spVORP, max_sal, tot_spVORP
# # 
# # selected_vars <- selected_vars[-c(6, 8, 4, 14)]
# # corrplot(cor(selected_vars), type = "upper")

## New Linear model with final variables
variables <- names(selected_vars)[-c(dim(selected_vars)[2])]
fmla <- as.formula(paste(names(train)[1], " ~ ", paste(variables, collapse= "+")))

nbasalary_lm <- lm(fmla, data = train)
summary(nbasalary_lm)

## Make Random Forest model with final variables
rf_model <- randomForest(fmla, data = train)
summary(rf_model)
varImpPlot(rf_model)
plot(rf_model)

## Testing both:
### Linear Model:
lin_results <- data.frame(cbind(master_clean$id[-train_rows], round(predict(nbasalary_lm, test), 3), 
                                master_clean$MaxElo[-train_rows]))
names(lin_results) <- c("id", "predicted", "actual")
lin_results$predicted <- as.numeric(lin_results$predicted)
lin_results$actual <- as.numeric(lin_results$actual)
lin_results$diff <- lin_results$actual - lin_results$predicted
lin_perc_wn_100 <- length(which(abs(lin_results$diff) < 100)) / length(lin_results$diff)


### Random Forest Model
rf_results <- data.frame(cbind(master_clean$id[-train_rows], round(predict(rf_model, test), 3),
                                    master_clean$MaxElo[-train_rows]))
names(rf_results) <- c("id", "predicted", "actual")
rf_results$predicted <- as.numeric(rf_results$predicted)
rf_results$actual <- as.numeric(rf_results$actual)
rf_results$diff <- rf_results$actual - rf_results$predicted
rf_perc_wn_100 <- length(which(abs(rf_results$diff) < 100)) / length(rf_results$diff)





