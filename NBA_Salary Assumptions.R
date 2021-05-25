library(tidyverse) #for graphs
library(ggfortify) #for some fancy graph stuff
library(car) #for analysis
library(readxl) #to read specific sheets
library(corrplot) #for correlation stuff
setwd("~/Downloads") #or wherever you have the file

#Download all the data from the excel sheet
teams <- read_excel("NbaSalary.xlsx", sheet = "18-19 Team")
salaries <- read_excel("NbaSalary.xlsx", sheet = "18-19 Salary")
comb <- read_excel("NbaSalary.xlsx", sheet = "Combined")
comb <- comb[c(1:6, 36:40)] #taking out data I'm not using. First 5 columns are maybe numbers we'll try to predict, 
#next 5 columns are the salary stuff that could be predictive.

summary(comb)

## Assumption Checking

#### Scatterplot matrix of the data.

plot(comb, pch = 19)

#### Fit a linear regression model

nbasalary_ptsdiff.lm <- lm(PTS_DIFF ~ `Total Salary` + `Avg Salary` + `Median Salary` + `Max Salary` + `Min Salary`, data = comb)
nbasalary_minelo.lm <- lm(`MIN ELO` ~ `Total Salary` + `Avg Salary` + `Median Salary` + `Max Salary` + `Min Salary`, data = comb)

summary(nbasalary_ptsdiff.lm)

comb$residuals <- nbasalary_ptsdiff.lm$residuals
comb$fitted.values <- nbasalary_ptsdiff.lm$fitted.values

### Linearity

#### Residuals vs. Fitted Values plot
sz <- 20
salary.resid.plot <- autoplot(nbasalary_ptsdiff.lm, which = 1, ncol = 1, nrow = 1) +
  theme_bw() + 
  theme(aspect.ratio = 1)
salary.resid.plot

#### Residuals vs variable plots
bf.pred.plot.total <- ggplot(data = comb, mapping = aes(x = `Total Salary`, y = residuals)) +
  geom_point() +
  theme_bw() + 
  theme(aspect.ratio = 1)
bf.pred.plot.total
bf.pred.plot.avg <- ggplot(data = comb, mapping = aes(x = `Avg Salary`, y = residuals)) +
  geom_point() +
  theme_bw() + 
  theme(aspect.ratio = 1)
bf.pred.plot.avg
bf.pred.plot.max <- ggplot(data = comb, mapping = aes(x = `Max Salary`, y = residuals)) +
  geom_point() +
  theme_bw() + 
  theme(aspect.ratio = 1)
bf.pred.plot.max
bf.pred.plot.min <- ggplot(data = comb, mapping = aes(x = `Min Salary`, y = residuals)) +
  geom_point() +
  theme_bw() + 
  theme(aspect.ratio = 1)
bf.pred.plot.min

#### Added-Variable plots
avPlots(nbasalary_ptsdiff.lm)

### Normality of residuals and centered at zero

#### Boxplot of residuals
salary.box <- ggplot(data = comb, mapping = aes(y = residuals)) +
  geom_boxplot() +
  theme(aspect.ratio = 1)
salary.box

#### Histogram of residuals
salary.hist <- ggplot(data = comb, mapping = aes(x = residuals)) +
  geom_histogram(mapping = aes(y = ..density..), binwidth = 4) +
  stat_function(fun = dnorm, color = "red", size = 2, 
                args = list(mean = mean(comb$residuals), sd = sd(comb$residuals))) +
  theme_bw() + 
  theme(aspect.ratio = 1)
  # scale_x_continuous(limits = c(-30, 40), 
  #                    breaks = seq(-30, 40, by = 10), 
  #                    minor_breaks = seq(-30, 40, by = 10)) +
  # scale_y_continuous(limits = c(0, .06),                  
  #                    breaks = seq(0, .06, by = .02), 
  #                    minor_breaks = seq(0, .06, by = .02))
salary.hist

#### QQ plot
bf.qq.plot <- autoplot(nbasalary_ptsdiff.lm, which = 2, ncol = 1, nrow = 1) +
  theme_bw() + 
  theme(aspect.ratio = 1)
bf.qq.plot

#### Shapiro-Wilke test 
shapiro.test(comb$residuals)

### The residuals have equal/constant variance across all values of X

salary.resid.plot

### The model describes all observations

#### DFBETAS 
comb$dfbetas_speed <- as.vector(dfbetas(nbasalary_ptsdiff.lm)[, 2])

ggplot(data = comb) + 
  geom_point(mapping = aes(x = as.numeric(rownames(comb)), 
                           y = abs(dfbetas_speed))) +
  theme_bw() +
  ylab("Absolute Value of DFBETAS for Speed") +
  xlab("Observation Number") +
  
  geom_hline(mapping = aes(yintercept = 2 / sqrt(length(dfbetas_speed))),
             color = "red", linetype = "dashed") + 
  scale_x_continuous(limits = c(0, 35)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme(aspect.ratio = 1)

comb %>% 
  mutate(rowNum = row.names(comb)) %>%  
  filter(abs(dfbetas_speed) > 2 / 
           sqrt(length(rownames(comb)))) %>%  
  arrange(desc(abs(dfbetas_speed))) 
#### DFFITS
comb$dffits <- dffits(nbasalary_ptsdiff.lm)

ggplot(data = comb) + 
  geom_point(mapping = aes(x = as.numeric(rownames(comb)), 
                           y = abs(dffits))) +
  theme_bw() +
  ylab("Absolute Value of DFFITS for Y") +
  xlab("Observation Number") +
  geom_hline(mapping = aes(yintercept = 2 * sqrt(length(nbasalary_ptsdiff.lm$coefficients) /
                                                   length(dffits))),
             color = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(0, 35)) +
  scale_y_continuous(limits = c(0, 1.5)) +
  theme(aspect.ratio = 1)

comb %>% 
  mutate(rowNum = row.names(comb)) %>%   
  filter(abs(dffits) > 2 * sqrt(length(comb_lm$coefficients) / 
                                  length(dffits))) %>%
  arrange(desc(abs(dffits)))  

### No multicollinearity

bf.vifs <- vif(nbasalary_ptsdiff.lm)
bf.vifs
mean(bf.vifs)
