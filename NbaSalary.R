library(tidyverse) #for graphs
library(ggfortify) #for some fancy graph stuff
library(car) #for analysis
library(readxl) #to read specific sheets
library(corrplot) #for correlation stuff
setwd("~/Downloads") #or wherever you have the file

#Download all the data from the excel sheet
#teams <- read_excel("NbaSalary.xlsx", sheet = "18-19 Team")
#salaries <- read_excel("NbaSalary.xlsx", sheet = "18-19 Salary")
comb <- read_excel("NbaSalary.xlsx", sheet = "Combined")
comb <- comb[c(1:6, 36:40)] #taking out data I'm not using. First 5 columns are maybe numbers we'll try to predict, 
#next 5 columns are the salary stuff that could be predictive.

summary(comb)



############# IGNORE EVERYTHING UNDER HERE, JUST OLD NOTES THAT COULD MAYBE BE HELPFUL #################


#### 2. Create and print a scatterplot matrix of the data.

```{r, fig.align='center'}
# your code here
plot(bodyfat, pch = 19)
```

#### 3. Based on the scatterplot matrix, briefly explain which variables you think will be "significant" for predicting brozek and which variables you think will *not* be helpful at predicting brozek. Explain how the scatterplot helped determine your answers.

Based on the scatterplot, I think abdom will be the most helpful, followed by neck, chest and weight. The problem with using all of these will most likely be multicollinearity, but we will explor that a little later. I see that abdom is the most linearly correlated, so I believe it will be most helpful.

#### 4. Create and print a correlation matrix.

```{r}
# your code here
round(cor(bodyfat), 2)
corrplot(cor(bodyfat), type = "upper")
```

#### 5. Based on the scatterplot matrix and the correlation matrix, are their any pairs of variables that you suspect will cause a problem for the multicollinearity assumption? If so, which ones?

Yes, I am worried about weight, neck, chest and abdom all related to each other. They all seem to have a strong correlation with one another as well as brozek, so I am certainly thinking they will cause a problem with multicollinearity.

#### 6. Fit a multiple linear regression model to the data (no transformations). Print a summary of the results. Save the residuals and fitted values to the `bodyfat` data frame.

```{r}
# your code here
bodyfat.lm <- lm(brozek ~ ., data = bodyfat)
summary(bodyfat.lm)
bodyfat$residuals <- bodyfat.lm$residuals
bodyfat$fitted.values <- bodyfat.lm$fitted.values
```

#### 7. Briefly comment on the "significance" of the variables: were you surprised by the results? Are there any variables that are significant that you think shouldn't be? Are there any variables that are not significant that you think should be?

The model tells us that weight, neck and abdom are significant and I feel that's about correct. Somewhat suprised chest isn't in there, given how similar it lined up to those others. 

#### 8. Briefly comment on the sign (+/-) of the coefficients for the variables. Are their any variables where the sign is the opposite of what you expected?

I'm more concerned here, as I definitely expected weight, and neck to be positively correlated and they are somehow negative. We've definitely got a problem here. 

#### 9. Mathematically write out the multiple linear regression model for this data set using the coefficients you found above (do not use betas). Do not use "X" and "Y" in your model - use variable names that are fairly descriptive.

$\widehat{\text{bodyfat}_i} = -20.1 + .005\text{age}_i - .087\text{weight}_i - .14\text{height}_i - .44\text{neck}_i + .00048\text{chest}_i + 0.88\text{abdom}_i$
  
  #### 10. *Assuming* the model assumptions are all met, how would you interpret the coefficient for Weight?
  
  If age, weight, height, neck, chest and abdom are all equal to 0, we would expect body fat to be -20.1.

#### 11. Briefly explain what it means to "hold all else constant," when you interpret the coefficient for Weight?

Holding all else constant, essentially means that if we don't change any of the other variables (age, height, chest, neck, abdom) or they are all "constant" and we just look at one, in this case, weight. Assuming this, we would expect the body fat to decrease by .087 for each 1 unit increase in weight. 

#### 12. Briefly explain what the F-test indicates, as reported in the model output from question 6.

Our F-test has a significant p-value so it indicates that we do have a significant relationship between body fat and the variables age, weight, height, chest, neck and abdom.

#### 13. Briefly interpret the *adjusted* R-squared, as reported in the model output from question 6.

71.51% of the body fat is explained by the variables age, weight, height, chest, neck and abdom after adjusting for the number of variables in the model.

### Questions 14-20 involve using diagnostics to determine if the linear regression assumptions are met.  Now check to make sure each of the following assumptions for multiple linear regression are met. For each assumption, (1) perform appropriate diagnostics to determine if the assumption is violated, and (2) explain whether or not you think the assumption is violated and why you think that.

#### 14. (L) The X's vs Y are linear (use the residual vs. predictor plots, partial regression plots, and one other diagnostic tool of your choice)

```{r, fig.align='center'}
# your code here
sz <- 20
bf.resid.plot <- autoplot(bodyfat.lm, which = 1, ncol = 1, nrow = 1) +
  theme_bw() + 
  theme(aspect.ratio = 1)
bf.resid.plot
```

```{r, fig.align='center'}
# your code here
bf.pred.plot.age <- ggplot(data = bodyfat, mapping = aes(x = age, y = residuals)) +
  geom_point() +
  theme_bw() + 
  theme(aspect.ratio = 1)
bf.pred.plot.age
bf.pred.plot.weight <- ggplot(data = bodyfat, mapping = aes(x = weight, y = residuals)) +
  geom_point() +
  theme_bw() + 
  theme(aspect.ratio = 1)
bf.pred.plot.weight
bf.pred.plot.height <- ggplot(data = bodyfat, mapping = aes(x = height, y = residuals)) +
  geom_point() +
  theme_bw() + 
  theme(aspect.ratio = 1)
bf.pred.plot.height
bf.pred.plot.neck <- ggplot(data = bodyfat, mapping = aes(x = neck, y = residuals)) +
  geom_point() +
  theme_bw() + 
  theme(aspect.ratio = 1)
bf.pred.plot.neck
bf.pred.plot.chest <- ggplot(data = bodyfat, mapping = aes(x = chest, y = residuals)) +
  geom_point() +
  theme_bw() + 
  theme(aspect.ratio = 1)
bf.pred.plot.chest
bf.pred.plot.abdom <- ggplot(data = bodyfat, mapping = aes(x = abdom, y = residuals)) +
  geom_point() +
  theme_bw() + 
  theme(aspect.ratio = 1)
bf.pred.plot.abdom
```

```{r, fig.align='center'}
# your code here
avPlots(bodyfat.lm)
```

There are a few points toward the tail end of the residuals that look concerning, but not terrible. the Added-Variable Plots, show good linearity, so I feel we can move forward saying this assumption is met.

#### 15. (I) The residuals are independent (no diagnostic tools - just think about how the data was collected and briefly write your thoughts)

I'd say the variables are not independent honestly, I'm not sure how my weight isn't at least somewhat dependent on my height.

#### 16. (N) The residuals are normally distributed and centered at zero (use at least 2 diagnostic tools)

```{r, fig.align='center'}
# your code here
bf.box <- ggplot(data = bodyfat, mapping = aes(y = residuals)) +
  geom_boxplot() +
  theme(aspect.ratio = 1)
bf.box
```

```{r, fig.align='center'}
# your code here
bf.hist <- ggplot(data = bodyfat, mapping = aes(x = residuals)) +
  geom_histogram(mapping = aes(y = ..density..), binwidth = 4) +
  stat_function(fun = dnorm, color = "red", size = 2, 
                args = list(mean = mean(bodyfat$residuals), sd = sd(bodyfat$residuals))) +
  theme_bw() + 
  theme(aspect.ratio = 1)
  # scale_x_continuous(limits = c(-30, 40), 
  #                    breaks = seq(-30, 40, by = 10), 
  #                    minor_breaks = seq(-30, 40, by = 10)) +
  # scale_y_continuous(limits = c(0, .06),                  
  #                    breaks = seq(0, .06, by = .02), 
  #                    minor_breaks = seq(0, .06, by = .02))
bf.hist
```

```{r, fig.align='center'}
# your code here
bf.qq.plot <- autoplot(bodyfat.lm, which = 2, ncol = 1, nrow = 1) +
  theme_bw() + 
  theme(aspect.ratio = 1)
bf.qq.plot
```

```{r, fig.align='center'}
# your code here
shapiro.test(bodyfat$residuals)
```

These look great! Very normally distributed, large p-value from the Shapiro-Wilk. This assumption is definitely met.

#### 17. (E) The residuals have equal/constant variance across all values of X (use one diagnostic tool)

```{r, fig.align='center'}
# your code here
bf.resid.plot
```

I'd say this is good. There isn't really any curves or bad patterns in the layout of the points, they're mostly consistent across the plot. So we can move forward with the assumption that the residuals are homoscedastic being met.

#### 18. (A) The model describes all observations (i.e., there are no influential points) (use the DFBETAS, DFFITS, partial regression plots (no need to re-plot them here - just refer to the plots you made above), and one other diagnostic tool of your choice)

```{r, fig.align='center'}
# your code here
bodyfat$cooksd <- cooks.distance(bodyfat.lm)
bodyfat[bodyfat$cooksd >= 4 / length(bodyfat$cooksd), ]
```

```{r, fig.align='center'}
bf.dfbetas <- as.data.frame(dfbetas(bodyfat.lm))
bf.dfbetas$obs <- 1:length(bodyfat$brozek)

ggplot(data = bf.dfbetas) + 
  geom_point(mapping = aes(x = obs, y = abs(age))) +
  ylab("Absolute Value of DFBETAS") +
  xlab("Observation Number") +
  # geom_hline(mapping = aes(yintercept = 1),
  # color = "red", linetype = "dashed") +  # for n <= 30
  geom_hline(mapping = aes(yintercept = 2 / sqrt(length(obs))),
             color = "red", linetype = "dashed") +  # for n > 30
  # scale_x_continuous(limits = c(0, 62)) +
  # scale_y_continuous(limits = c(0, 1.08)) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = sz - 5),
    axis.title.y = element_text(size = sz - 5),
    axis.text = element_text(size = sz - 5),
    aspect.ratio = 1,
    plot.title = element_blank()
  )

bf.dfbetas.1 <- bf.dfbetas[abs(bf.dfbetas$age) > 2 / sqrt(length(bf.dfbetas$obs)), ]
bf.dfbetas.1[order(bf.dfbetas.1$age), ]

ggplot(data = bf.dfbetas) + 
  geom_point(mapping = aes(x = obs, y = abs(weight))) +
  ylab("Absolute Value of DFBETAS") +
  xlab("Observation Number") +
  # geom_hline(mapping = aes(yintercept = 1),
  # color = "red", linetype = "dashed") +  # for n <= 30
  geom_hline(mapping = aes(yintercept = 2 / sqrt(length(obs))),
             color = "red", linetype = "dashed") +  # for n > 30
  # scale_x_continuous(limits = c(0, 62)) +
  # scale_y_continuous(limits = c(0, 1.08)) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = sz - 5),
    axis.title.y = element_text(size = sz - 5),
    axis.text = element_text(size = sz - 5),
    aspect.ratio = 1,
    plot.title = element_blank()
  )

bf.dfbetas.2 <- bf.dfbetas[abs(bf.dfbetas$weight) > 2 / sqrt(length(bf.dfbetas$obs)), ]
bf.dfbetas.2[order(bf.dfbetas.1$weight), ]

ggplot(data = bf.dfbetas) + 
  geom_point(mapping = aes(x = obs, y = abs(height))) +
  ylab("Absolute Value of DFBETAS") +
  xlab("Observation Number") +
  # geom_hline(mapping = aes(yintercept = 1),
  # color = "red", linetype = "dashed") +  # for n <= 30
  geom_hline(mapping = aes(yintercept = 2 / sqrt(length(obs))),
             color = "red", linetype = "dashed") +  # for n > 30
  # scale_x_continuous(limits = c(0, 62)) +
  # scale_y_continuous(limits = c(0, 1.08)) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = sz - 5),
    axis.title.y = element_text(size = sz - 5),
    axis.text = element_text(size = sz - 5),
    aspect.ratio = 1,
    plot.title = element_blank()
  )

bf.dfbetas.3 <- bf.dfbetas[abs(bf.dfbetas$height) > 2 / sqrt(length(bf.dfbetas$obs)), ]
bf.dfbetas.3[order(bf.dfbetas.1$height), ]

ggplot(data = bf.dfbetas) + 
  geom_point(mapping = aes(x = obs, y = abs(neck))) +
  ylab("Absolute Value of DFBETAS") +
  xlab("Observation Number") +
  # geom_hline(mapping = aes(yintercept = 1),
  # color = "red", linetype = "dashed") +  # for n <= 30
  geom_hline(mapping = aes(yintercept = 2 / sqrt(length(obs))),
             color = "red", linetype = "dashed") +  # for n > 30
  # scale_x_continuous(limits = c(0, 62)) +
  # scale_y_continuous(limits = c(0, 1.08)) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = sz - 5),
    axis.title.y = element_text(size = sz - 5),
    axis.text = element_text(size = sz - 5),
    aspect.ratio = 1,
    plot.title = element_blank()
  )

bf.dfbetas.4 <- bf.dfbetas[abs(bf.dfbetas$neck) > 2 / sqrt(length(bf.dfbetas$obs)), ]
bf.dfbetas.4[order(bf.dfbetas.1$neck), ]

ggplot(data = bf.dfbetas) + 
  geom_point(mapping = aes(x = obs, y = abs(chest))) +
  ylab("Absolute Value of DFBETAS") +
  xlab("Observation Number") +
  # geom_hline(mapping = aes(yintercept = 1),
  # color = "red", linetype = "dashed") +  # for n <= 30
  geom_hline(mapping = aes(yintercept = 2 / sqrt(length(obs))),
             color = "red", linetype = "dashed") +  # for n > 30
  # scale_x_continuous(limits = c(0, 62)) +
  # scale_y_continuous(limits = c(0, 1.08)) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = sz - 5),
    axis.title.y = element_text(size = sz - 5),
    axis.text = element_text(size = sz - 5),
    aspect.ratio = 1,
    plot.title = element_blank()
  )

bf.dfbetas.5 <- bf.dfbetas[abs(bf.dfbetas$chest) > 2 / sqrt(length(bf.dfbetas$obs)), ]
bf.dfbetas.5[order(bf.dfbetas.1$chest), ]

ggplot(data = bf.dfbetas) + 
  geom_point(mapping = aes(x = obs, y = abs(abdom))) +
  ylab("Absolute Value of DFBETAS") +
  xlab("Observation Number") +
  # geom_hline(mapping = aes(yintercept = 1),
  # color = "red", linetype = "dashed") +  # for n <= 30
  geom_hline(mapping = aes(yintercept = 2 / sqrt(length(obs))),
             color = "red", linetype = "dashed") +  # for n > 30
  # scale_x_continuous(limits = c(0, 62)) +
  # scale_y_continuous(limits = c(0, 1.08)) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = sz - 5),
    axis.title.y = element_text(size = sz - 5),
    axis.text = element_text(size = sz - 5),
    aspect.ratio = 1,
    plot.title = element_blank()
  )

bf.dfbetas.6 <- bf.dfbetas[abs(bf.dfbetas$abdom) > 2 / sqrt(length(bf.dfbetas$obs)), ]
bf.dfbetas.6[order(bf.dfbetas.1$abdom), ]
```

```{r, fig.align='center'}
bf.dffits <- data.frame("dffits" = dffits(bodyfat.lm))
bf.dffits$obs <- 1:length(bodyfat$brozek)

ggplot(data = bf.dffits) + 
  geom_point(mapping = aes(x = obs, y = abs(dffits))) +
  ylab("Absolute Value of DFFITS") +
  xlab("Observation Number") +
  # geom_hline(mapping = aes(yintercept = 1),
  #            color = "red", linetype = "dashed") +  # for n <= 30
  geom_hline(mapping = aes(yintercept = 2 * 
                             sqrt(length(bodyfat.lm$coefficients) / length(obs))),
             color = "red", linetype = "dashed") +  # for n > 30
  theme_bw() +
  # scale_x_continuous(limits = c(0, 62)) +
  # scale_y_continuous(limits = c(0, 1.08)) +
  theme(
    axis.title.x = element_text(size = sz - 5),
    axis.title.y = element_text(size = sz - 5),
    axis.text = element_text(size = sz - 5),
    aspect.ratio = 1,
    plot.title = element_blank()
  )

bf.dffits[abs(bf.dffits$dffits) > 2 * 
            sqrt(length(bodyfat.lm$coefficients) / length(bf.dffits$obs)), ]

```

I'd say we really have one observation that is badly outside the model here, observation number 39. Could be a super skinny or super heavy guy, or something else unusual. Could be bad data. We might want to consider entirely removing this variable moving forward. 

#### 19. (R) Additional predictor variables are not required (no diagnostic tools - just think about the variables you have and if there are other variables you think would help predict the response)

I think that we do have most things that would help explain body fat. No other obvious predictors that we haven't put in our model. 

#### 20. No multicollinearity (for this assumption, compare the variance inflation factors to your comments in questions 5 and 7 - using the correlation matrix and checking for non-significant coefficients that you think should be significant. Do the variance inflation factors match your assumptions from questions 5 and 7? Is this assumption met?

```{r, fig.align='center'}
# your code here
bf.vifs <- vif(bodyfat.lm)
bf.vifs
mean(bf.vifs)
```

Yes they do, it looks like there is some multicollinearity for neck and chest, but abdom and weight are the ones to really be worried about. We would probably need to remove some of these and use a variable that we felt encompassed them, I'd probably choose abdom and remove weight.

#### 21. Briefly summarize what you learned, personally, from this analysis about the statistics, model fitting process, etc.

I learned that your data isn't always what you want it to be. Multicollinearity is fairly easy to spot if you look closely at the correlation matrix. I also enjoyed learning and seeing how the same kinds of plots that applied to simple linear regression apply here as well. Also learned that EDA is very important before you move into trying to make assumptions.

#### 22. Briefly summarize what you learned from this analysis *to a non-statistician*. Write a few sentences about (1) the purpose of this data set and analysis and (2) what you learned about this data set from your analysis. Write your response as if you were addressing a business manager (avoid using statistics jargon) and just provide the main take-aways.

We were hoping to learn what variables we could use to predict body fat without having to use the complicated submersion technique. We learned that body fat is definitely explained by our chosen variables, especially abdomen size, but in order to draw any strong conclusions, we would need to remove a few factors since they are so closely related to each other. We will rework the model with a few of the variables removed and come back with a better model that should help us predict body fat without having to use submersion!