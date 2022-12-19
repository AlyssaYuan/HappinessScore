plot(Generosity, GDP.per.capita)

# Regression Happiness Score and Generosity
ols.fit.gen <- lm(Score ~ Generosity, data = GP1_Happiness_Data)
sum.fit.het <- coeftest(ols.fit.gen, vcov = vcovHC(ols.fit.1, "HC1"))
sum.fit.het
t test of coefficients:
  
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  5.24331    0.17375 30.1766   <2e-16 ***
  Generosity   0.88606    0.98377  0.9007   0.3692    
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary(ols.fit.gen)

Call:
  lm(formula = Score ~ Generosity, data = GP1_Happiness_Data)

Residuals:
  Min       1Q   Median       3Q      Max 
-2.56930 -0.81851  0.00815  0.78707  2.39012 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)   5.2433     0.1951  26.872   <2e-16 ***
  Generosity    0.8861     0.9390   0.944    0.347    
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.114 on 154 degrees of freedom
Multiple R-squared:  0.005749,	Adjusted R-squared:  -0.0007069 
F-statistic: 0.8905 on 1 and 154 DF,  p-value: 0.3468

cov(GP1_Happiness_Data$Generosity,GP1_Happiness_Data$GDP.per.capita)
[1] -0.003023055
# Since the covariance between Generosity and GDP.per.capita is negative, therefore, the two variables
# move inversely with each other. However, the value of the covariance is very close to zero 
# which indicates that Generosity and GDP.per.capita only vary together slightly.

ols.fit1 <- lm(Score ~ Generosity + GDP.per.capita, data = GP1_Happiness_Data)
sum.fit.het1 <- coeftest(ols.fit1, vcov = vcovHC(ols.fit1, "HC1"))
sum.fit.het1

t test of coefficients:
  
                Estimate Std. Error t value Pr(>|t|)    
(Intercept)     3.06884    0.17479 17.5568  < 2e-16 ***
  Generosity      1.63547    0.65048  2.5143  0.01296 *  
  GDP.per.capita  2.24930    0.12783 17.5956  < 2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary(ols.fit1)

Call:
  lm(formula = Score ~ Generosity + GDP.per.capita, data = GP1_Happiness_Data)

Residuals:
  Min       1Q   Median       3Q      Max 
-1.96325 -0.43161  0.00491  0.46783  1.53688 

Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
(Intercept)      3.0688     0.1741  17.627  < 2e-16 ***
  Generosity       1.6355     0.5609   2.916  0.00408 ** 
  GDP.per.capita   2.2493     0.1341  16.771  < 2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.6631 on 153 degrees of freedom
Multiple R-squared:  0.6497,	Adjusted R-squared:  0.6451 
F-statistic: 141.9 on 2 and 153 DF,  p-value: < 2.2e-16

# When we computed a regression with Generosity and GDP.per.capita, the multiple R-squared increased
# which shows that by controlling for GDP.per.capita, we can explain more of the data than by just
# running a regression for Generosity.

# Increasing the Generosity index by one unit, increases Happiness Score by about 1.6355 units.
# We underestimated the impact Generosity has on Happiness Score. 
# Both Generosity and GDP.per.capita have a positive impact on Happiness Score, but GDP.per.capita
# has a much larger impact on Happiness Score than Generosity.