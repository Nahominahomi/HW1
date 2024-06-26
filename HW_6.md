---
title: "HW_5"
author: "Nahomi Farington"
date: "2024-03-19"
output: html_document
---

```{r}
load("C:/Users/Owner/Desktop/Psychology/R-work/V0500_Lecture1/BRFSS2022/.RData")
```

```{r}
library(ggplot2)
library(tidyverse)
library(stargazer)
```


```{r}
brfss22$Age_midpt <- fct_recode(brfss22$X_AGEG5YR, "21" = "Age 18 to 24",
                                      "27" = "Age 25 to 29", "32" = "Age 30 to 34",
                                      "37" = "Age 35 to 39", "42" = "Age 40 to 44",
                                      "47" = "Age 45 to 49", "52" = "Age 50 to 54",
                                      "57" = "Age 55 to 59", "62" = "Age 60 to 64",
                                      "67" = "Age 65 to 69", "72" = "Age 70 to 74",
                                      "77" = "Age 75 to 79", "82" = "Age 80 or older",
                                      NULL = "Dont know/Refused/Missing")
brfss22$Age_midpt <- as.numeric(levels(brfss22$Age_midpt))[brfss22$Age_midpt]
brfss22$income_midpoint <- fct_recode(brfss22$INCOME3, 
                        "7500" = "Household income less than $10,000",
                        "12500" = "Less than $15,000 ($10,000 to less than $15,000)",
                        "17500" = "Less than $20,000 ($15,000 to less than $20,000) ",
                        "22500" = "Less than $25,000 ($20,000 to less than $25,000) ",
                        "30000" = "Less than $35,000 ($25,000 to less than $35,000) ",
                        "42500" = "Less than $50,000 ($35,000 to less than $50,000) ",
                        "62500" = "Less than $75,000 ($50,000 to less than $75,000)",
                        "87500" = "Less than $100,000 ($75,000 to less than $100,000)",
                        "125000" = "Less than $150,000 ($100,000 to less than $150,000)",
                        "175000" = "Less than $200,000 ($150,000 to less than $200,000)",
                        "210000" = "$200,000 or more",
                        NULL = "Dont know/Not sure",
                        NULL = "Refused")
brfss22$income_midpoint <- as.numeric(levels(brfss22$income_midpoint))[brfss22$income_midpoint]

brfss22$Educ_number <- fct_recode(brfss22$EDUCA, 
                                  "0" = "Never attended school or only kindergarten", 
                                  "4.5" = "Grades 1 through 8 (Elementary)",
                                  "10" = "Grades 9 through 11 (Some high school)",
                                  "12" = "Grade 12 or GED (High school graduate)",
                                  "14" = "College 1 year to 3 years (Some college or technical school)",
                                  "16" = "College 4 years or more (College graduate)",
                                  NULL = "Refused" )
brfss22$Educ_number <- as.numeric(levels(brfss22$Educ_number))[brfss22$Educ_number]

levelsmarijaneace <- "dont know not sure"

select1 <- !is.na(brfss22$MARIJAN1)
brfss_marijan <- subset(brfss22, select1 )


p_cannabis_age <- ggplot(data = brfss_marijan,
                       mapping = aes(x = Age_midpt,
                                     y = MARIJAN1))
p_cannabis_age + geom_smooth()
```

```{r}
model_1 <- lm(MARIJAN1 ~ Age_midpt, data = brfss_marijan)
summary(model_1)
```


```{r}
model_2 <- lm(MARIJAN1 ~ Age_midpt + X_PRACE2 + X_HISPANC + EDUCA, data = brfss_marijan)
summary(model_2)

# maybe get fancy
require(stargazer)

stargazer(model_2, type = "??")
```








Mental Health and State


```{r}
model_3 <- lm(MARIJAN1 ~ ADDEPEV3, data = brfss_marijan)
summary(model_3)
```


```{r}
model_3.1 <- lm(MARIJAN1 ~ SDHSTRE1, data = brfss_marijan)
summary(model_3.1)
```


```{r}
model_4 <- lm(MARIJAN1 ~ Age_midpt + ADDEPEV3 + SDHSTRE1 + X_STATE, data = brfss_marijan)
summary(model_4)
```
The findings suggest that besides age, factors such as mental health, and state of residence also play a role in predicting cannabis use patterns. In this case, most states and stress levels show a negative or lower significance in cannabis use.



- Health 

```{r}
model_5 <- lm(MARIJAN1 ~ PRIMINSR, data = brfss_marijan)
summary(model_5)
```

```{r}
model_6 <- lm(MARIJAN1 ~ SLEPTIM1 + GENHLTH, data = brfss_marijan)
summary(model_6)
```
Certain types of health insurance coverage are associated with cannabis use. Sleep time and self-reported general health status (Good, Fair, Poor) are also associated with cannabis use.





# what additional information could you include in the regression, that would help predict cannabis use? 
-mental health: Mental health issues are often correlated with substance use, including cannabis.
-Education: Higher education levels  may influence attitudes, knowledge about substance use effects, and decision-making regarding cannabis use.

# What subgroups have different relationships?
Age: Different age groups may have varying relationships with cannabis use, with younger individuals often showing higher rates of use.

# Are there sets of variables that really increase the R-squared (so really help predict)? 
- age
-health

# Are there other variables that don’t help (even if we think they should)? Think about which results confirm what is known versus which results are surprising
- State: due to cannabis legalization status across states, the result show a negative relationship.
- Health insurance:  health insurance type may not have a strong association with cannabis use at the individual, but the results shows some relation which are surprising.
