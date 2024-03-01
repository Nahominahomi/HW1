---
title: "4__HW"
author: "Nahomi Farington"
date: "2024-02-29"
output: html_document
---

```{r}
library(ggplot2)
library(tidyverse)
```

```{r}
load("C:/Users/Owner/Desktop/Psychology/R-work/V0500_Lecture1/BRFSS2022/BRFSS2022_rev.RData")
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
```

```{r}
select_tristate <- (brfss22$X_STATE == "Ohio") | (brfss22$X_STATE == "Maryland") | (brfss22$X_STATE == "Texas")
brfss_tristate <- subset(brfss22,select_tristate)
```

As people get older there is higher possibility to an increment in BMI

```{r}
p_tri <- ggplot(data = brfss_tristate,
                mapping = aes(x = Age_midpt,
                              y = X_BMI5,
                              color = X_STATE,
                              fill = X_STATE))
p_tri + geom_smooth()
```

```{r}
p_tri <- ggplot(data = brfss_tristate,
                mapping = aes(x = Age_midpt,
                              y = X_BMI5))
p_tri + geom_smooth()
```

Interesting to see the decrease after the 50s.

```{r}
p_facet <- ggplot(data = brfss_tristate,
                  mapping = aes(x = Age_midpt,
                                y = X_BMI5,
                                color = X_STATE)) +
  geom_smooth() +
  facet_wrap(~X_STATE, scales = "free_y") +
  labs(title = "Relationship between Age and BMI in Ohio, Maryland, and Texas",
       x = "Age Midpoint",
       y = "BMI")

print(p_facet)

```


P-facet, let us see another perspective on how the population of each state changes over time.




```{r}
ACEdidntask <- (as.numeric(is.na(brfss22$ACEDEPRS)) + 
                        as.numeric(is.na(brfss22$ACEDRINK)) +
                        as.numeric(is.na(brfss22$ACEDRUGS)) +
                        as.numeric(is.na(brfss22$ACEPRISN)) +
                        as.numeric(is.na(brfss22$ACEDIVRC)) +
                        as.numeric(is.na(brfss22$ACEPUNCH)) +
                        as.numeric(is.na(brfss22$ACEHURT1)) +
                        as.numeric(is.na(brfss22$ACESWEAR)) +
                        as.numeric(is.na(brfss22$ACETOUCH)) )
select_ACE <- (ACEdidntask == 0) 
brfss_ACE <- subset(brfss22, select_ACE)
```

# Adverse Childhood Experience Related to Drinking abuse and Unwanted Touch

```{r}
brfss_ACE$ACEDRINK_recode <- fct_recode(
  brfss_ACE$ACEDRINK,
  "Yes" = "Yes, Adverse Childhood Exper, lived with someone who was a problem drinker or alcoholic",
  "No" = "No",
  "Don't know/Refused/Missing" = "dont know not sure" 
)


```

```{r}
summary(brfss_ACE$ACEDRINK_recode)

```

```{r}

brfss_ACE$ACETOUCH_recode <- fct_recode(brfss_ACE$ACETOUCH, 
                                        "Total" = "Adverse Childhood Exper, never:  How often did anyone at least 5 years older than you or an adult, ever touch you sexually","Once" = "once",
                                        "More than Once" = "more than once", 
                                        "NULL" = "dont know not sure",
                                        "NULL" = "refused")
```

```{r}
table(brfss_ACE$ACETOUCH_recode)
```

```{r}
xtabs(~ brfss_ACE$ACEDRINK + brfss_ACE$ACETOUCH)
```

```{r}
ftable(xtabs(~ brfss_ACE$ACEDRINK + brfss_ACE$ACETOUCH))

```

```{r}
df1 <- data.frame(
  ACE_TOUCH = c("Once", "More than once", "Don't know", "Refused"),
  ACE_DRINK = c(795, 1138, 7, 0, 1914, 1751, 10, 2, 51, 91, 57, 3, 203, 321, 8, 680))
```

```{r}
ggplot(df1, aes(x = ACE_TOUCH, y = ACE_DRINK, fill = ACE_TOUCH)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Adverse Childhood Experiences",
       x = "ACE related to Living with a Problem Drinker/Alcoholic",
       y = "Frequency",
       fill = "ACE related to Unwanted Sexual Contact") +
  theme_minimal()
```

A substantial number of respondents reported living with someone who was a problem drinker or alcoholic and experienced unwanted sexual contact during their childhood. A larger group experienced such contact more than once

# Adverse Childhood Experience Related to Drug abuse and Insults

```{r}
brfss_ACE$ACEDRUG_recode <- fct_recode(brfss_ACE$ACEDRUGS, 
                                        "Yes" = "Yes, Adverse Childhood Exper, lived with someone who used illegal street drugs or who abused prescription medications",
                                        "No" = "No", 
                                        "NULL" = "dont know not sure",
                                        "NULL" = "refused"
)

```

```{r}
table(brfss_ACE$ACEDRUG_recode)
```

```{r}
brfss_ACE$ACESWEAR_recode <- fct_recode(brfss_ACE$ACESWEAR, 
                                        "Total" = "Adverse Childhood Exper, never: How often did a parent or adult in your home ever swear at you, insult you, or put you down","Once" = "once",
                                        "More than Once" = "more than once", 
                                        "NULL" = "dont know not sure",
                                        "NULL" = "refused")

```

```{r}
table(brfss_ACE$ACESWEAR_recode)
```


```{r}
xtabs(~ brfss_ACE$ACEDRUGS + brfss_ACE$ACESWEAR)
```

```{r}
ftable(xtabs(~ brfss_ACE$ACEDRUGS + brfss_ACE$ACESWEAR))
```

```{r}
df2 <- data.frame(
  ACESWEAR = c("Once", "More than once", "Don't know", "Refused"),
  ACEDRUGS = c(189, 2188, 13, 2, 3099, 10464, 132, 15, 45, 458, 60, 7, 27, 352, 9, 675)
)
```

```{r}
ggplot(df2, aes(x = ACESWEAR, y = ACEDRUGS, fill = ACESWEAR)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Adverse Childhood Experiences",
       x = "ACE related to Drug Abuse",
       y = "Frequency",
       fill = "ACE related to Swearing/Insults") +
  theme_minimal()
```

A significant portion reported living with someone who used illegal street drugs or abused prescription medications during their childhood. A noticeable number of respondent reported experiencing being sworn at, insulted, or put down at least once during their childhood.A larger group reported experiencing such behavior more than once.
 
The findings suggest that a notable proportion of the population has had adverse childhood experiences related to drug use and swearing/insults.