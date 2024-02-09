---
title: "HW 2_Nahomi Farington"
output: html_document
date: "2024-02-07"
---

```{r}
load("C:/Users/Owner/Desktop/Psychology/R-work/V0500_Lecture1/BRFSS2022/BRFSS2022_rev.RData")
```

```{r}
summary(brfss22)
```

```{r}
library(tidyverse)
```

```{r}
NN <- length(brfss22$ADDEPEV3)
set.seed(12345)
restrict_1 <- (runif(NN) < 0.1)
summary(restrict_1)
```

```{r}
brfss_small <- subset(brfss22, restrict_1)
```

```{r}
ggplot(brfss_small, aes(x = INCOME3, fill = ADDEPEV3)) +
  geom_bar(position = "dodge") +  # Use position = "dodge" instead of "fill"
  scale_fill_viridis_d() +
  labs(title = "Depression Diagnosis by Income Level", x = "Income Level", y = "Proportion")

```


```{r}
ggplot(brfss_small, aes(x = EDUCA, fill = EMPLOY1)) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d() +
  labs(title = "Depression Diagnosis by Education and Employment", x = "Education Level", y = "Proportion of Employment")
```

```{r}
cross_tab <- table(brfss_small$EDUCA, brfss_small$EMPLOY1)
print(cross_tab)

```

```{r}
## Investigating how depression is linked to things like income, education, and job status is interesting because it helps us see how mental health connects with society. By figuring out how these factors affect depression, we can create ways to help and make mental well-being better for everyone.
```


