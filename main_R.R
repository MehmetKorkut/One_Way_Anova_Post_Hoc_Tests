---
title: "Normality, 1 Way Anova and Post Hoc Tests"
author: "Mehmet Korkut"
date: "2022-08-18"
output:
  html_document:
    df_print: paged
  word_document: default
  pdf_document: default
---
```{r setup}

load(file = "objects.RData")

```


```{r packages}
library("dplyr")
library("ggpubr")
library(ggplot2)
library(car)
library("dgof")
library(broom)
library("stargazer")
library("apaTables")
library(tidyverse)
library(ggpubr)
library(rstatix)
library(vtable)
library(psych)
```

```{r loading objects}
save.image(file = "my_work_space.RData")
load("my_work_space.RData")
```


## APA Corroletion Table

Table 1

Means, standard deviations, and correlations with confidence intervals

Variable M SD 1 2 3\
1. Temperature ᵒc1 23.45 10.55

2.  Temperature ᵒc2 22.69 10.25 .94\*\*\
    [.94, .95]

3.  Temperature ᵒc3 22.58 10.20 .92\*\* .90\*\*\
    [.91, .92] [.89, .90]

4.  Temperature ᵒc4 23.28 11.99 .92\*\* .91\*\* .93\*\*\
    [.92, .93] [.91, .92] [.93, .94]

Note. M and SD are used to represent mean and standard deviation, respectively. Values in square brackets indicate the 95% confidence interval. The confidence interval is a plausible range of population correlations that could have caused the sample correlation (Cumming, 2014). \* indicates p \< .05. \*\* indicates p \< .01.

## Boxplots

```{r boxplotss}
boxplot(Temp~ Group,
        data = df2,
        main = "Temperature",
        xlab = "Groups",
        ylab = "Value",
        col = "white")
```

## Getting Rid Of Outliers

```{r outlier,include=TRUE}
##Getting rid of outliers
quartiles <- quantile(data_full_embeded$`Temperature ᵒc1`, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data_full_embeded$`Temperature ᵒc1`)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

df_new <- subset(data_full_embeded, data_full_embeded$`Temperature ᵒc1` > Lower & data_full_embeded$`Temperature ᵒc1` < Upper)

dim(df_new)
##################
quartiles <- quantile(data_full_embeded$`Temperature ᵒc2`, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data_full_embeded$`Temperature ᵒc2`)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

df_new <- subset(data_full_embeded, data_full_embeded$`Temperature ᵒc2` > Lower & data_full_embeded$`Temperature ᵒc2` < Upper)

dim(df_new)
###################
quartiles <- quantile(data_full_embeded$`Temperature ᵒc4`, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data_full_embeded$`Temperature ᵒc4`)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

df_new <- subset(data_full_embeded, data_full_embeded$`Temperature ᵒc4` > Lower & data_full_embeded$`Temperature ᵒc4` < Upper)

dim(df_new)


```

## Normalization

```{r sqrt}
standardized_df <- as.data.frame(scale(df2[2]))
standardized_df$Group <- df2$Group
```

## QQ Plot

```{r qqplot }
ggqqplot(df_last, x= "Temp", facet.by = "Group")
```

## Density Plots for Checking Normality

```{r density, include=TRUE}
ggdensity(scale_data, x = "Temperature ᵒc1", fill = "lightgray", title = "",xlab="Ground Cover") + stat_overlay_normal_density(color = "black", linetype = "dashed")

ggdensity(scale_data, x = "Temperature ᵒc2", fill = "lightgray", title = "",xlab="green roof with mixed vegetation")+ stat_overlay_normal_density(color = "black", linetype = "dashed")

ggdensity(scale_data, x = "Temperature ᵒc3", fill = "lightgray", title = "",xlab="Bush") + stat_overlay_normal_density(color = "black", linetype = "dashed")

ggdensity(scale_data, x = "Temperature ᵒc4", fill = "lightgray", title = "",xlab="Empty Hut") + stat_overlay_normal_density(color = "black", linetype = "dashed")
```

# One Way Anova

## Homogenity of Variance

Homogenity of variance assumption is failed. Therefore, we need to use alternate to One Way Anova which is Welch One way ANOVA test with pairwise t test comparison or Games-Howell post hoc test.

```{r}
model  <- lm(Temp ~ Group, data = df_last)
plot(model, 1)
```

### Levenes Test for Homogeneity-FAILED

```{r levene}
df_last %>% levene_test(Temp~ Group)
```

```{r barlett}
bartlett.test(Temp ~ Group, data = df_last)
```

## Welch One Way Anova

```{r welch one way anova}
# Welch One way ANOVA test
res.aov2 <- standardized_df %>% welch_anova_test(Temp ~ Group)
# Pairwise comparisons (Games-Howell)
pwc2 <- standardized_df  %>% games_howell_test(Temp ~ Group)
# Visualization: box plots with p-values
pwc2 <- pwc2 %>% add_xy_position(x = "Group", step.increase = 1)
ggboxplot(df_last, x = "Group", y = "Temp") +
  stat_pvalue_manual(pwc2, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov2, detailed = TRUE),
    caption = get_pwc_label(pwc2)
    )
```

### Interpretation of Welch Anova and Games Howell Pairwise Comparisons

Since p value is less than α = .05, we reject the null hypothesis which is all group means are equal. In other words,temperatures are equal between the four different types of roofs. According to the graph above, three different roof types are different compared to empty hut (control group).

## Pairwise T Test

```{r pairwise t test}
pwc3 <- standardized_df %>% 
  pairwise_t_test(
    Temp ~ Group, pool.sd = FALSE,
    p.adjust.method = "bonferroni"
    )
pwc3
```

Note : Time Series Graphs on Tableau will also be added to the report in order to show the difference between control and experimental groups.

## Summary

-   Anova Assumptions *he responses for each factor level have a normal population distribution*

-   These distributions have the same variance

-   The data are independent.

1.  data is not normally distributed but close

2.  Getting rid of outliers

3.  Square root transformation

4.  Making data normally distributed (so close)--first assumption is met

5.  Checking same variance for groups, with levene test--failed to met the variance assumption

6.  Alternative ANOVA method is Welch One Way Anova with Games Howell pairwise comparison

7.  I also added pairwise t test.

### Walch One Way Anova Results

Our sample evidence provides sufficient evidence to conclude that the means of all groups are not equal in the population.

## The Games-Howell Post Hoc Test Results

According to the pairwise group test--Games Howell Post Hoc Test Results, the mean difference between bush roof and ground cover,empty hut and mixed vegetation is statistically significant. Bush roof performs better job for cooling compared to empty roof, ground cover and mixed vegetation roof. In addition, the mean difference between ground cover and mixed vegetation is also statistically significant. This means that ground cover performs a slightly better job than mixed vegetation roof. However, there is no statistically significant evidence to support mean difference between empty roof and ground cover, mixed vegetation.

```{r pwc}
kruskal.test(Temp ~ Group, data = df2)
pairwise.wilcox.test(standardized_df$Temp,standardized_df$Group,
                 p.adjust.method = "BH")
```

Means below ;

```{r mean}
df2 %>%
  group_by(Group) %>%
  summarise_at(vars(Temp), list(name = mean),na.rm=TRUE)
```
