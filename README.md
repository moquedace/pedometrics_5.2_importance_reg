---
Title: "Importance - Regression"
Authors: "Fernandes-Filho, Elpídio Inácio; Moquedace, Cássio Marques; Pereira, Luís Flávio; Veloso, Gustavo Vieira; Carvalho Junior, Waldir"
---

## Loading packages
```{r message=FALSE, warning=FALSE}
pkg <- c("dplyr", "ggplot2", "stringr", "tidyr", "RColorBrewer", "data.table")
sapply(pkg, require, character.only = T)
```

## Cleaning the environment (removing objects and previously loaded packages)
```{r message=FALSE, warning=FALSE}
rm(list = ls())  
gc()
```

## Loading importance data
```{r message=FALSE, warning=FALSE}
df_imp <- list.files(path = "./regression", pattern = ".csv$", full.names = T, recursive = T) %>% 
  grep(pattern = "imp_pred_TOTAL", value = T) %>% 
  read.csv2()
```

## Categorical predictors 
```{r message=FALSE, warning=FALSE}
varfact <- c("geology", "drainage", "landforms_tpi_based",
             "surface_specific_points", "lulc",
             "terrain_surface_classification_iwahashi", "soil", "slope_idx",
             "valley_idx") %>% sort()

for (h in seq_along(varfact)) {
  
  df_imp <- df_imp %>% 
    mutate(predictor = str_replace(predictor, paste0(".*", varfact[h], ".*"),
                                   varfact[h]))
  
}
```

## Calculating average importance of categorical variables
```{r message=FALSE, warning=FALSE}
df_imp <- df_imp %>% 
  group_by(rep, predictor) %>% 
  summarise(importance = mean(importance))
```

## Loading a dataframe with final name of variables and function for counting repetitions
```{r message=FALSE, warning=FALSE}
n_cova <- read.csv2("./data/name_cova.csv")

print(n_cova)

dfg_imp <- left_join(df_imp, n_cova, by = c("predictor" = "name_abr")) 

n_fun <- function(x) {
  return(data.frame(y = 3.5 * 30,
                    label = length(x)))
}
```

## Plotting boxplots
```{r message=FALSE, warning=FALSE, fig.width=10, fig.height=5}
imp_pred <- ggplot(dfg_imp, aes(x = importance,
                                y = reorder(name_comp, importance,
                                            decreasing = T))) +
  stat_boxplot(geom = "errorbar", size = 0.5) +
  geom_boxplot(color = "black", fill = "grey40") +
  scale_y_discrete(limits = rev) +
  labs(y = "Predictor", fill = "Depth (cm)",
       x = "Relative importance (%)", y = NULL) +
  scale_x_continuous(expand = c(0.01, 0, 0.05, 0)) +
  theme(axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, inherit.blank = T),
        panel.grid = element_line(colour = "grey90"),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15)) +
  stat_summary(fun.data = n_fun, geom = "text", col = "red", size = 4,
               hjust = 0.5, position = position_dodge(0.6),
               fontface = "bold") +
  stat_summary(fun = mean, geom = "point", col = "red", shape = 15, size = 1.5,
               position = position_dodge(0.75)) ; imp_pred
```

<p align="center">
<img src="reg_imp_full.jpg" width="800">
</p>
