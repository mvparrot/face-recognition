# Prepare data
library(purrr)
library(dplyr)
library(ggplot2)
hitmiss <- function(x){
  allType <- c("Animetrics", "Google", "Microsoft", "Skybiometry")
  hit <- allType %in% x$type
  x[1,] %>%
    dplyr::select(file:visorhat) %>%
    cbind(type = allType, hit = hit)
}

glmFits <- ALLmetaIMGFaces %>%
  split(.$FaceKey) %>%
  map_df(~ hitmiss(.)) %>%
  split(.$type) %>%
  map(~ glm(hit ~ 0 + situation + . + visorhat*glasses, data = select(., -type, -file), binomial(link = "logit")))

glmSummary <- glmFits %>%
  map(~ rename(cbind(rownames_to_column(cbind(as.data.frame(coef(summary(.)))))), variable = rowname))

glmPlot <- do.call(rbind, Map(cbind, glmSummary, type = names(glmSummary)))

# Graphs
## Coefficient of variables by software
glmPlot %>%
  ggplot(aes(x=type, y=Estimate)) +
  geom_col() + 
  facet_wrap(~ variable)

## Significance of variables by software
glmPlot %>%
  mutate(significant = `Pr(>|z|)` < 0.05) %>%
  mutate(`Pr(<|z|)` = 1 - `Pr(>|z|)`) %>%
  ggplot(aes(x=type, y=`Pr(<|z|)`)) +  
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_col(aes(fill=significant)) + 
  facet_wrap(~ variable)

## Significance of Estimated variable influences by software
glmPlot %>%
  mutate(significant = `Pr(>|z|)` < 0.05) %>%
  ggplot(aes(x=type, y=Estimate)) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_col(aes(fill=significant)) + 
  facet_wrap(~ variable, scales = "free_y")

summary(ALLmetaIMGFaces)


#################
#WIP attempt to include type in model
glmFits <- ALLmetaIMGFaces %>%
  split(.$FaceKey) %>%
  map_df(~ hitmiss(.)) %>% 
  #split(.$type) %>%
  map(~ glm(hit ~ 0 + type*., data = select(., -file), binomial(link = "logit")))


