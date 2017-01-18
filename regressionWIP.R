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
  map(~ select(., -type, -file, -boxID)) %>%
  map(~ glm(hit ~ ., data=.))

glmSummary <- glmFits %>%
  map(~ cbind(rownames_to_column(cbind(as.data.frame(coef(summary(.))))))) %>%
  map(~ rename(., variable = rowname))

glmPlot <- do.call(rbind, Map(cbind, glmSummary, type = names(glmSummary)))

## Coefficient of variables by software
glmPlot %>%
  ggplot(aes(x=type, y=Estimate)) +
  geom_col() + 
  facet_wrap(~ variable)

## Significance of variables by software
glmPlot %>%
  mutate(significant = `Pr(>|t|)` < 0.05) %>%
  ggplot(aes(x=type, y=`Pr(>|t|)`)) +
  geom_col(aes(fill=significant)) + 
  facet_wrap(~ variable)
