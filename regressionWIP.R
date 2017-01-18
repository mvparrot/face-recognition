# Prepare data
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
  map(~ glm(hit ~ ., data = select(., -type, -file, -boxID)))

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
  mutate(significant = `Pr(>|t|)` < 0.05) %>%
  ggplot(aes(x=type, y=`Pr(>|t|)`)) +
  geom_col(aes(fill=significant)) + 
  facet_wrap(~ variable)
