# Prepare data
library(tidyverse)
library(gridExtra)


ALLmetaIMG<-read_csv("ALLmetaIMG.csv",
                     col_types = cols(type = col_factor(levels = c("Manual", 
                      "Animetrics", "Google", "Microsoft", "Skybiometry"))))


ALLmetaIMG$graphic<-factor(ALLmetaIMG$graphic, levels = 0:1, labels = c("Live image", "Graphic"))
ALLmetaIMG$bg<-factor(ALLmetaIMG$bg, levels = 0:3, labels = c("Crowd", "Court", "Logo wall", "Not applicable"))
ALLmetaIMG$person<-factor(ALLmetaIMG$person, levels = 0:1, labels = c("No Person", "Person"))
ALLmetaIMG$shotangle<-factor(ALLmetaIMG$shotangle, levels = 0:2, labels = c("Player Shoulder Height", "Birds Eye", "Upward Angle"))
ALLmetaIMG$situation<-factor(ALLmetaIMG$situation, levels = 0:5, labels = c("Court in play", "Court player close-up", "Court close-up not player", "Crowd", "Off court close up of player", "Transition"))

ALLmetaIMG$detect<-factor(ALLmetaIMG$detect, levels = 0:3, labels = c("Player", "Other staff on court", "Fan", "None"))
ALLmetaIMG$obscured<-factor(ALLmetaIMG$obscured, levels = 0:1, labels = c("No", "Yes"))
ALLmetaIMG$lighting<-factor(ALLmetaIMG$lighting, levels = 0:2, labels = c("Direct sunlight", "Shaded", "Partially shaded"))
ALLmetaIMG$headangle<-factor(ALLmetaIMG$headangle, levels = 0:3, labels = c("Front on", "Back of head", "Profile", "Other"))
ALLmetaIMG$glasses<-factor(ALLmetaIMG$glasses, levels = 0:1, labels = c("No", "Yes"))
ALLmetaIMG$visorhat<-factor(ALLmetaIMG$visorhat, levels = 0:1, labels = c("No", "Yes"))

ALLmetaIMG$headangle <- as.character(ALLmetaIMG$headangle)
ALLmetaIMG$headangle[ALLmetaIMG$headangle == "Back of head"] <- "Other"
ALLmetaIMG$headangle <- as.factor(ALLmetaIMG$headangle)

ALLmetaIMGPlayers<- ALLmetaIMG %>% filter(detect=="Player")

# change this to facesVenn with binary information
ALLmetaIMGFaces<-ALLmetaIMG%>%
  filter(matchesManual) %>% 
  dplyr::select(-ID, -facecounter)%>%
  mutate(fileID = as.numeric(factor(file))) %>%
  mutate(FaceKey=paste(fileID, boxID, sep="-")) %>%
  mutate(FaceID=paste(fileID, boxID, substring(type, 1,2), sep="-"))





hitmiss <- function(x){
  allType <- c("Animetrics", "Google", "Microsoft", "Skybiometry")
  hit <- allType %in% x$type
  x[1,] %>%
    dplyr::select(file:visorhat) %>%
    cbind(type = allType, hit = hit)
}

#glmFits <- ALLmetaIMGFaces %>% 
#  split(.$FaceKey) %>% 
#  map_df(~ hitmiss(.)) %>% 
#  split(.$type) %>% 
#  map(~ glm(hit ~ . + visorhat*glasses, data = select(., -type, -file, -boxID), binomial(link = "logit"))) 
#
#glmSummary <- glmFits %>% 
#  map(~ rename(cbind(rownames_to_column(cbind(as.data.frame(coef(summary(.)))))), variable = rowname)) 
#
#glmPlot <- do.call(rbind, Map(cbind, glmSummary, type = names(glmSummary))) 


# Graphs
## Coefficient of variables by software
#glmPlot %>%
#  ggplot(aes(x=type, y=Estimate)) +
#  geom_col() + 
#  facet_wrap(~ variable)

## Significance of variables by software
#glmPlot %>%
#  mutate(significant = `Pr(>|z|)` < 0.05) %>%
#  mutate(`Pr(<|z|)` = 1 - `Pr(>|z|)`) %>%
#  ggplot(aes(x=type, y=`Pr(<|z|)`)) +  
#  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
#  geom_col(aes(fill=significant)) + 
#  facet_wrap(~ variable)

## Significance of Estimated variable influences by software
#glmPlot %>%
#  mutate(significant = `Pr(>|z|)` < 0.05) %>%
#  ggplot(aes(x=type, y=Estimate)) +  
#  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
#  geom_col(aes(fill=significant)) + 
#  facet_wrap(~ variable, scales = "free_y")
#
# summary(ALLmetaIMGFaces)
#View(glmPlot)

#################
#WIP attempt to include type in model
#glmFits <- ALLmetaIMGFaces %>%
#  split(.$FaceKey) %>%
#  map_df(~ hitmiss(.)) %>% 
#  #split(.$type) %>%
#  map(~ glm(hit ~ 0 + type*., data = select(., -file), binomial(link = "logit")))
 


#### 
# Create new data structure to model from

ALLmetaIMGFacesWide<-ALLmetaIMGFaces %>%
  split(.$FaceKey) %>%
  map_df(~ hitmiss(.)) %>%
  spread(type, hit)

ALLmetaIMGFacesWide$Animetrics<-as.numeric(ALLmetaIMGFacesWide$Animetrics)
ALLmetaIMGFacesWide$Google<-as.numeric(ALLmetaIMGFacesWide$Google)
ALLmetaIMGFacesWide$Skybiometry<-as.numeric(ALLmetaIMGFacesWide$Skybiometry)
ALLmetaIMGFacesWide$Microsoft<-as.numeric(ALLmetaIMGFacesWide$Microsoft)



GlmModelCreation <- function(model, data = ALLmetaIMGFaces) {
  glmFits <- data %>% 
  split(.$FaceKey) %>% 
  map_df(~ hitmiss(.)) %>% 
  split(.$type) %>% 
  map(~ glm(model, data = select(., -type, -file, -boxID), binomial(link = "logit")))
}

ConvertModel2Table <- function(model){
  model %>%
    summary %>%
    coef %>%
    as.data.frame %>%
    cbind %>%
    rownames_to_column %>%
    cbind %>%
    dplyr::rename(variable = rowname)
}

GlmModelEstimates <- function(model, data = GlmModelCreation(model)){
  glmSummary <- data %>% 
    map(~ ConvertModel2Table(.)) 
  
  glmPlot <- do.call(rbind, Map(cbind, glmSummary, type = names(glmSummary))) 
  return(glmPlot)
}

SignificancePlot <- function(model, data = GlmModelEstimates(model)) {
  data %>%
  mutate(significant = `Pr(>|z|)` < 0.05) %>%
  mutate(`Pr(<|z|)` = 1 - `Pr(>|z|)`) %>%
  ggplot(aes(x=type, y=`Pr(<|z|)`)) +  
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_col(aes(fill=significant)) + 
  facet_wrap(~ variable)
}

EstimatesPlot <- function(model, data = GlmModelEstimates(model)) {
  data %>%
  mutate(significant = `Pr(>|z|)` < 0.05) %>%
  ggplot(aes(x=type, y=Estimate)) +  
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_col(aes(fill=significant)) + 
  facet_wrap(~ variable, scales = "free_y")
}



ModelPlotResults<-function(model, data = GlmModelEstimates(model)){
  ep<-EstimatesPlot(model, data)
  sp<- SignificancePlot(model, data)
  grid.arrange(ep, sp)
}


ModelPlotResults(hit ~ glasses + visorhat + bg + visorhat*glasses)

