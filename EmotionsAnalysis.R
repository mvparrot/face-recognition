##Emotions Analysis

library(tidyverse)
library(readr)
EmoM <-read_csv("data/EmotionsMerged.csv")
ME <- read_csv("data/MicrosoftEmotions.csv")
SB<-read_csv("data/SkybiometryEmotions.csv")
GE<- read_csv("SoftwareRequestScripts/GoogleClassifiedFaces.csv")
ALLmetaIMGnamed <- read.csv("ALLmetaIMGnamed.csv")

ALLmetaIMGnamedFaces<-ALLmetaIMGnamed%>%
  filter(matchesManual)%>%
  dplyr::select(-ID, -facecounter)%>%
  mutate(fileID = as.numeric(factor(file))) %>%
  mutate(FaceKey=paste(fileID, boxID, sep="-")) %>%
  mutate(FaceID=paste(fileID, boxID, substring(type, 1,2), sep="-")) %>%
  mutate(FileName=paste("face-", FaceID, ".png", sep=""))


EmotionsMerged<-left_join(by="FileName", MicrosoftEmotions, ALLmetaIMGnamedFaces)
#EmotionsMerged<-write.csv(EmotionsMerged, "EmotionsMerged.csv", row.names = FALSE)


EmotionsMerged<-EmotionsMerged %>% mutate(s = sum(anger, contempt, disgust, fear, happiness, neutral, sadness, surprise, na.rm=TRUE))

# Connect face id to original Google set

GE <- GE %>% 
  rowwise() %>%
  mutate(minX = min(boundingPoly.x1, boundingPoly.x4), maxX = min(boundingPoly.x2, boundingPoly.x3),
         minY = min(boundingPoly.y1, boundingPoly.y2), maxY = min(boundingPoly.y3, boundingPoly.y4)) %>%
  ungroup() %>%
  mutate(minX = ifelse(is.na(minX), 0, minX), minY = ifelse(is.na(minY), 0, minY))

# create full Google DataSet
GF1<-GoogleFaces[,c(1, 36, 18:21)]
GE1<-GE[,c(1, 10:13,135:138)]
GE <- left_join(GF1, GE1, by = c("file","minX","maxX","minY","maxY")) 




#consider just players?
#EmotionsMerged<-EmotionsMerged %>% filter(detect=="Player")


#Convert to true or false emotion values

ME <- ME %>% mutate(HappinessTF = ifelse(happiness > 0.5,TRUE,FALSE)) %>%
             mutate(SadnessTF = ifelse(sadness > 0.5,TRUE,FALSE))%>%
             mutate(AngerTF = ifelse(anger > 0.5,TRUE,FALSE))%>%
             mutate(FearTF = ifelse(fear > 0.5,TRUE,FALSE))%>%
             mutate(SurpriseTF = ifelse(surprise > 0.5,TRUE,FALSE))


GE <- GE %>%
  mutate(JoyTF = ifelse((joyLikelihood=="LIKELY"|joyLikelihood=="VERY_LIKELY"), TRUE,FALSE))

GE <- GE %>% filter(!duplicated(GE$FileName))



lapply(X = ME[,10:14], table)
table(ME$SadnessTF)

table(SB$happiness.value)
table(ME$HappinessTF, GE$JoyTF)

table(SB$anger.value)


#sets of players with similar facial expressions

set1<-EmotionsMerged[24:29,]
set2<-EmotionsMerged[29:34,]
set3<-EmotionsMerged[c(238,240:244),]

#17 to 22 was opposing player
library(GGally)
p1 <- ggparcoord(data = set1, columns = 2:9, groupColumn="file", order = "allClass")

p2 <- ggparcoord(data = set2, columns = 2:9, groupColumn="file", order = "allClass")

p3 <- ggparcoord(data = set3, columns = 2:9, groupColumn="file", order = "allClass")


overlayGgplot("2016_CT6_R02_FLopez_ESP_vs_GPella_ARG_MS227_clip.0022.png")




#Create normalised SB values
sumf <- function(vec) sum(vec)

SB<-SB %>%
  rowwise %>%
  mutate(z = sumf(c(anger.confidence, disgust.confidence, fear.confidence, happiness.confidence, neutral_mood.confidence, sadness.confidence, surprise.confidence, na.rm=T)))

SB<-SB %>%
  mutate(anger = anger.confidence/z,
         disgust = disgust.confidence/z,
         fear = fear.confidence/z,
         happiness = happiness.confidence/z,
         neutral = neutral_mood.confidence/z,
         sadness = sadness.confidence/z,
         surprise = surprise.confidence/z)








# Emotion outputs 

GE <- GE[,c(2, 7:10)]


lapply(GEmotions[,-1], table)

SBEmotions <- SB[,c(21:27)]

ggplot(SBEmotions, aes(x=neutral))+geom_histogram(bins=15)
ggplot(SBEmotions, aes(x=anger))+geom_histogram(bins=15)
ggplot(SBEmotions, aes(x=disgust))+geom_histogram(bins=15)
ggplot(SBEmotions, aes(x=fear))+geom_histogram(bins=15)
ggplot(SBEmotions, aes(x=happiness))+geom_histogram(bins=15)
ggplot(SBEmotions, aes(x=sadness))+geom_histogram(bins=15)
ggplot(SBEmotions, aes(x=surprise))+geom_histogram(bins=15)

MEmotions <- ME[,2:9]

ggplot(MEmotions, aes(x=neutral))+geom_histogram(bins=15)
ggplot(MEmotions, aes(x=anger))+geom_histogram(bins=15)
ggplot(MEmotions, aes(x=disgust))+geom_histogram(bins=15)
ggplot(MEmotions, aes(x=fear))+geom_histogram(bins=15)
ggplot(MEmotions, aes(x=happiness))+geom_histogram(bins=15)
ggplot(MEmotions, aes(x=sadness))+geom_histogram(bins=15)
ggplot(MEmotions, aes(x=surprise))+geom_histogram(bins=15)

nrow(SBEmotions)
#1095
nrow(MEmotions)
#1319

