#Load Packages
library(imager)
library(dplyr)
library(tidyr)
library(purrr)



#Load Data
MicrosoftClassifiedFaces <- read_csv("data/ClassifiedFaces/MicrosoftClassifiedFaces.csv")
AnimetricsClassifiedFaces <- read_csv("data/ClassifiedFaces/AnimetricsClassifiedFaces.csv")
SkybiometryClassifiedFaces <- read_csv("data/ClassifiedFaces/SkybiometryClassifiedFaces.csv")
GoogleClassifiedFaces <- read_csv("data/ClassifiedFaces/GoogleClassifiedFaces.csv")
ManualClassifiedFaces <- read_csv("data/ManualClassifiedFaces.csv")
ManualClassifiedScenes <- read_csv("data/ManualClassifiedScenes.csv")

#Check all equal length
#length(unique(ManualClassifiedScenes$file))
#length(unique(AnimetricsClassifiedFaces$file))
#length(unique(SkybiometryClassifiedFaces$file))
#length(unique(GoogleClassifiedFaces$file))

#Remove images with no faces
MicrosoftClassifiedFaces <- MicrosoftClassifiedFaces[!is.na(MicrosoftClassifiedFaces$faceRectangle.top),]
AnimetricsClassifiedFaces <- AnimetricsClassifiedFaces[!is.na(AnimetricsClassifiedFaces$topLeftX),]
SkybiometryClassifiedFaces <- SkybiometryClassifiedFaces[!is.na(SkybiometryClassifiedFaces$width),]
GoogleClassifiedFaces <- GoogleClassifiedFaces[!is.na(GoogleClassifiedFaces$rollAngle),]

#Manual Classified Faces
ManualClassifiedFaces <- ManualClassifiedFaces %>%
  dplyr::mutate(type = "Manual", time.user.self=NA, time.sys.self=NA, time.elapsed=NA, ID=1:NROW(ManualClassifiedFaces))

#Merge data
MicrosoftMerge <- MicrosoftClassifiedFaces %>% 
  dplyr::mutate(type = "Microsoft", ID=NA,
         minX = faceRectangle.left, maxX = faceRectangle.left + faceRectangle.width,
         minY = faceRectangle.top, maxY = faceRectangle.top + faceRectangle.height) %>%
  dplyr::select(file, type, ID, time.user.self, time.sys.self, time.elapsed, minX, maxX, minY, maxY)  
AnimetricsMerge <- AnimetricsClassifiedFaces %>%
  dplyr::mutate(type = "Animetrics",  ID=NA,
         minX = topLeftX, maxX = topLeftX + width,
         minY = topLeftY, maxY = topLeftY + height) %>%
  dplyr::select(file, type, ID, time.user.self, time.sys.self, time.elapsed, minX, maxX, minY, maxY)  
SkyBiometryMerge <- SkybiometryClassifiedFaces %>% 
  dplyr::mutate(type = "Skybiometry", ID=NA,
         minX = (center.x - width/2)*8, maxX = (center.x + width/2)*8,
         minY = (center.y - height/2)*4.5, maxY = (center.y + height/2)*4.5) %>%
  dplyr::select(file, type, ID, time.user.self, time.sys.self, time.elapsed, minX, maxX, minY, maxY)  
GoogleMerge <- GoogleClassifiedFaces %>% rowwise() %>%
  dplyr::mutate(type = "Google", ID=NA,
         minX = min(boundingPoly.x1, boundingPoly.x4), maxX = min(boundingPoly.x2, boundingPoly.x3),
         minY = min(boundingPoly.y1, boundingPoly.y2), maxY = min(boundingPoly.y3, boundingPoly.y4)) %>%
  ungroup() %>%
  dplyr::mutate(minX = ifelse(is.na(minX), 0, minX), minY = ifelse(is.na(minY), 0, minY)) %>%
  dplyr::select(file, type, ID, time.user.self, time.sys.self, time.elapsed, minX, maxX, minY, maxY) 
ManualMerge <- ManualClassifiedFaces %>%
  dplyr::mutate(type = "Manual") %>%
  dplyr::rename(minX = xmin, maxX = xmax,
         minY = ymin, maxY = ymax) %>%
  dplyr::select(file, type, ID, time.user.self, time.sys.self, time.elapsed, minX, maxX, minY, maxY)#%>% filter(file=="2016_RLA_R01_JDuckworth_AUS_vs_LHewitt_AUS_MS150_clip.0056.png")  
mergedData <- rbind(MicrosoftMerge, AnimetricsMerge, SkyBiometryMerge, GoogleMerge, ManualMerge)
mergedData$type <- factor(mergedData$type)
mergedData$file <- factor(as.character(mergedData$file))

#used within boxOverlap (helper function)
prepareFaceBox <- function(data){
  boxX <- c(data$minX, data$minX, data$maxX, data$maxX, data$minX)
  boxY <- c(data$maxY, data$minY, data$minY, data$maxY, data$maxY)
  data.frame(x = boxX, y = boxY)
}

#Finds boxes of same face, creates an ID for each face within an image
boxOverlap <- function(boxes){
  require(raster)
  boxes$boxID <- 1:NROW(boxes)
  message(boxes$file[1])
  if(NROW(boxes)>1){
    for (newBox in 1:(NROW(boxes) - 1)){
      for (compareBox in (newBox + 1):NROW(boxes)){
        if(boxes[newBox,"boxID"] != boxes[compareBox,"boxID"]){
          newPoly <- SpatialPolygons(list(Polygons(list(Polygon(prepareFaceBox(boxes[newBox,]))), 1)))
          comparePoly <- SpatialPolygons(list(Polygons(list(Polygon(prepareFaceBox(boxes[compareBox,]))), 1)))
          intersectPoly <- suppressWarnings(intersect(newPoly, comparePoly))
          if(!is.null(intersectPoly)){ #If they actually intersect
            intersectArea <- area(intersectPoly)
            totalArea <- area(newPoly) + area(comparePoly) - intersectArea
            if(intersectArea/totalArea > 1/3){
              boxes[compareBox,"boxID"] <- boxes[newBox,"boxID"]
            }
          }
        }
      }
    }
  }
  return(boxes)
}




#Match faces VERY IMPORTANT
mergedFaceMatches <- mergedData %>%
  split(.$file) %>%
  map_df(~ boxOverlap(.x))

#Does it match manual
mergedFaceMatches <- mergedFaceMatches %>% group_by(file, boxID) %>%
  dplyr::mutate(matchesManual = ifelse(any(type == "Manual"), TRUE, FALSE))
  

#size of box
classifiedIMG <- mergedFaceMatches %>% dplyr::mutate(size = (maxX - minX) * (maxY - minY))


### MERGE ALL THE DATA!

metaManualClassifiedFaces <- merge(dplyr::select(ManualClassifiedFaces, file, facecounter, 
                                                 detect, obscured, lighting, headangle, glasses, visorhat, ID),
                                   dplyr::select(dplyr::ungroup(dplyr::filter(mergedFaceMatches, type=="Manual")), ID, boxID), by=c("ID"))



## New method to retain scene information on non-matches
## Pairwise merge, always include file, change merge order


metaFaces <- full_join(metaManualClassifiedFaces, classifiedIMG, by=c("file", "boxID")) %>% 
  dplyr::select(-ID.y, ID=ID.x)

## THIS CAUSES boxID NAs
ALLmetaIMG <- merge(ManualClassifiedScenes, metaFaces, by="file", all=TRUE)

#Extract file name info
fileSplit <- strsplit(unique(as.character(ALLmetaIMG$file)), "_")
fileSplit <- as.data.frame(do.call(rbind, fileSplit))
colnames(fileSplit) <- c("year", "court", "round", "player1", "country1", "junk1", "player2", "country2", "matchDraw", "junk2")
fileInfo <- cbind(file = unique(as.character(ALLmetaIMG$file)), fileSplit[,-c(6, 10)])
ALLmetaIMG <- merge(ALLmetaIMG, fileInfo, by="file")

#Finding duplicates
ALLmetaIMG <- ALLmetaIMG %>% group_by(file, boxID) %>%
  dplyr::mutate(duplicates = duplicated(type))


write.csv(ALLmetaIMG,file = "ALLmetaIMG.csv", row.names = FALSE)

library(readr)
# creating named
ALLmetaIMG<-read_csv("data/ALLmetaIMG.csv", col_types = cols(type = col_factor(levels = c("Manual", "Animetrics", "Google", "Microsoft", "Skybiometry"))))


#Factor Names 
ALLmetaIMG$graphic<-factor(ALLmetaIMG$graphic, levels = 0:1, labels = c("Live image", "Graphic"))
ALLmetaIMG$person<-factor(ALLmetaIMG$person, levels = 0:1, labels = c("No Person", "Person"))
ALLmetaIMG$situation<-factor(ALLmetaIMG$situation, levels = 0:5, labels = c("Court in play", "Court player close-up", "Court close-up not player", "Crowd", "Off court close up of player", "Transition"))
ALLmetaIMG$bg<-factor(ALLmetaIMG$bg, levels = 0:3, labels = c("Crowd", "Court", "Logo wall", "Not applicable"))
ALLmetaIMG$shotangle<-factor(ALLmetaIMG$shotangle, levels = 0:2, labels = c("Player Shoulder Height", "Birds Eye", "Upward Angle"))

ALLmetaIMG$detect<-factor(ALLmetaIMG$detect, levels = 0:2, labels = c("Player", "Other staff on court", "Fan"))
ALLmetaIMG$obscured<-factor(ALLmetaIMG$obscured, levels = 0:1, labels = c("No", "Yes"))
ALLmetaIMG$lighting<-factor(ALLmetaIMG$lighting, levels = 1:2, labels = c("Shaded", "Partially shaded"))
ALLmetaIMG$headangle<-factor(ALLmetaIMG$headangle, levels = 0:3, labels = c("Front on", "Back of head", "Profile", "Other"))
ALLmetaIMG$glasses<-factor(ALLmetaIMG$glasses, levels = 0:1, labels = c("No", "Yes"))
ALLmetaIMG$visorhat<-factor(ALLmetaIMG$visorhat, levels = 0:1, labels = c("No", "Yes"))

ALLmetaIMG$headangle <- as.character(ALLmetaIMG$headangle)
ALLmetaIMG$headangle[ALLmetaIMG$headangle == "Back of head"] <- "Other"
ALLmetaIMG$headangle <- as.factor(ALLmetaIMG$headangle)

write.csv(ALLmetaIMG, "ALLmetaIMGnamed.csv", row.names = FALSE)

#check for NAs in variables

ALLmetaIMGPlayers <- ALLmetaIMG %>% 
  #filter(detect==0) %>%
  dplyr::select(graphic,person,situation,bg, shotangle,visorhat,
                detect, obscured, lighting, headangle, glasses, visorhat)

summary(ALLmetaIMGPlayers)
