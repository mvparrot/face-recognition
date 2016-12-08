setwd("~/github/face-recognition/")

#Load Packages
library(imager)
library(dplyr)
library(tidyr)
library(purrr)

#Load Data
MicrosoftClassifiedFaces <- read.csv("~/github/face-recognition/MicrosoftClassifiedFaces.csv")
AnimetricsClassifiedFaces <- read.csv("~/github/face-recognition/AnimetricsClassifiedFaces.csv")
SkybiometryClassifiedFaces <- read.csv("~/github/face-recognition/SkybiometryClassifiedFaces.csv")
GoogleClassifiedFaces <- read.csv("~/github/face-recognition/GoogleClassifiedFaces.csv")
ManualClassifiedFaces <- read.csv("~/github/face-recognition/ManualClassifiedFaces.csv")
ManualClassifiedScenes <- read.csv("~/github/face-recognition/ManualClassifiedScenes.csv")

#Check all equal length
length(unique(MicrosoftClassifiedFaces$file))
length(unique(AnimetricsClassifiedFaces$file))
length(unique(SkybiometryClassifiedFaces$file))
length(unique(GoogleClassifiedFaces$file))

#Remove images with no faces
MicrosoftClassifiedFaces <- MicrosoftClassifiedFaces[!is.na(MicrosoftClassifiedFaces$faceRectangle.top),]
AnimetricsClassifiedFaces <- AnimetricsClassifiedFaces[!is.na(AnimetricsClassifiedFaces$topLeftX),]
SkybiometryClassifiedFaces <- SkybiometryClassifiedFaces[!is.na(SkybiometryClassifiedFaces$width),]
GoogleClassifiedFaces <- GoogleClassifiedFaces[!is.na(GoogleClassifiedFaces$rollAngle),]

#Manual Classified Faces minimum size set
ManualClassifiedFaces <- ManualClassifiedFaces %>%
  mutate(type = "Manual", time.user.self=NA, time.sys.self=NA, time.elapsed=NA, ID=1:NROW(ManualClassifiedFaces))

#Merge data
MicrosoftMerge <- MicrosoftClassifiedFaces %>% 
  mutate(type = "Microsoft", ID=NA,
         minX = faceRectangle.left, maxX = faceRectangle.left + faceRectangle.width,
         minY = faceRectangle.top, maxY = faceRectangle.top + faceRectangle.height) %>%
  dplyr::select(file, type, ID, time.user.self, time.sys.self, time.elapsed, minX, maxX, minY, maxY)
AnimetricsMerge <- AnimetricsClassifiedFaces %>%
  mutate(type = "Animetrics",  ID=NA,
         minX = topLeftX, maxX = topLeftX + width,
         minY = topLeftY, maxY = topLeftY + height) %>%
  dplyr::select(file, type, ID, time.user.self, time.sys.self, time.elapsed, minX, maxX, minY, maxY)
SkyBiometryMerge <- SkybiometryClassifiedFaces %>% 
  mutate(type = "Skybiometry", ID=NA,
         minX = (center.x - width/2)*8, maxX = (center.x + width/2)*8,
         minY = (center.y - height/2)*4.5, maxY = (center.y + height/2)*4.5) %>%
  dplyr::select(file, type, ID, time.user.self, time.sys.self, time.elapsed, minX, maxX, minY, maxY)
GoogleMerge <- GoogleClassifiedFaces %>% 
  rowwise() %>%
  mutate(type = "Google", ID=NA,
         minX = min(boundingPoly.x1, boundingPoly.x4), maxX = min(boundingPoly.x2, boundingPoly.x3),
         minY = min(boundingPoly.y1, boundingPoly.y2), maxY = min(boundingPoly.y3, boundingPoly.y4)) %>%
  ungroup() %>%
  mutate(minX = ifelse(is.na(minX), 0, minX), minY = ifelse(is.na(minY), 0, minY)) %>%
  dplyr::select(file, type, ID, time.user.self, time.sys.self, time.elapsed, minX, maxX, minY, maxY)
ManualMerge <- ManualClassifiedFaces %>%
  rename(minX = xmin, maxX = xmax,
         minY = ymin, maxY = ymax) %>%
  dplyr::select(file, type, ID, time.user.self, time.sys.self, time.elapsed, minX, maxX, minY, maxY)
mergedData <- rbind(MicrosoftMerge, AnimetricsMerge, SkyBiometryMerge, GoogleMerge, ManualMerge)
mergedData$type <- factor(mergedData$type)
mergedData$file <- factor(as.character(mergedData$file))

#BOXID
prepareFaceBox <- function(data){
  boxX <- c(data$minX, data$minX, data$maxX, data$maxX, data$minX)
  boxY <- c(data$maxY, data$minY, data$minY, data$maxY, data$maxY)
  data.frame(x = boxX, y = boxY)
}

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
            if(intersectArea/totalArea > 0.1){
              boxes[compareBox,"boxID"] <- boxes[newBox,"boxID"]
            }
          }
        }
      }
    }
  }
  return(boxes)
}

#Match faces
mergedFaceMatches <- mergedData %>%
  split(.$file) %>%
  map_df(~ boxOverlap(.x))

#Does it match manual?
mergedFaceMatches <- mergedFaceMatches %>% group_by(file, boxID) %>%
  mutate(matchesManual = any(type == "Manual"))

#size of box
mergedFaceMatches <- mergedFaceMatches %>% mutate(size = (maxX - minX) * (maxY - minY))

classifiedIMG <- mergedFaceMatches[mergedFaceMatches$file%in%ManualClassifiedScenes$file,]
#Calculate percentage of box matching manual by API type
classifiedIMG[classifiedIMG$type!="Manual",] %>% group_by(type) %>% summarise(percent = mean(matchesManual))

### MERGE ALL THE DATA!
metaManualClassifiedFaces <- merge(dplyr::select(ManualClassifiedFaces, file, facecounter, detect, obscured, lighting, headangle, glasses, visorhat, ID),
                                   dplyr::select(dplyr::ungroup(dplyr::filter(mergedFaceMatches, type=="Manual")), ID, boxID), by="ID")
#metaIMG <- merge(ManualClassifiedScenes, metaManualClassifiedFaces, by="file")
#MatchingMetaIMG <- merge(dplyr::select(metaIMG, -ID), dplyr::select(mergedFaceMatches, -ID), by=c("file", "boxID"))
#ALLmetaIMG <- merge(dplyr::select(metaIMG, -ID), dplyr::select(mergedFaceMatches, -ID), by=c("file", "boxID"), all.y = TRUE)

## New method to retain scene information on non-matches
## Pairwise merge, always include file, change merge order

#metaFaces <- merge(metaManualClassifiedFaces, classifiedIMG, by="file") %>% dplyr::select(-ID.y, -boxID.x) %>% rename(ID = ID.x, boxID = boxID.y)
metaFaces <- merge(metaManualClassifiedFaces, classifiedIMG, by="file", all=TRUE) %>% dplyr::select(-ID.y, -boxID.x) %>% rename(ID = ID.x, boxID = boxID.y)

ALLmetaIMG <- merge(ManualClassifiedScenes, metaFaces, by="file", all=TRUE)

#Extract file name info
fileSplit <- strsplit(unique(as.character(ALLmetaIMG$file)), "_")
fileSplit <- as.data.frame(do.call(rbind, fileSplit))
colnames(fileSplit) <- c("year", "court", "round", "player1", "country1", "junk1", "player2", "country2", "matchDraw", "junk2")
fileInfo <- cbind(file = unique(as.character(ALLmetaIMG$file)), fileSplit[,-c(6, 10)])
ALLmetaIMG <- merge(ALLmetaIMG, fileInfo, by="file")

#Finding duplicates
ALLmetaIMG <- ALLmetaIMG %>% group_by(file, boxID) %>%
  mutate(duplicates = duplicated(type))

#Create plots
makePlot <- function(imgList, mergeData, matchBox=TRUE){
  for(i in imgList){
    img <- load.image(paste0("images/", i))
    plot(img)
    faceData <- mergeData %>% filter(file == i)
    message(i)
    if(NROW(faceData) > 0){
      for(face in 1:NROW(faceData)){
        faceBox <- faceData[face,] %>% prepareFaceBox()
        if(matchBox){
          lines(faceBox$x, faceBox$y, col=as.numeric(faceData[face,"boxID"])+1)
          print(paste0(faceData[face,]$type, ": ", paste0(as.numeric(faceData[face,"boxID"])+1)))
        }
        else{
          lines(faceBox$x, faceBox$y, col=as.numeric(faceData[face,"type"])+1)
          print(paste0(faceData[face,]$type, ": ", paste0(as.numeric(faceData[face,"type"])+1)))
        }
      }
      if(!matchBox){
        legend("topright", legend = unique(faceData$type), col = unique(as.numeric(faceData$type) + 1), lty = 1)
      }
    }
  }
}

makePlot(as.character(a[a$size>75000,]$file), ALLmetaIMG)
makePlot("2016_SC2_R02_XHan_CHN_vs_YPutintseva_KAZ_WS204_clip.0098.png", ALLmetaIMG)

makePlot(b[1], ALLmetaIMG)
