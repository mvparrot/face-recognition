setwd("~/github/face-recognition/")

#Load Packages
library(imager)
library(dplyr)

#Load Data
MicrosoftClassifiedFaces <- read.csv("~/github/face-recognition/MicrosoftClassifiedFaces.csv")
AnimetricsClassifiedFaces <- read.csv("~/github/face-recognition/AnimetricsClassifiedFaces.csv")
SkybiometryClassifiedFaces <- read.csv("~/github/face-recognition/SkybiometryClassifiedFaces.csv")
ManualClassifiedFaces <- read.csv("~/github/face-recognition/ManualClassifiedFaces.csv")

#Check all equal length
length(unique(MicrosoftClassifiedFaces$file))
length(unique(AnimetricsClassifiedFaces$file))
length(unique(SkybiometryClassifiedFaces$file))

#Remove images with no faces
MicrosoftClassifiedFaces <- MicrosoftClassifiedFaces[!is.na(MicrosoftClassifiedFaces$faceRectangle.top),]
AnimetricsClassifiedFaces <- AnimetricsClassifiedFaces[!is.na(AnimetricsClassifiedFaces$topLeftX),]
SkybiometryClassifiedFaces <- SkybiometryClassifiedFaces[!is.na(SkybiometryClassifiedFaces$width),]

#Manual Classified Faces minimum size set
ManualClassifiedFaces <- ManualClassifiedFaces[ManualClassifiedFaces$xmax - ManualClassifiedFaces$xmin > 36,]

#Merge data
MicrosoftMerge <- MicrosoftClassifiedFaces %>% 
  mutate(type = "Microsoft",
         minX = faceRectangle.left, maxX = faceRectangle.left + faceRectangle.width,
         minY = faceRectangle.top + faceRectangle.height, maxY = faceRectangle.top) %>%
  select(file, type, time.user.self, time.sys.self, time.elapsed, minX, maxX, minY, maxY)
AnimetricsMerge <- AnimetricsClassifiedFaces %>%
  mutate(type = "Animetrics", 
         minX = topLeftX, maxX = topLeftX + width,
         minY = topLeftY + height, maxY = topLeftY) %>%
  select(file, type, time.user.self, time.sys.self, time.elapsed, minX, maxX, minY, maxY)
SkyBiometryMerge <- SkybiometryClassifiedFaces %>% 
  mutate(type = "Skybiometry", 
         minX = (center.x - width/2)*8, maxX = (center.x + width/2)*8,
         minY = (center.y - height/2)*4.5, maxY = (center.y + height/2)*4.5) %>%
  select(file, type, time.user.self, time.sys.self, time.elapsed, minX, maxX, minY, maxY)
mergedData <- rbind(MicrosoftMerge, AnimetricsMerge, SkyBiometryMerge)
mergedData$type <- factor(mergedData$type)

prepareFaceBox <- function(data){
  boxX <- c(data$minX, data$minX, data$maxX, data$maxX, data$minX)
  boxY <- c(data$maxY, data$minY, data$minY, data$maxY, data$maxY)
  data.frame(x = boxX, y = boxY)
}

makePlot <- function(imgList, mergeData){
  devAskNewPage(TRUE)
  for(i in imgList){
    img <- load.image(paste0("images/", i))
    plot(img)
    faceData <- mergeData %>% filter(file == i)
    message(i)
    if(NROW(faceData) > 0){    
      for(face in 1:NROW(faceData)){
        faceBox <- faceData[face,] %>% prepareFaceBox()
        lines(faceBox$x, faceBox$y, col=as.numeric(faceData[face,"type"])+1)
        print(paste0(faceData[face,"type"], ": ", paste0(as.numeric(faceData[face,"type"])+1)))
      }
    }
  }
}

imgList <- unique(ManualClassifiedFaces$file)
img <- load.image(paste0("images/", "2016_HSA_R04_ARadwanska_POL_vs_AFriedsam_GER_WS403_clip.0081.png"))
plot(img)

imgList <- unique(ManualClassifiedFaces$file)

makePlot(dir("images")[-(1:150)], mergedData)
