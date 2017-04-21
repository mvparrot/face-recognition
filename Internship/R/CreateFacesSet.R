
library(magick)
ALLmetaIMGnamed<-read.csv("ALLmetaIMGnamed.csv")


ALLmetaIMGnamedFaces<-ALLmetaIMGnamed%>%
  filter(matchesManual) %>% 
  dplyr::select(-ID, -facecounter)%>%
  mutate(fileID = as.numeric(factor(file))) %>%
  mutate(FaceKey=paste(fileID, boxID, sep="-")) %>%
  mutate(FaceID=paste(fileID, boxID, substring(type, 1,2), sep="-"))


GoogleFaces<-ALLmetaIMGnamedFaces %>% subset(type=="Google")

#create data set of just faces
data<-GoogleFaces


#CreateFaceSet
for(n in 1:NROW(GoogleFaces)){
SubImage <- image_read(paste0("images/", data$file[n]))

cropValue<-paste0(data$maxX[n]-data$minX[n]+20, "x",data$maxY[n]-data$minY[n]+20,"+", data$minX[n]-10,"+", data$minY[n]-10, collapse = ", ")
Cropped<-image_crop(SubImage, cropValue)

image_write(Cropped, path=(paste0("Faces/","face-", data$FaceID[n], ".png", collapse = ", ")))
}

write.csv(GoogleFaces, "GoogleFaces.csv", row.names = FALSE)

