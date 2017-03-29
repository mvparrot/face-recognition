### API loop function
library(jsonlite)
setwd("")

reqsize <- 97

# Starting script for SkyBiometry API
api_key <- ""
api_secret <- ""

getFacesSkyBiometry <- function(faceTags){
  if(length(faceTags$photos$tags[[1]]) > 1){
    tagdata <- flatten(faceTags$photos$tags[[1]])[,-1]
  }
  else{
    tagdata <- data.frame(label = NA, confirmed = NA, manual = NA, width = NA, height = NA,
                          yaw = NA, roll = NA, pitch = NA, points = NA, similarities = NA, tid = NA, recognizable = NA,
                          attributes.face.value = NA, attributes.face.confidence = NA, center.x = NA, center.y = NA,
                          eye_left.x = NA, eye_left.y = NA, eye_left.confidence = NA, eye_left.id = NA,
                          eye_right.x = NA, eye_right.y = NA, eye_right.confidence = NA, eye_right.id = NA,
                          mouth_center.x = NA, mouth_center.y = NA, mouth_center.confidence = NA, mouth_center.id = NA,
                          nose.x = NA, nose.y = NA, nose.confidence = NA, nose.id = NA)
  }
  cbind(file = strsplit(faceTags$photos$url, split = "/")[[1]][length(strsplit(faceTags$photos$url, split = "/")[[1]])], url = faceTags$photos$url, tagdata)
}

request_pattern <- "http://api.skybiometry.com/fc/faces/detect.json?api_key=KEY&api_secret=SECRET&urls=IMAGE"
the_request <- function(url) 
  sub("IMAGE", url, sub("SECRET", api_secret, sub("KEY", api_key, request_pattern)))
the_request <- Vectorize(the_request)

currentFaces <- read.csv("SkybiometryClassifiedFaces.csv")
allimages <- read.csv("imagenames.csv")$imagelist
imagelist <- allimages[-match(unique(na.omit(currentFaces[,"file"])), allimages)]
if (length(imagelist) == 0){
  imagelist <- allimages
}


## Manual check on http://mitchelloharawild.com/tennis/ image server
# imagelist <- c(
#   "2016_HSA_R01_BTomic_AUS_vs_DIstomin_UZB_MS157_clip.0003.png",
#   "2016_HSA_R01_BTomic_AUS_vs_DIstomin_UZB_MS157_clip.0020.png"
# )

# for (i in 0:floor(length(imagelist)/reqsize)){
#   image_urls <- na.omit(imagelist[100*i + (1:reqsize)])
#   #image_urls <- paste0("http://mitchelloharawild.com/tennis/", image_urls)
#   image_urls <- paste0("http://on-the-t.com/assets/match_images/", image_urls)
#   requests <- the_request(image_urls)
#   SkyRequest <- lapply(requests, readLines)
#   con <- lapply(SkyRequest, fromJSON)
#   parsedFaces <- lapply(con, getFacesSkyBiometry)
#   final_df <- do.call(rbind,parsedFaces)
#   #Save backup file
#   write.csv(final_df, file=paste0("SkybiometryBackup/SkybiometryClassifiedFaces_", Sys.Date(), "_", i, ".csv"), row.names = FALSE)
#   
#   #Save current combined file
#   currentFaces <- rbind(currentFaces, final_df)
#   write.csv(currentFaces, file="SkybiometryClassifiedFaces.csv", row.names = FALSE)
#   
#   message(paste0("Successfully completed batch ", i+1, " of ", ceiling(length(imagelist)/reqsize)))
#   message(paste0("Time of next batch: ", as.POSIXct(con[[1]]$usage$reset_time, origin="1970-01-01")))
#   while(Sys.time() < as.POSIXct(con[[1]]$usage$reset_time, origin="1970-01-01")){
#     Sys.sleep(30)
#   }
# }

for (img in imagelist){
  image_url <- paste0("http://on-the-t.com/assets/match_images/", img)
  request <- the_request(image_url)
  time <- system.time(SkyRequest <- try(readLines(request)))
  while(class(SkyRequest) == "try-error"){
    message(paste0("ERROR: API call failed - ", img))
    message("Trying again in 10 minutes...")
    time <- system.time(SkyRequest <- try(readLines(request)))
  }
  parse_json <- fromJSON(SkyRequest)
  parsedFaces <- getFacesSkyBiometry(parse_json)
  final_df <- as.data.frame(parsedFaces)
  
  #Save current combined file
  currentFaces <- rbind(currentFaces, final_df)
  write.csv(currentFaces, file="SkybiometryClassifiedFaces.csv", row.names = FALSE)
  
  message(paste0("SUCCESS (", match(img, imagelist), "/", length(imagelist), ", ", round(time[3], digits = 2), "s): ", img))
  if(parse_json$usage$remaining == 0){
    #Save backup file
    write.csv(final_df, file=paste0("SkybiometryBackup/SkybiometryClassifiedFaces_", Sys.Date(), "_", match(img, imagelist), ".csv"), row.names = FALSE)
    message(paste0(Sys.Date(), " >> Backup saved"))
    
    #Wait until usage reset
    message(paste0("Time of next reset: ", as.POSIXct(parse_json$usage$reset_time, origin="1970-01-01")))
    # while(Sys.time() < as.POSIXct(parse_json$usage$reset_time, origin="1970-01-01")){
    #   Sys.sleep(30)
    # }
    Sys.sleep(as.numeric(difftime(as.POSIXct(parse_json$usage$reset_time, origin="1970-01-01"), Sys.time(), units="secs")))
  }
}





#### Emotions
library(httr)
imgURLS<-GoogleDriveImageURLS

#aname<-TestFaces$Filename[2]

#creates and posts the request
SkybiometryAPI <- function(name){
  img_url<-paste0("https://raw.githubusercontent.com/srkob1/face-recognition/master/Faces/", name, sep="")
  get_url<-paste0("http://api.skybiometry.com/fc/faces/detect.json?api_key=", 
                api_key, "&api_secret=", api_secret, "&urls=", img_url, "&attributes=mood", sep="")
  print(get_url)
  GET(url=get_url,
      content_type('application/json'), encode = 'json'
  )
}

#Checks time limit
CheckTime<-function(a){
  while(a$status_code != "200"){
    message(paste0("ERROR: API call failed - ", a$status_code))
    message("Trying again in 1 minute")
    Sys.sleep(1500)
    time <- system.time(Request <- try(a <- SkybiometryAPI(id)))}
  a<-a
}

#creates a data frame
getEmotionsSkyBiometry <- function(a){
  con<-content(a)
  if(length(con$photos[[1]]$tags)>0){
    attributes<-con$photos[[1]]$tags[[1]]$attributes
    tagdata<-as.data.frame(attributes)
  }
  else{
    tagdata <- data.frame(face.value=NA,face.confidence=NA, mood.value=NA, mood.confidence=NA, neutral_mood.value=NA,
neutral_mood.confidence=NA, anger.value=NA, anger.confidence=NA, disgust.value=NA, disgust.confidence=NA, 
fear.value=NA, fear.confidence=NA, happiness.value =NA, happiness.confidence=NA, sadness.value=NA, 
sadness.confidence=NA, surprise.value =NA, surprise.confidence=NA)
  }
  tagdata<-tagdata
}







library(tidyverse)
imgURLS <- read_csv("data/GoogleDriveImageURLS.csv")
TestFaces <- read.csv("data/TestFaces.csv")


#works but returns all the same results
myoutput <- TestFaces %>%
  split( .$Filename) %>%
  map(~ .$FileName %>% 
        SkybiometryAPI %>%
        CheckTime %>%
        getEmotionsSkyBiometry) %>%
  bind_rows(.id = "Filename")

write.csv(output, "output.csv")

#testcon, 

#Testing using testfaces


library(httr)
  
filenames<-imgURLS$Filename



#imagelist <- imgURLS$Filename[-match(unique(na.omit(currentFaces[,"aname"])), imgURLS$Filename)]
currentFaces <- read.csv("data/SkybiometryEmotions.csv")
val<-length(currentFaces$aname)
duplicated(currentFaces$aname)
val



for (img in (val+1):1398){
    aname<-imgURLS$Filename[img]
    print(imgURLS$Filename[img])
    a<-SkybiometryAPI(aname)
    CheckTime(a)
    final_df <- getEmotionsSkyBiometry(a)
    named_df <- cbind(aname, final_df)
    #Save current combined file
    
    currentFaces <- read.csv("data/SkybiometryEmotions.csv")
    currentFaces <- rbind(currentFaces, named_df)
    write.csv(currentFaces, file="data/SkybiometryEmotions.csv", row.names = FALSE)
    
    #message(paste0("SUCCESS (", match(img, imgURLS$Filename), "/", length(imagelist), ", ", round(time[3], digits = 2), "s): ", img))
    con<-content(a)
    if(con$usage$remaining == 0){
      #Save backup file
      #write.csv(currentFaces, file=paste0("SkybiometryBackup/SkybiometryClassifiedFaces_", Sys.Date(), "_", match(img, imagelist), ".csv"), row.names = FALSE)
      #message(paste0(Sys.Date(), " >> Backup saved"))
      
      #Wait until usage reset
      message(paste0("Time of next reset: ", as.POSIXct(con$usage$reset_time, origin="1970-01-01")))
      # while(Sys.time() < as.POSIXct(con$usage$reset_time, origin="1970-01-01")){
      #   Sys.sleep(30)
      # }
      Sys.sleep(as.numeric(difftime(as.POSIXct(con$usage$reset_time, origin="1970-01-01"), Sys.time(), units="secs")))
    }
  }
 
 



