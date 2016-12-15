### API loop function
library(jsonlite)
setwd("~/Desktop/face-recognition/")
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
#   parse_json <- lapply(SkyRequest, fromJSON)
#   parsedFaces <- lapply(parse_json, getFacesSkyBiometry)
#   final_df <- do.call(rbind,parsedFaces)
#   #Save backup file
#   write.csv(final_df, file=paste0("SkybiometryBackup/SkybiometryClassifiedFaces_", Sys.Date(), "_", i, ".csv"), row.names = FALSE)
#   
#   #Save current combined file
#   currentFaces <- rbind(currentFaces, final_df)
#   write.csv(currentFaces, file="SkybiometryClassifiedFaces.csv", row.names = FALSE)
#   
#   message(paste0("Successfully completed batch ", i+1, " of ", ceiling(length(imagelist)/reqsize)))
#   message(paste0("Time of next batch: ", as.POSIXct(parse_json[[1]]$usage$reset_time, origin="1970-01-01")))
#   while(Sys.time() < as.POSIXct(parse_json[[1]]$usage$reset_time, origin="1970-01-01")){
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
    Sys.sleep(600)
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

