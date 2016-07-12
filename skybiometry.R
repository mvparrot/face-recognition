library(jsonlite) # Adding jsonlite for parsing
library(imager)
library(forecast)
library(httr)

# Starting script for SkyBiometry API
api_key <- "2411ca5a7294462b9923e6ef7f97eb24"
api_secret <- "f57d0bc3b0eb427389f6c890700b4749"

# Go to directory with images
#setwd("~/Courses/mvparrot/data/match_clips/images")

#image_urls <- file.path("http://on-the-t.com/assets/match_images", list.files())
#image_urls <- grep("RLA.*R07", image_urls, val = TRUE) # Only RLA R07 are posted
image_urls <- c(
  "http://mitchelloharawild.com/tennis/2016_HSA_R01_BTomic_AUS_vs_DIstomin_UZB_MS157_clip.0003.png",
  "http://mitchelloharawild.com/tennis/2016_HSA_R01_BTomic_AUS_vs_DIstomin_UZB_MS157_clip.0020.png"
)

request_pattern <- "http://api.skybiometry.com/fc/faces/detect.json?api_key=KEY&api_secret=SECRET&urls=IMAGE"

request_url<-"{http://api.skybiometry.com/fc/faces/detect.json?returnFaceId=false&returnFaceLAndmarks=true}"
headers<-"add_headers(Content-Type='application/json', Ocp-Apim-Subscription-Key ='2411ca5a7294462b9923e6ef7f97eb24')"


httr_request<-POST(request_url, headers, encode = "multipart")

??POST

the_request <- function(url) 
	sub("IMAGE", url, sub("SECRET", api_secret, sub("KEY", api_key, request_pattern)))
	

the_request <- Vectorize(the_request)

requests <- the_request(image_urls)

SkyRequest <- lapply(requests, readLines)


# Initial parsing attempt
parse_json <- lapply(SkyRequest, fromJSON)
# faceTags is the individual photos within SkyRequest
faceTags <- fromJSON(SkyRequest[[1]])
faceTags <- fromJSON(SkyRequest[[2]])

faceTags$photos$tags #gives the values without labelled pairs
faceTags$photos$tags[[1]] #gives the full set of labelled pairs

#create the face box that will be applied
makeBoxSkyBiometry <- function(center.x, center.y, width, height){
  minx <- center.x - width/2
  miny <- center.y - height/2
  maxx <- center.x + width/2
  maxy <- center.y + height/2
  x <- c(minx, minx, maxx, maxx, minx)*8
  y <- c(maxy, miny, miny, maxy, maxy)*4.5
  return(data.frame(x=x, y=y))
}

# apply the face information 
faceBox <- makeBoxSkyBiometry(faceTags$photos$tags[[1]]$center$x,faceTags$photos$tags[[1]]$center$y, faceTags$photos$tags[[1]]$width, faceTags$photos$tags[[1]]$height)

# Plot images and show face boxes and eyes
plot(load.image("images/2016_HSA_R01_BTomic_AUS_vs_DIstomin_UZB_MS157_clip.0020.png"))
for(i in 1:NROW(faceTags$photos$tags[[1]])){
  faceBox <- makeBoxSkyBiometry(faceTags$photos$tags[[1]]$center$x[i],faceTags$photos$tags[[1]]$center$y[i], faceTags$photos$tags[[1]]$width[i], faceTags$photos$tags[[1]]$height[i])
  lines(faceBox$x, faceBox$y ,col="green")
  points(faceTags$photos$tags[[1]]$eye_left$x[i]*8,faceTags$photos$tags[[1]]$eye_left$y[i]*4.5, col="green")
  points(faceTags$photos$tags[[1]]$eye_right$x[i]*8,faceTags$photos$tags[[1]]$eye_right$y[i]*4.5, col="red")
}

# gets data for further analysis
getFacesSkyBiometry <- function(faceTags){
  if(length(faceTags$photos$tags[[1]]) > 1){
    cbind(url = faceTags$photos$url, faceTags$photos$tags[[1]])
  }
  else{
    data.frame(url = NA, label=NA, confirmed = NA, manual=NA, width=NA, height=NA, yaw=NA, roll=NA,
               pitch = NA, attributes.face.value = NA, attributes.face.confidence = NA, points = NA, similarities = NA,
               tid = NA, recognizable = NA, center.x = NA, center.y = NA)
  }
}

# maps data to a datamat
mytest <- forecast:::datamat(faceTags$photos$tags[[1]])
#allocates correct column names
colnames(mytest) <- names(unlist(faceTags$photos$tags[[1]][1,]))

