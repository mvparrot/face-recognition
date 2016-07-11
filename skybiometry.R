library(jsonlite) # Adding jsonlite for parsing
library(imager)
# Starting script for SkyBiometry API
api_key <- "2411ca5a7294462b9923e6ef7f97eb24"
api_secret <- "f57d0bc3b0eb427389f6c890700b4749"

# Go to directory with images
setwd("~/Courses/mvparrot/data/match_clips/images")

image_urls <- file.path("http://on-the-t.com/assets/match_images", list.files())
image_urls <- grep("RLA.*R07", image_urls, val = TRUE) # Only RLA R07 are posted
image_urls <- c(
  "http://mitchelloharawild.com/tennis/2016_HSA_R01_BTomic_AUS_vs_DIstomin_UZB_MS157_clip.0003.png",
  "http://mitchelloharawild.com/tennis/2016_CT8_R02_TSmyczek_USA_vs_VTroicki_SRB_MS222_clip.0050.png",
  "http://mitchelloharawild.com/tennis/2016_CT8_R02_TSmyczek_USA_vs_VTroicki_SRB_MS222_clip.0052.png",
  "http://mitchelloharawild.com/tennis/2016_CT8_R02_TSmyczek_USA_vs_VTroicki_SRB_MS222_clip.0017.png",
  "http://mitchelloharawild.com/tennis/2016_HSA_R01_BTomic_AUS_vs_DIstomin_UZB_MS157_clip.0020.png"
)
request_pattern <- "http://api.skybiometry.com/fc/faces/detect.json?api_key=KEY&api_secret=SECRET&urls=IMAGE"

the_request <- function(url) 
	sub("IMAGE", url, sub("SECRET", api_secret, sub("KEY", api_key, request_pattern)))
	
the_request <- Vectorize(the_request)

requests <- the_request(image_urls)

test <- lapply(requests, readLines)


# Initial parsing attempt
parse_json <- lapply(test, fromJSON)
a <- fromJSON(test[[1]])
a <- fromJSON(test[[5]])
a$photos$tags[[1]]

makeBoxSkyBiometry <- function(center.x, center.y, width, height){
  minx <- center.x - width/2
  miny <- center.y - height/2
  maxx <- center.x + width/2
  maxy <- center.y + height/2
  x <- c(minx, minx, maxx, maxx, minx)*8
  y <- c(maxy, miny, miny, maxy, maxy)*4.5
  return(data.frame(x=x, y=y))
}

box <- makeBoxSkyBiometry(a$photos$tags[[1]]$center$x,a$photos$tags[[1]]$center$y, a$photos$tags[[1]]$width, a$photos$tags[[1]]$height)

plot(load.image("images/2016_HSA_R01_BTomic_AUS_vs_DIstomin_UZB_MS157_clip.0020.png"))
for(i in 1:NROW(a$photos$tags[[1]])){
  box <- makeBoxSkyBiometry(a$photos$tags[[1]]$center$x[i],a$photos$tags[[1]]$center$y[i], a$photos$tags[[1]]$width[i], a$photos$tags[[1]]$height[i])
  lines(box$x, box$y ,col="green")
  points(a$photos$tags[[1]]$eye_left$x[i]*8,a$photos$tags[[1]]$eye_left$y[i]*4.5, col="green")
  points(a$photos$tags[[1]]$eye_right$x[i]*8,a$photos$tags[[1]]$eye_right$y[i]*4.5, col="red")
  
}

getFacesSkyBiometry <- function(a){
  if(length(a$photos$tags[[1]]) > 1){
    cbind(url = a$photos$url, a$photos$tags[[1]])
  }
  else{
    data.frame(url = NA, label=NA, confirmed = NA, manual=NA, width=NA, height=NA, yaw=NA, roll=NA,
               pitch = NA, attributes.face.value = NA, attributes.face.confidence = NA, points = NA, similarities = NA,
               tid = NA, recognizable = NA, center.x = NA, center.y = NA)
  }
  
}

do.call(rbind,parsedFaces)


mytest <- forecast:::datamat(a$photos$tags[[1]])

colnames(mytest) <- names(unlist(a$photos$tags[[1]][1,]))

View(mytest)
NCOL(mytest)
