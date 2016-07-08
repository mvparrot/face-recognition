to# Starting script for SkyBiometry API
api_key <- "2411ca5a7294462b9923e6ef7f97eb24"
api_secret <- "f57d0bc3b0eb427389f6c890700b4749"

# Go to directory with images
setwd("~/Courses/mvparrot/data/match_clips/images")

image_urls <- file.path("http://on-the-t.com/assets/match_images", list.files())
image_urls <- grep("RLA.*R07", image_urls, val = TRUE) # Only RLA R07 are posted

request_pattern <- "http://api.skybiometry.com/fc/faces/detect.json?api_key=KEY&api_secret=SECRET&urls=IMAGE"

the_request <- function(url) 
	sub("IMAGE", url, sub("SECRET", api_secret, sub("KEY", api_key, request_pattern)))
	
the_request <- Vectorize(the_request)

requests <- the_request(image_urls)

test <- lapply(requests, readLines)

strsplit(test[[1]], ":") # Example of parsing start