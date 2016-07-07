# Starting script for SkyBiometry API
api_key <- "2411ca5a7294462b9923e6ef7f97eb24"
api_secret <- "f57d0bc3b0eb427389f6c890700b4749"

image_urls <- c(
	"https://dl.dropboxusercontent.com/u/49312773/2016_CT6_R01_BJovanovski_SRB_vs_ACornet_FRA_WS163_clip.0001.png",
	"https://dl.dropboxusercontent.com/u/49312773/2016_CT6_R01_BJovanovski_SRB_vs_ACornet_FRA_WS163_clip.0002.png",
	"https://dl.dropboxusercontent.com/u/49312773/2016_CT6_R01_BJovanovski_SRB_vs_ACornet_FRA_WS163_clip.0003.png",
	"https://dl.dropboxusercontent.com/u/49312773/2016_CT6_R01_EKulichkova_RUS_vs_APetkovic_GER_WS128_clip.0092.png"
)

request_pattern <- "http://api.skybiometry.com/fc/faces/detect.json?api_key=KEY&api_secret=SECRET&urls=IMAGE"

the_request <- function(url) 
	sub("IMAGE", url, sub("SECRET", api_secret, sub("KEY", api_key, request_pattern)))
	
the_request <- Vectorize(the_request)

requests <- the_request(image_urls)

test <- lapply(requests, readLines)

strsplit(test[[1]], ":") # Example of parsing start