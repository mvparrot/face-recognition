# Starting script for Animetrics FaceR API
a.api_key <- ""
a.selection <- "FULL"

image_urls <- c("mitchelloharawild.com/tennis/2016_HSA_R01_BTomic_AUS_vs_DIstomin_UZB_MS157_clip.0003.png")
                
request_pattern <- "http://api.animetrics.com/v1/detect?api_key=KEY&selection=SELECTION&urls=IMAGE"

the_request <- function(url)
  sub("IMAGE", url, sub("SELECTION", a.selection, sub("KEY", a.api_key, request_pattern)))

the_request <- Vectorize(the_request)

requests <- the_request(image_urls)

# for a single url request
a <- readLines(requests)

# convert from JSON
fromJSON(a)

#for multiple url requests
a <- lapply(requests, readLines)

#Test using some images uploaded to Mitchs server
alllinks <- paste0("mitchelloharawild.com/tennis/", dir("images")[36:45])
head(alllinks)
