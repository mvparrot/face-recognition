library(httr)


emotURL = "https://westus.api.cognitive.microsoft.com/emotion/v1.0/recognize"
mybody = list(url = img.url)
emotKey = ""


#bEmot <- POST(url=emotURL, 
#          content_type('application/json'),
#          add_headers(.headers = c('Ocp-Apim-Subscription-Key' = emotKey)),
#          body = list(url = img.url),
#          encode = 'json')
#
#emotResult=httr::content(aEmot)[[1]]
#emotResultdf <-as.data.frame((emotResult$scores))

#keep for testing works img.url =  "https://drive.google.com/uc?export=download&id=0B0i5WV2jMQ1vSG1Va25wRmVkQkE"
# works img.url =  "https://drive.google.com/uc?export=download&id=0B0i5WV2jMQ1vSVlvWjlZUktpMUU"


#send the request to microsoft
MicrosoftAPI <- function(id){
  POST(url=emotURL,
      content_type('application/json'),
      add_headers(.headers = c('Ocp-Apim-Subscription-Key' = emotKey)),
      body = list(url = paste0("https://drive.google.com/uc?export=download&id=", id)),
      encode = 'json'
  )
}

library(httr)
library(dplyr)
#expand into a dataframe
getEmotionsMicrosoft <- function(a){
  
  if (length(content(a, "raw"))>2) {
    emotResult = httr::content(a)[[1]]
    tagdata <- as.data.frame((emotResult$scores))

  }
  else{
    tagdata <- data.frame(
      anger = NA,
      contempt = NA,
      disgust = NA,
      fear = NA,
      happiness = NA,
      neutral = NA,
      sadness = NA,
      surprise = NA
    )

  }
  
}

#combine with previous results
EmotionsResult <- function(imagelist) {
  for (img in 1:length(imagelist)) {
    id <- substr(as.character(imgURLS$URL[img]), 33, 60)
    a <- MicrosoftAPI(id)
    result <- getEmotionsMicrosoft(a)
    named<-cbind(imgURLS$Filename[img], result)
    names(named)[1]<-"FileName"
    print(img)
    print(named)
    currentFaces <- read.csv("MicrosoftEmotions.csv")
    currentFaces1 <- rbind(currentFaces, named)
    write.csv(currentFaces1, file = "MicrosoftEmotions.csv", row.names = FALSE)
    
  }
}

imagelist<-as.list(unique(imgURLS$Filename))



imgURLS <- read_csv("GoogleDriveImageURLS.csv")
imgURLS$Filename



#run: should result in a csv with 1319 results (plus column headings)
#currently results in 20 rows then gives an error
#Error in data.frame(..., check.names = FALSE) : 
#  arguments imply differing number of rows: 1, 0


EmotionsResult(imagelist)


