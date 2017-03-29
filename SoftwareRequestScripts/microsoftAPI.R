library(httr)


faceURL = "https://westus.api.cognitive.microsoft.com/face/v1.0/detect?returnFaceId=false&returnFaceLandmarks=true&returnFaceAttributes=age,gender,smile,facialHair"
img.url = "https://www.whitehouse.gov/sites/whitehouse.gov/files/images/first-family/44_barack_obama[1].jpg"
mybody = list(url = img.url)
#faceKey = ""


aFace <- POST(url=faceURL, 
          content_type('application/json'),
          add_headers(.headers = c('Ocp-Apim-Subscription-Key' = faceKey)),
          body = mybody,
          encode = 'json')

faceResult=httr::content(aFace, as="text")
faceResult


MicrosoftAPI <- function(img.url){
  POST(url = faceURL, 
    content_type('application/json'), 
    add_headers(.headers = c('Ocp-Apim-Subscription-Key' = faceKey)),
    body = mybody,
    encode = 'json'
  )
}

#faceTags<-faceResult

getMicrosoft <- function(faceTags){
  if(length(faceTags[[1]]) > 1){
    tagdata <- flatten(faceTags[[1]])[,-1]
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
getMicrosoft(faceResult)



