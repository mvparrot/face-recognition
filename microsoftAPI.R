library(httr)
a <- POST(url="https://api.projectoxford.ai/face/v1.0/detect?returnFaceId=false&returnFaceLandmarks=true",
     add_headers(`Ocp-Apim-Subscription-Key` = "1f5f1bdb8e7d4a9abf25daea59ce79bc",
                 `Content-Type` = "application/json"),
     body = list(`url` = "http://mitchelloharawild.com/tennis/2016_HSA_R01_BTomic_AUS_vs_DIstomin_UZB_MS157_clip.0003.png"))


library(httr)

faceURL = "https://api.projectoxford.ai/face/v1.0/detect?returnFaceId=true&returnFaceLandmarks=true&returnFaceAttributes=age,gender,smile,facialHair"
img.url = "http://mitchelloharawild.com/tennis/2016_CT8_R02_TSmyczek_USA_vs_VTroicki_SRB_MS222_clip.0049.png"

mybody = list(url = img.url)



MicrosoftAPI <- function(img.url){
  POST(url = "https://api.projectoxford.ai/face/v1.0/detect?returnFaceId=false&returnFaceLandmarks=true", 
    content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = "1f5f1bdb8e7d4a9abf25daea59ce79bc")),
    body = list(url = img.url),
    encode = 'json'
  )
}
