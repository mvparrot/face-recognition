library(httr)
#set up and return git web address for id
a <- GET(url="https://api.kairos.com/v2/analytics/{https://github.com/srkob1/face-recognition/blob/master/TestFaces/face-947-1-Go.png}",
          add_headers(`app_id` = "8078e093",
                      `app_key` = "dd97f6d1afd17676f7130e180068b760")
)

#img.url = "The git img url"

mybody = list(url = img.url)


KairosAPI <- function(img.url){
  GET(url="https://api.kairos.com/v2/analytics/{id}",
      add_headers(`app_id` = "8078e093",
                  `app_key` = "dd97f6d1afd17676f7130e180068b760")
  )
}

         
        