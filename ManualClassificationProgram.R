#Mitchell O'Hara-Wild

#setwd() to your github repository before running
#In this repository, create a folder called "images" and store your images there.

require(imager)
require(shiny)
require(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  textOutput("info", h3),
  plotOutput("plot1",
             click = "plot_click",
             dblclick = "plot_dblclick",
             hover = "plot_hover",
             brush = "plot_brush",
             height="auto"
  ),# Partial example
  conditionalPanel(
    condition = "input.plot_brush",
    #Face
    h3("Face description"),
    
    fluidRow(
      column(4, 
             radioButtons("detect", label = h3("Detect Face"), 
                         choices = list("Player" = 0,
                                        "Other staff on court" = 1, "Fan" = 2, "None" = 3), selected = 0)
      ),
      column(4,
             radioButtons("obscured", label = h3("Face obscured"),
                          choices = list("Yes" = 1, "No" = 0), selected = 0)
      ),
      column(4,
             radioButtons("lighting", label = h3("Lighting"),
                          choices = list("Direct sunlight" = 0, "Shaded" = 1, "Partially shaded" = 2), selected = 0)
      )
    ),
    
    fluidRow(
      
      column(4, 
             radioButtons("headangle", label = h3("Head angle"), 
                         choices = list("Front on" = 0, "Back of head" = 1, 
                                        "Profile" = 2, "Other" = 3), selected = 0)
      ),
      column(4,
             radioButtons("glasses", label = h3("Glasses"),
                          choices = list("Yes" = 1, "No" = 0), selected = 0),
             actionButton("cancelface", "Cancel")
      ),
      column(4,
             radioButtons("visorhat", label = h3("Visor/hat"),
                          choices = list("Yes" = 1, "No" = 0), selected = 0),
             actionButton("nextface", "Save")
      )
    )
  ),
  conditionalPanel(
    condition = "!input.plot_brush",
    #Scene
    h3("Scene description"),
    
    fluidRow(
      column(4, 
             radioButtons("graphic", label = h3("2D Graphic"), 
                          choices = list("Live image" = 0, "2D Graphic" = 1), selected = 0),
             
             radioButtons("bg", label = h3("Background"), 
                          choices = list("Crowd" = 0,
                                         "Court" = 1, "Logo wall" = 2, "Not applicable" = 3), selected = 0)
             
      ),
      column(4,
             radioButtons("person", label = h3("Detectable Person"),
                          choices = list("Yes" = 1, "No" = 0), selected = 1),
             radioButtons("shotangle", label = h3("Shot angle"),
                          choices = list("Level with players" = 0,
                                         "Birds eye" = 1,
                                         "Upward angle" = 2),selected = 0)
      ),
      column(4,
             radioButtons("situation", label = h3("Situation"),
                          choices = list("Court in play" = 0, 
                                         "Court player close-up" = 1,
                                         "Court close-up not player" = 2,
                                         "Crowd" = 3,
                                         "Off court close up of player" = 4,
                                         "Transition" = 4),selected = 0),
             
             actionButton("nextimg", "Next image")
      )
    )
  )
)

server <- function(input, output, session) {
  if(!("images" %in% dir())){
    stop("Use setwd() to the github repository, with images in a folder called 'images'")
  }
  v <- try(
    reactiveValues(imagelist = dir("images")[-ifelse(length(match(na.omit(read.csv("ManualClassifiedScenes.csv")[,"file"]), dir("images")))!=0, match(na.omit(read.csv("ManualClassifiedScenes.csv")[,"file"]), dir("images")), 1:length(dir("images")))],
                   counter = 0, 
                   facedata = rep(NA, 11),
                   saveddataface = read.csv("ManualClassifiedFaces.csv"),
                   saveddatascene = read.csv("ManualClassifiedScenes.csv"))
  )
  if(class(v) == "try-error"){
    runjs("alert('Previously classified images not found.')")
    v <- reactiveValues(imagelist = dir("images"),
                        counter = 0,
                        facedata = rep(NA, 11),
                        saveddataface = NULL,
                        saveddatascene = NULL)
  }
  
  observe({
    #If you click submit
    input$nextimg
    
    #Save data to disk
    isolate({
      #Skip first run
      if(v$counter > 0){
        newSceneData <- data.frame(file=v$imagelist[v$counter], graphic = input$graphic, person = input$person, situation = input$situation, bg = input$bg, shotangle=input$shotangle)
        v$saveddatascene <<- rbind(v$saveddatascene, newSceneData)
        
        if(NCOL(v$facedata) > 1){ #If at least one entry
          v$facedata <<- as.data.frame(v$facedata)[-1,] #Remove NA row
          if(NROW(v$facedata)>0){
            v$facedata <<- cbind(file=v$imagelist[v$counter], v$facedata) #add file name
          }
          v$saveddataface <<- rbind(v$saveddataface, v$facedata) #add to data
        }
        
        
        #Save scene info
        write.csv(v$saveddatascene, file=paste0("ManualClassifiedScenes.csv"), row.names = FALSE)
        write.csv(v$saveddatascene, file=paste0("datahistory/ManualClassifiedScenes_", Sys.Date(), ":", as.POSIXlt(Sys.time())$hour, ".csv"), row.names = FALSE)
        
        #Save face info
        if(!is.null(v$saveddataface)){
          write.csv(v$saveddataface, file=paste0("ManualClassifiedFaces.csv"), row.names = FALSE)
          write.csv(v$saveddataface, file=paste0("datahistory/ManualClassifiedFaces_", Sys.Date(), ":", as.POSIXlt(Sys.time())$hour, ".csv"), row.names = FALSE)
        }
        #v$imagedata <- rep(NA, 4)
        v$facedata <- rep(NA, 11) 
      }
      
      v$counter <<- v$counter + 1 #change image
    })
  })
  
  output$plot1 <- renderPlot({
    plot(load.image(paste0("images/", v$imagelist[v$counter])))
    #print(imagelist[v$counter])
    #Add lines
    if(!all(is.na(v$facedata))){
      for(i in 2:NROW(v$facedata)){
        lines(c(rep(v$facedata[i,"xmin"], 2), rep(v$facedata[i,"xmax"], 2), v$facedata[i,"xmin"]),
              c(v$facedata[i,"ymin"], rep(v$facedata[i,"ymax"], 2), rep(v$facedata[i,"ymin"],2)) ,col="green")
        }
    }},
    height = function() {
      session$clientData$output_plot1_width*(450/800)
    }
  )
  
  observe({
    input$nextface
    
    #Remove brush from img
    runjs("document.getElementById('plot1_brush').remove()")
    
    #Save data
    isolate({
      newFaceData <- c(facecounter=ifelse(is.null(nrow(v$facedata)), 1, nrow(v$facedata)), xmin=input$plot_brush$xmin, xmax=input$plot_brush$xmax, ymin=input$plot_brush$ymin, ymax=input$plot_brush$ymax,
        detect=input$detect, obscured=input$obscured, lighting=input$lighting,
        headangle=input$headangle, glasses=input$glasses, visorhat=input$visorhat)
      if(length(newFaceData) > 7){
        v$facedata <<- rbind(v$facedata,newFaceData)
      }
    })
  })
  
  observe({
    input$cancelface
    
    #Remove brush from img
    runjs("document.getElementById('plot1_brush').remove()")
  })
  
  output$info <- renderText({
    paste0("Image ", v$counter, "/", length(v$imagelist))
  })
}


shinyApp(ui, server)
