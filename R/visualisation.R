library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# colour scheme
type.colours <- c(Animetrics = "#B3E2CD",
                  Google = "#FDCDAC",
                  Manual = "#CBD5E8",
                  Microsoft = "#F4CAE4",
                  Skybiometry = "#E6F5C9")


#Load ALLmetaIMG in (from analysis.R)
ggplot(ALLmetaIMG, aes(x = type)) + geom_bar(position="dodge")

#NA refers to the boxes which don't match manual (hence we don't have info about the image classification)
ggplot(ALLmetaIMGnamed, aes(x = factor(x = factor(visorhat)), group=type, fill=type)) + geom_bar(position="dodge")  + scale_colour_manual(values=type.colours)
ggplot(ALLmetaIMGnamed, aes(x = factor(x = factor(glasses)), group=type, fill=type)) + geom_bar(position="dodge") + scale_colour_manual(values=type.colours)
ggplot(ALLmetaIMGnamed, aes(x = factor(x = factor(shotangle)), group=type, fill=type)) + geom_bar(position="dodge") + scale_colour_manual(values=type.colours)
ggplot(ALLmetaIMGnamed, aes(x = factor(x = factor(graphic)), group=type, fill=type)) + geom_bar(position="dodge") + scale_colour_manual(values=type.colours)
ggplot(ALLmetaIMGnamed, aes(x = factor(x = factor(person)), group=type, fill=type)) + geom_bar(position="dodge") + scale_colour_manual(values=type.colours)
ggplot(ALLmetaIMGnamed, aes(x = factor(x = factor(situation)), group=type, fill=type)) + geom_bar(position="dodge") + scale_colour_manual(values=type.colours)
ggplot(ALLmetaIMGnamed, aes(x = factor(x = factor(bg)), group=type, fill=type)) + geom_bar(position="dodge") + scale_colour_manual(values=type.colours)
ggplot(ALLmetaIMGnamed, aes(x = factor(x = factor(detect)), group=type, fill=type)) + geom_bar(position="dodge") + scale_colour_manual(values=type.colours)
ggplot(ALLmetaIMGnamed, aes(x = factor(x = factor(obscured)), group=type, fill=type)) + geom_bar(position="dodge") + scale_colour_manual(values=type.colours)
ggplot(ALLmetaIMGnamed, aes(x = factor(x = factor(lighting)), group=type, fill=type)) + geom_bar(position="dodge") + scale_colour_manual(values=type.colours)
ggplot(ALLmetaIMGnamed, aes(x = factor(x = factor(headangle)), group=type, fill=type)) + geom_bar(position="dodge") + scale_colour_manual(values=type.colours)


## TODO: Proportions of manual classifications.
a <- ALLmetaIMG %>% filter(matchesManual) %>% group_by(glasses, type) %>% summarise(a=n())
ggplot(a, aes(x=factor(glasses), y=a, fill=type)) + geom_bar(stat="identity", position = "dodge")
a$manVal <- rep(c(1344, 186), each=5) # MAKE THIS MORE GENERAL
b <- a %>% mutate(prop = a/manVal)
b
ggplot(filter(b, type!="Manual"), aes(x=factor(glasses), y=prop, fill=type)) + geom_bar(stat="identity", position = "dodge")



a %>% group_by(glasses) %>% mutate(ManualCount = getManualCount(type, count)) %>%
  mutate(proportion = count/ManualCount)

getManualCount <- function(type, nTotal) {
  return(nTotal[type == "Manual"])
}
ggplotProportion <- function(dataset, factorVar){
  factorVar <- deparse(substitute(factorVar))
  dataset <- dataset %>% filter(matchesManual) %>% group_by_(factorVar, "type") %>% summarise(nTotal=n()) %>% group_by_(factorVar) %>% mutate(ManualCount = getManualCount(type, nTotal)) %>%
    mutate(proportion = nTotal/ManualCount) %>% rename_(xvar = factorVar) %>% filter(type!="Manual")
  ggplot(dataset, aes(x=factor(xvar), y=proportion, group = type, fill=type)) + geom_bar(stat = "identity", position = "dodge") +
    ylab("Proportion of faces matched") + xlab(factorVar)
}

ggplotProportion(ALLmetaIMG, visorhat)
ggplotProportion(ALLmetaIMG, glasses)
ggplotProportion(ALLmetaIMG, shotangle)
ggplotProportion(ALLmetaIMG, graphic)
ggplotProportion(ALLmetaIMG, person)
ggplotProportion(ALLmetaIMG, situation)
ggplotProportion(ALLmetaIMG, bg)
ggplotProportion(ALLmetaIMG, detect)
ggplotProportion(ALLmetaIMG, obscured)
ggplotProportion(ALLmetaIMG, lighting)
ggplotProportion(ALLmetaIMG, headangle)
