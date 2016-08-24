library(ggplot2)


#Load ALLmetaIMG in (from analysis.R)
ggplot(ALLmetaIMG, aes(x = type)) + geom_bar(position="dodge")

#NA refers to the boxes which don't match manual (hence we don't have info about the image classification)
ggplot(ALLmetaIMG, aes(x = factor(x = factor(visorhat)), group=type, fill=type)) + geom_bar(position="dodge")
ggplot(ALLmetaIMG, aes(x = factor(x = factor(glasses)), group=type, fill=type)) + geom_bar(position="dodge")
ggplot(ALLmetaIMG, aes(x = factor(x = factor(shotangle)), group=type, fill=type)) + geom_bar(position="dodge")
ggplot(ALLmetaIMG, aes(x = factor(x = factor(graphic)), group=type, fill=type)) + geom_bar(position="dodge")
ggplot(ALLmetaIMG, aes(x = factor(x = factor(person)), group=type, fill=type)) + geom_bar(position="dodge")
ggplot(ALLmetaIMG, aes(x = factor(x = factor(situation)), group=type, fill=type)) + geom_bar(position="dodge")
ggplot(ALLmetaIMG, aes(x = factor(x = factor(bg)), group=type, fill=type)) + geom_bar(position="dodge")
ggplot(ALLmetaIMG, aes(x = factor(x = factor(detect)), group=type, fill=type)) + geom_bar(position="dodge")
ggplot(ALLmetaIMG, aes(x = factor(x = factor(obscured)), group=type, fill=type)) + geom_bar(position="dodge")
ggplot(ALLmetaIMG, aes(x = factor(x = factor(lighting)), group=type, fill=type)) + geom_bar(position="dodge")
ggplot(ALLmetaIMG, aes(x = factor(x = factor(headangle)), group=type, fill=type)) + geom_bar(position="dodge")


## TODO: Proportions of manual classifications.
a <- ALLmetaIMG %>% filter(matchesManual) %>% group_by(glasses, type) %>% summarise(a=n())
ggplot(a, aes(x=factor(glasses), y=a, fill=type)) + geom_bar(stat="identity", position = "dodge")
a$manVal <- rep(c(1344, 186), each=5) # MAKE THIS MORE GENERAL
b <- a %>% mutate(prop = a/manVal)
b
ggplot(filter(b, type!="Manual"), aes(x=factor(glasses), y=prop, fill=type)) + geom_bar(stat="identity", position = "dodge")

getManualCount <- function(type, count) {
  return(count[type == "Manual"])
}

a %>% group_by(glasses) %>% mutate(ManualCount = getManualCount(type, count)) %>%
  mutate(proportion = count/ManualCount)

ALLmetaIMG %>% filter(matchesManual) %>% group_by(glasses, type) %>% s
