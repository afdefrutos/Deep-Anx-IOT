#correlation
View(hpcnotime)
corrData <- correlation(hpcnotime)

View(corrData)
View(corrData[["correlation"]])



#plots

plot(hpcbn$Total, hpcbn$ReactiveH)

View(correlation(hpcbn))

ggplot(hpcbn , aes(x=Month , y=Total)) + geom_point(aes(color=Year))





#HeatMap
source("calendarHeat.R")
source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")


calendarHeat2 <- calendarHeat(hpc$Date, hpc$Global_Reactive_power,
                              varname="Reactive")

iris %>%
  filter(Species=="virginica") %>%
  select(Petal.Length, Species) %>%
  group_by(Species) %>%
  summarise(mean(Petal.Length))
