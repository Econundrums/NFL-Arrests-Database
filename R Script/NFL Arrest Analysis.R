library(readxl)
library(plotly)
library(nnet)


df = read_excel("NFL Dataframe - Classified.xlsx")

## Defines violent crimes

violentCrimes = c("Domestic violence", "Bomb threat", 
                  "Sexual assault", "Battery",
                  "Assault, gun", "Domestic violence, rape",
                  "Attempted murder", 
                  "Battery, resisting arrrest", "Domestic",
                  "Gun, assault", "Burglary, assault",
                  "Manslaughter, child abuse", "Murder",
                  "Assault", "Robbery", "Child abuse",
                  "Assault, alcohol", "Murder, gun",
                  "DUI, assault", "Manslaughter", 
                  "Alcohol, assault", "Coercion, gun",
                  "Battery, alcohol",
                  "Domestic violence, alcohol", 
                  "Domestic violence, gun", "Theft, gun",
                  "Burglary, battery")

aggAssault = c("Domestic violence", "Bomb threat", 
               "Battery", "Assault, gun", "Attempted murder",
               "Battery, resisting arrrest", "Domestic", 
               "Gun, assault", "Assault", 
               "Child abuse", "Assault, alcohol",
               "DUI, assault", "Alcohol, assault", 
               "Coercion, gun", "Battery, alcohol", 
               "Domestic violence, alcohol", "Domestic violence, gun")

sexAssault = c("Sexual assault", "Domestic violence, rape")

robbery = c("Burglary, assault", "Robbery", 
            "Theft, gun", "Burglary, battery")

murder = c("Manslaughter, child abuse", "Murder", 
           "Murder, gun", "Manslaughter")

df$VIOLENT = ifelse(df$CATEGORY %in% violentCrimes, "Violent", "Non-violent")

df$VIOLENT_NONVIOLENT = ifelse(df$CATEGORY %in% aggAssault, "Aggravated Assault", 
                          ifelse(df$CATEGORY %in% sexAssault, "Sexual Assault/Rape",
                            ifelse(df$CATEGORY %in% robbery, "Robbery", 
                              ifelse(df$CATEGORY %in% murder, "Murder", "Non-violent"))))


## Violent vs. Non-violent bar chart

tab = sort(table(df$VIOLENT), decreasing = TRUE)

data = data.frame('Category' = names(tab), 'Count' = unname(tab))


barChart = plot_ly(data, x = data$Category, y = data$Count.Freq, type = 'bar', 
                   text = data$Count.Freq, textposition = 'auto')%>%
  layout(yaxis = list(title = "Count"), barmode = 'group')

barChart

## Violent crime by category pie chart

tab = sort(table(df[df$VIOLENT == "Violent" ,'VIOLENT_NONVIOLENT']), decreasing = TRUE)

data = data.frame('Category' = names(tab), 'Count' = unname(tab))

pieChart = plot_ly(data, labels = data$Category, values = data$Count.Freq, type = 'pie')

pieChart

## Violent crime count by player position donut chart

tab = sort(table(df[df$VIOLENT == "Violent" ,'POS']), decreasing = TRUE)

data = data.frame('Category' = names(tab), 'Count' = unname(tab))

donutChart1 = data %>% 
  group_by(Category) %>% 
  plot_ly(labels = data$Category, values = data$Count.Freq)%>% add_pie(hole = 0.6)

## Violent crime count by player position amongst convicted/guilty players donut chart

tab = sort(table(df[df$VIOLENT == "Violent" & df$GUILTY == 1 ,'POS']), decreasing = TRUE)

data = data.frame('Category' = names(tab), 'Count' = unname(tab))

donutChart2 = data %>% 
  group_by(Category) %>% 
  plot_ly(labels = data$Category, values = data$Count.Freq)%>% add_pie(hole = 0.6)

## View donut charts

donutChart1
donutChart2

## Sets dataframe to just guilty players

df = df[df$GUILTY == 1, ]

model = multinom(POS ~ VIOLENT, df)

summary(model)

z = summary(model)$coefficients/summary(model)$standard.errors
p = (1-pnorm(abs(z), 0, 1)) * 2

p

