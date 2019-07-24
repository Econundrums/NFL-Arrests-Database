# # Counts violent crimes, both charged and convicted
# 
# posTable = sort(table(df[df$VIOLENT == 1, "POS"]), decreasing = TRUE)
# posTableGuilty = sort(table(df[df$GUILTY == 1 & df$VIOLENT == 1, 
#                                "POS"]), decreasing = TRUE)
# 
# 
# # Count of crime by type with basic bar chart
# # NTS: We may want to group crimes together
# 
# crimeTable = sort(table(df$CATEGORY), decreasing = TRUE)
# crimeTable = crimeTable[crimeTable >= 5]
# crimes = names(crimeTable)
# crimeCount = unname(crimeTable)
# 
# crimeTableGuilty = sort(table(df[df$GUILTY == 1, "CATEGORY"])
#                         , decreasing = TRUE)
# crimeTableGuilty = crimeTableGuilty[crimeTableGuilty >=5 ]
# crimesGuilty = names(crimeTableGuilty)
# crimeCountGuilty = unname(crimeTableGuilty)
# 
# barChart = plot_ly(x = crimes, y = crimeCount, 
#             name = "NFL Arrests", type = "bar") %>%
#   add_trace(y = crimeCountGuilty, name = 'Guilty') %>%
#   layout(yaxis = list(title = "Count"), barmode = 'group')
# 
# # Top 5 teams with most players in the database.
# 
# teamTable = sort(table(df$TEAM), decreasing = TRUE)
# 
# teamNames = names(teamTable)
# teamCount = unname(teamTable)
# 
# team_DF = data.frame('Teams' = teamNames, 'Count' = teamCount)
# 
# pieChart = plot_ly(team_DF, labels = team_DF$Teams, 
#                    values = team_DF$Count.Freq, type = 'pie') 
# 
# # Top 5 teams with most guilty players in the database.
# 
# guiltyTeams = sort(table(df[df$GUILTY == 1, "TEAM"]), decreasing = TRUE)[1:5]
# 
# # Top 5 players who've allegedly commited the most crimes.
# 
# players = sort(table(df$NAMES), decreasing = TRUE)[1:5]
# 
# # Top 5 players who've commited the most crimes and been
# # found guilty.
# 
# guiltyPlayers = sort(table(df[df$GUILTY == 1, "NAMES"]), decreasing = TRUE)[1:5]
# 
# 
# 


##################################


library(readxl)
library(plotly)
library(ggplot2)
library(xlsx)
library(MASS)
library(nnet)

df = read_excel("NFL_DataClassified.xlsx")

# Defines violent crimes

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

AA = c("Domestic violence", "Bomb threat", "Battery", "Assault, gun", "Attempted murder",
       "Battery, resisting arrrest", "Domestic", "Gun, assault", "Assault", "Child abuse", "Assault, alcohol",
       "DUI, assault", "Alcohol, assault", "Coercion, gun", "Battery, alcohol", 
       "Domestic violence, alcohol", "Domestic violence, gun")

SAR = c("Sexual assault", "Domestic violence, rape")

robbery = c("Burglary, assault", "Robbery", "Theft, gun", "Burglary, battery")

murder = c("Manslaughter, child abuse", "Murder", "Murder, gun", "Manslaughter")

df$VIOLENT = ifelse(df$CATEGORY %in% violentCrimes, "Violent", "Non-violent")

df$VIOLENT_NONVIOLENT = ifelse(df$CATEGORY %in% AA, "Aggravated Assault", 
                          ifelse(df$CATEGORY %in% SAR, "Sexual Assault/Rape",
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

tab = sort(table(df[df$VIOLENT == "Violent" & df$GUILTY == 1 ,'POS']), decreasing = TRUE)

data = data.frame('Category' = names(tab), 'Count' = unname(tab))

donutChart2 = data %>% 
  group_by(Category) %>% 
  plot_ly(labels = data$Category, values = data$Count.Freq)%>% add_pie(hole = 0.6)

donutChart1
donutChart2

## Creates a bunch of columns of dummy variables by position

allPos = unique(df$POS)

for (i in 1:length(allPos)){
  df[,allPos[i]] = ifelse(df$POS == allPos[i], 1, 0)
}


df$VIOLENT = ifelse(df$VIOLENT == 'Violent', 1, 0)

#Shuffles the dataframe
set.seed(1)
df = sample_n(df, nrow(df))

#Splits data into training and test set
percentTrain = 0.6
lastTrainRow = round(percentTrain * nrow(df))
dfTrain = df[1:lastTrainRow, ]
dfTest = df[-(1:lastTrainRow), ]

mn = multinom(POS ~ VIOLENT, dfTrain)

predicted = mn %>% predict(dfTest)

mean(predicted == dfTest$POS)

z = summary(mn)$coefficients/summary(mn)$standard.errors
p = (1-pnorm(abs(z), 0, 1)) * 2


## Sets it to guilty players

df = df[df$GUILTY == 1, ]

set.seed(2)
df = sample_n(df, nrow(df))

lastTrainRow = round(percentTrain * nrow(df))
dfTrain = df[1:lastTrainRow, ]
dfTest = df[-(1:lastTrainRow), ]

mn = multinom(POS ~ VIOLENT, dfTrain)

predicted = mn %>% predict(dfTest)

mean(predicted == dfTest$POS)

z2 = summary(mn)$coefficients/summary(mn)$standard.errors
p2 = (1-pnorm(abs(z2), 0, 1)) * 2
