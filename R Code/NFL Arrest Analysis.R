library(readxl)
library(plotly)
library(tidyverse)
library(caret)


df = read_excel("Data/NFL Player Arrests.xlsx", sheet = "Results")

## Defines violent crimes

violentCrimes = c("Animal abuse", 
                  "Dogfighting", "Sexual battery",
                  "Animal cruelty", "Animal cruelty, drugs",
                  "Armed robbery", "Injury to elderly",
                  "Domestic violence", "Bomb threat", 
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
                  "Burglary, battery", "Sexual abuse")

aggAssault = c("Domestic violence", "Bomb threat", 
               "Battery", "Assault, gun", "Attempted murder",
               "Battery, resisting arrrest", "Domestic", 
               "Gun, assault", "Assault", 
               "Child abuse", "Assault, alcohol",
               "DUI, assault", "Alcohol, assault", 
               "Coercion, gun", "Battery, alcohol", 
               "Domestic violence, alcohol", "Domestic violence, gun",
               "Injury to elderly")

sexAssault = c("Sexual assault", "Domestic violence, rape",
               "Sexual battery", "Sexual abuse")

robbery = c("Burglary, assault", "Robbery", 
            "Theft, gun", "Burglary, battery",
            "Armed robbery")

murder = c("Manslaughter, child abuse", "Murder", 
           "Murder, gun", "Manslaughter", "DUI, manslaughter")

animalCruelty = c("Animal cruelty", "Animal cruelty, drugs", "Animal abuse")

df = df %>% mutate(
  VIOLENT = if_else(df$CATEGORY %in% violentCrimes, "Violent", "Non-violent"),
  VIOLENT_TYPE = case_when(
    df$CATEGORY %in% aggAssault ~ "Aggravated Assault",
    df$CATEGORY %in% sexAssault ~ "Sexual Assault",
    df$CATEGORY %in% robbery ~ "Robbery",
    df$CATEGORY %in% murder ~ "Murder or Manslaughter",
    df$CATEGORY %in% animalCruelty ~ "Animal Cruelty"
  )
)

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

## Logistic regression

logit2Prob = function(coefficients){
  odds = exp(coefficients)
  prob = odds / (1 + odds)
  return(prob)
}

dfGuilty = df[df$GUILTY == 1, ]
dummies = dummyVars(~ POS, data = dfGuilty) %>% predict(newdata = dfGuilty)
violent = ifelse(dfGuilty$VIOLENT == "Violent", 1, 0)
dfLogit = cbind(as.data.frame(violent), dummies)

logit = glm(violent ~ POSLB + POSOT + POSRB, family = binomial, data = dfLogit)
summary(logit)

# Get the odds and probabilities

coeffs = logit$coefficients
odds = c(exp(coeffs[1]), exp(coeffs[1]) * exp(coeffs[-1]))
probs = odds/(1 + odds)

# Countplot

ftViolent = with(df[df$POS %in% c("LB", "OT", "RB"),], table(POS, VIOLENT)) %>%
  as.data.frame()

p = ggplot(as.data.frame(ftViolent), aes(x = POS, y = Freq, fill = VIOLENT)) + 
  geom_bar(stat = "identity", position = position_dodge())
  

p + ggtitle("Count Violent vs. Non-violent by POS") + 
  geom_text(aes(label=as.character(Freq)), vjust=1.5, size=3.5, 
            position = position_dodge(0.9))

################## Other interesting facts #################################

# Player with most arrests.

countArrests = count(df, NAMES)
countArrests[countArrests$n == max(countArrests$n),]
df[df$NAMES == 'Adam Jones', "CATEGORY"]

# Player with most arrests for violent crimes.

countVlntArrests = count(df[df$VIOLENT == "Violent", ], NAMES)
countVlntArrests[countVlntArrests$n == max(countVlntArrests$n),]
df[df$NAMES %in% c('Adam Jones', 'Larry Johnson', 'Leonardo Carson'), ]

# Teams with most criminal players.

ftViolent = with(df, table(TEAM)) %>%
  as.data.frame()

ftViolent = ftViolent[order(ftViolent$Freq, decreasing = TRUE), ]

p = ggplot(ftViolent[1:20,], aes(x = reorder(TEAM, -Freq), y = Freq)) + 
  geom_bar(stat = "identity", position = position_dodge(), fill = "#ff5100")


p + ggtitle("Total Arrests by Team") + xlab("TEAM")
