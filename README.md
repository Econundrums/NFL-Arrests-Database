# NFL Arrests Database

# Introduction

This will be an introduction. 

# Data Collection and Cleaning

The first thing we need to do is collect and clean the data, which I will do by building a webscraper to go on the [USA TODAY](https://www.usatoday.com/sports/nfl/arrests/) website scrap the database. 

*A very sincere hat tip to [Andrew Cantino](https://vimeo.com/tectonic) for creating the [Selector Gadget](https://selectorgadget.com/) tool that helped me parse through html code.

```R
install.packages('rvest')
library('rvest')

WebScraper = function(url, css){
  webpage = read_html(url)
  htmlColumn = html_nodes(webpage, css)
  column = html_text(htmlColumn)
  return(column)
  }
  
website = 'https://www.usatoday.com/sports/nfl/arrests/'

NAMES = WebScraper(website, '.arrest-name')
POS = WebScraper(website, '.arrest-narrow:nth-child(4)')
TEAM = WebScraper(website, '.arrest-narrow:nth-child(2)')
CASE = WebScraper(website, '.arrest-midsize:nth-child(5)')
CATEGORY = WebScraper(website, '.arrest-midsize:nth-child(6)')
DESCRIPTION = WebScraper(website, '.left:nth-child(7)')
OUTCOME = WebScraper(website, '.outcome')

NFL_dataframe = data.frame("NAMES" = NAMES, "POS" = POS, "TEAM" = TEAM,
                           "CASE" = CASE, "CATEGORY" = CATEGORY, 
                           'DESCRIPTION' = DESCRIPTION, 
                           'OUTCOME' = OUTCOME, stringsAsFactors = FALSE)
```

Note that in the "OUTCOME" column of the dataframe there are many entries with the sentence "Resolution undetermined." These are players who have yet to be deemed guilty or not guilty. For my purpose, I need to remove these players from the database since I only care about players with known verdicts. The following piece of code will accomplish this.

```R

NFL_dataframe = NFL_dataframe[grepl("Resolution undetermined", 
                                    NFL_dataframe$OUTCOME) == FALSE, ]
NFL_dataframe$GUILTY = NA

```

# Text Mining and Prediction: Naive Bayes Edition

Next I need to classify each remaining player within the database as either guilty or not guilty -- i.e. a value of 1 in the GUILTY column if guilty and 0 otherwise.

I *could* just go through each row and categorize each row a 1 or a 0 based on the description in the OUTCOME column, but that would take too long. Instead, I'll use a homemade Naive Bayes R script to mine the text in the OUTCOME column and then predict whether or not to classify each player as guilty or not guilty.

If you want to learn the theory behind Naive Bayes (NB) combined with Bag-of-Words (BoW) as a method of text classification, there are plenty of references out there, but I found that I thought were most useful were [here](https://www.youtube.com/watch?v=EGKeC2S44Rs) (for learning how to do it by hand), [here](https://web.stanford.edu/~jurafsky/slp3/slides/7_Sent.pdf) (as an overview of sentiment analysis), and [here](https://nlp.stanford.edu/IR-book/html/htmledition/the-bernoulli-model-1.html) (more into the weeds. About ~90% of how my code is structured is based on the information contained within this link).  

## Training and Testing the NB Algorithm

First, I will (unfortunately) have to manually label some of the rows with either a 0 or a 1 so that the algorithm can train on said rows and use the results to make predictions on the rest of the dataset. Thus, I manually labeled in Excel the first 100 rows in the Excel of the NFL Database. 

Now for the actual code -- below is my homemade version of the Bernoulli NB algorithm.

```R

library(dplyr)
library(tm)
library(tidytext)

NaiveBayes = function(dataFrame, textColumn, outcomeColumn, percentTrain){
  
  #Shuffles the dataframe
  set.seed(0)
  df = sample_n(dataFrame, nrow(dataFrame))
  
  #Splits data into training and test set
  lastTrainRow = round(percentTrain * nrow(df))
  train = df[1:lastTrainRow, ]
  test = df[-(1:lastTrainRow), ]
  
  #Get a corpus of the training data and clean it by removing lower-case
  #letters, punctuation, English "stopwords", and whitespace.
  
  trainCorpus = VCorpus(VectorSource(train[, textColumn]))
  trainCorpus = trainCorpus %>% 
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeWords, stopwords(kind = "en")) %>%
    tm_map(stripWhitespace)
  
  #Gets a document term matrix to use for the actual algorithm and 
  #filters infrequent words
  
  dtmTrain = DocumentTermMatrix(trainCorpus)
  
  freqTerms = findFreqTerms(dtmTrain, 5)
  
  dtmTrain = DocumentTermMatrix(trainCorpus, control =
                                   list(dictionary = freqTerms))
  
  #Starts the Bernoulli Naive Bayes Algorithm
  
  #1. Extract vocabulary from training data
  
  vocab = Terms(dtmTrain)
  
  #2. Count the total number of documents N
  
  totDocs = nDocs(dtmTrain)
  
  #3. Get the prior probabilities for class 1 and class 0 -- i.e. N_c/N
  
  nClass0 = nrow(train[train[ ,outcomeColumn] == 0, ])
  nClass1 = nrow(train[train[ ,outcomeColumn] == 1, ])
  priorProb0 = nClass0 / totDocs
  priorProb1 = nClass1 / totDocs
  
  #4. Count the number of documents in each class 'c' containing term 't'
  
  trainTibble = tidy(as.matrix(dtmTrain))
  trainTibble$categ = train[,outcomeColumn]
  nDocsPerTerm0 = colSums(trainTibble[trainTibble$categ == 0, ])
  nDocsPerTerm1 = colSums(trainTibble[trainTibble$categ == 1, ])

  
  #5. Get the conditional probabilities for the data
  
  termCondProb0 = (nDocsPerTerm0 + 1)/(nClass0 + 2)
  termCondProb1 = (nDocsPerTerm1 + 1)/(nClass1 + 2)
  
  #6. Clean the test data
  
  testCorpus = VCorpus(VectorSource(test[,textColumn]))
  testCorpus = testCorpus %>% 
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeWords, stopwords(kind = "en")) %>%
    tm_map(stripWhitespace)
  
  dtmTest = DocumentTermMatrix(testCorpus)
  freqTermsTest = findFreqTerms(dtmTest, 5)
  dtmTest = DocumentTermMatrix(testCorpus, control =
                                   list(dictionary = freqTermsTest))
  
  #7. Extract vocab from test data
 
 vocabTest = Terms(dtmTest)
  
  #8. Compute "guilty" vs. "not guilty" probability of each player, compare them, and classify player based on the result.
  
  classifiedRows = c()
  
  for (i in 1:nDocs(dtmTest)) {

    score0 = priorProb0
    score1 = priorProb1
  
    doc = colSums(as.matrix(dtmTest[i, ]))
    doc = doc[doc>0]
  
    for (j in 1:length(termCondProb0)){
      if (names(termCondProb0)[j] %in% names(doc) == TRUE)
        score0 = score0 * unname(termCondProb0[j])
      else
        score0 = score0 *(1 - unname(termCondProb0[j]))
    }
  
    for (k in 1:length(termCondProb1)){
      if (names(termCondProb1)[k] %in% names(doc) == TRUE)
        score1 = score1 * unname(termCondProb1[k])
      else
        score1 = score1 * (1 - unname(termCondProb1[k]))
    }
  
   if(score0 >= score1)
     classifiedRows = append(classifiedRows, 0)
   else
     classifiedRows = append(classifiedRows, 1)
  
  
   }

  
  return(list('classifiedRows' = classifiedRows, 'test' = test))

```
