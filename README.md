# NFL Arrests Database

# Introduction

This will be an introduction. 

# Data Collection and Cleaning

The first thing we need to do is collect and clean the data, which will be accomplished by building a webscraper. For this task we'll need to use the package 'rvest' for some useful html reading and cleaning functions, as well as tool called [Selector Gadget](https://selectorgadget.com/) that will help identify specific css input needed to let our webscraper know what information on the webpage we want scraped.  (Hat tip to [Andrew Cantino](https://vimeo.com/tectonic) for this lovely gem.)

```R
install.packages('rvest')
library('rvest')

WebScraper = function(url, css){
  webpage = read_html(url)
  htmlColumn = html_nodes(webpage, css)
  column = html_text(htmlColumn)
  return(column)
  }
  
```

We now have all we need to collect that dataframe from the website and create our own dataframe to use for analysis. 

```R
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

If you type in View(NFL_dataframe), or if you already took a peak at the website before reading this far, you should have noticed that in the "OUTCOME" column of the dataframe contained many entries with the following phrase "Resolution undetermined." For our purposes, we can't use them since we only care about outcomes that categorize players as either guilty or not guilty, so lets remove them. The following piece of code will accomplish this.

```R

NFL_dataframe = NFL_dataframe[grepl("Resolution undetermined", 
                                    NFL_dataframe$OUTCOME) == FALSE, ]
NFL_dataframe$GUILTY = NA

```

# Text Mining and Prediction: Naive Bayes Edition

Next we need to classify each remaining player within the database as either guilty or not guilty -- i.e. a value of 1 in the GUILTY column if guilty and 0 otherwise.

I *could* just go through each row and categorize each row a 1 or a 0 based on the description in the OUTCOME column, but that would take too long. I also don't want to. Instead, we'll use a homemade Naive Bayes R script to mine the text in the OUTCOME column and then predict whether or not to classify each player as guilty or not guilty. (Yes, I'm aware there are already packages in R with pre-built Naive Bayes functions for such purposes, but the point of this side excursion was to test how well I understood the Naive Bayes algorithm as well as its strengths and weaknesses. It also vastly improved my coding skills.) 

If you want to learn the theory behind Naive Bayes (NB) combined with Bag-of-Words (BoW) as a method of text classification, there are plenty of references out there, but I found that I thought were most useful were [here](https://www.youtube.com/watch?v=EGKeC2S44Rs) (for learning how to do it by hand), [here](https://web.stanford.edu/~jurafsky/slp3/slides/7_Sent.pdf) (as an overview of sentiment analysis), and [here](https://nlp.stanford.edu/IR-book/html/htmledition/the-bernoulli-model-1.html) (more into the weeds. You **will** have to read this reference because the pseudo-code outlined is ~90% of how my code is set up).  

We will (unfortunately) have to manually label some of the rows so that the algorithm can train and use the results to run predictions on the rest of the dataset. I did the first 100 rows.

## Required Packages

```R

library(dplyr)
library(tm)
library(tidytext)

```
dplyr, tm, and tidytext are all text mining packages. I'll explain what each one provides as we progress further into the algorithm.


## Defining the NB Function and Creating a BoW

Let's begin writing the algorithm. Define a function in R as "NaiveBayes" and make it require the following arguments: the portion of the data that's for training (i.e. the 100 labeled rows), the portion that's for predicting (i.e. the rest of the dataset), the name of the column in the dataset that contains the text to be mined and used in the NB algorithm for classification (i.e. the "OUTCOME" column), and the name of column where NB will classify the row with a 1 or a 0 (i.e. the "GUILTY" column).    

```R

NaiveBayes = function(trainData, testData, textColumn, outcomeColumn){

  train = trainData
  test = testData
  
  trainCorpus = VCorpus(VectorSource(train[, textColumn]))
  trainCorpus = trainCorpus %>% 
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeWords, stopwords(kind = "en")) %>%
    tm_map(stripWhitespace)
    
```

[VectorSource](https://www.rdocumentation.org/packages/tm/versions/0.7-6/topics/VectorSource), [VCorpus](https://www.rdocumentation.org/packages/tm/versions/0.7-6/topics/VCorpus), and [tm_map](https://www.rdocumentation.org/packages/tm/versions/0.7-6/topics/tm_map) are functions from the tm package. VectorSource will interpret each row of the OUTCOME column in the training data as a document, while VCorpus will give us a [text corpus](https://en.wikipedia.org/wiki/Text_corpus) that we will use to set up and clean our BoW for the NB algorithm. We then use tm_map to do the following transformations 

1. Remove any capital letters from words and replace them with lower-case letters. This is so when we count word frequencies in our documents we will avoid, for example, counting words like "The" and "the" as two separate word counts and instead count "the" as happening twice. 

2. Remove punctuation from the documents. Similar to previous point, but this time we're avoiding counting things like "the,", "the", and "the." as separate word counts and instead count "the" as happening 3 times.

3. Remove English stopwords. Stopwords are usually words in natural language processing that add no value to our analysis. For example, in the outcome "Dismissed by judge in Alabama." we don't really care about the words "by" and "in" because they don't help us in determining whether or not the player was found guilty, so they should just be removed.

4. Strip any whitespace that may have accidentally been fat-fingered in.

Now for our word counts, we'll set up a [document term matrix](https://en.wikipedia.org/wiki/Document-term_matrix).

```R  

  dtmTrain = DocumentTermMatrix(trainCorpus)
  
  freqTerms = findFreqTerms(dtmTrain, 5)
  
  dtmTrain = DocumentTermMatrix(trainCorpus, control =
                                   list(dictionary = freqTerms))
                                   
```

[DocumentTermMatrix](https://en.wikipedia.org/wiki/Document-term_matrix) does exactly what you'd expect. We also should remove any words that appear less than 5 times out of *all* our documents in our training data using the findFreqTerms function-- the idea being that words used only a few times out of all our data have little to nothing to contribute to our predictive analysis. 

At this point we have our nice and clean BoW for us to use, so now we can begin writing the actual NB algorithm!

## Training the data

First we'll need to acquire all the different "unique" words used within all the documents and a count of the total number of documents used (the "Why?" will become relevant soon, unless you already read my provided links regarding the Bernoulli model). 

```R

  vocab = Terms(dtmTrain)
  totDocs = nDocs(dtmTrain)
  
```
Next we need to find out what the prior probabilities for each class are -- i.e. if we knew nothing else about a player in the database, what are the odds he's guilty or not guilty? This is just a simple ratio of the number of guilty or non-guilty players in the database over the total number of players.

```R

nClass0 = nrow(train[train[ ,outcomeColumn] == 0, ])
nClass1 = nrow(train[train[ ,outcomeColumn] == 1, ])
priorProb0 = nClass0 / totDocs
priorProb1 = nClass1 / totDocs

```
Now we'll need to figure out how many documents in each separate category contain each of our unique words (e.g. the word "dropped" might appear in 2 documents amongst the guilty players and in 20 documents amongst the non-guilty players). 

```R

trainTibble = tidy(as.matrix(dtmTrain))
trainTibble$categ = train[,outcomeColumn]
nDocsPerTerm0 = colSums(trainTibble[trainTibble$categ == 0, ])
nDocsPerTerm1 = colSums(trainTibble[trainTibble$categ == 1, ])
nDocsPerTerm0 = nDocsPerTerm0[-length(nDocsPerTerm0)]
nDocsPerTerm1 = nDocsPerTerm1[-length(nDocsPerTerm1)] 

```  
(If you're wondering what the point of redefining "nDocsPerTerm1" and "nDocsPerTerm0", long story short, it's because initially those variables will also count the number of times the word "categ" appears in each of the documents separated by class. The last two commands drop it. I don't want to explain why and the reason is trivial.)

Great! Now we have all we need to find the conditional probabilities for each word in each of different categories of documents. Let's do that!

```R

termCondProb0 = (nDocsPerTerm0 + 1)/(nClass0 + 2)
termCondProb1 = (nDocsPerTerm1 + 1)/(nClass1 + 2)

```

Again, see [here](https://nlp.stanford.edu/IR-book/html/htmledition/the-bernoulli-model-1.html) to understand the formula above. 

## Testing the Data

We're almost done! First we need to clean the test data, which is the same procedure as what we did for the training data, as well as and grab all the unique vocabulary. 

```R  

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
 
vocabTest = Terms(dtmTest)

```
Now the actual classification begins! Below is the code that predicts whether or not each row player in the test data should be classified as guilty or not based on their "OUTCOME" description. 

```R

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

  
  return(classifiedRows)

```

Kinda sorta make sense?

Essentially all we're doing is creating an empty vector for the classified results, creating a for-loop that computes the posterior...

...to be continued
