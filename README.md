# NFL Arrests Database

# Introduction

This will be an introduction. 

# Data Collection and Cleaning

Obviously, the first thing we need to do is collect and clean the data. Luckily, since this project involves the exploitation of an already clean and organized dataframe, this part is easy. Since we can't download the data directly, and copy & paste for each row is too tedious, we'll use a homemade web scraper!  

Before we build our web scraping function, we'll need to install the package 'rvest' so that we can take advantage of some handy functions (e.g. read_html, html_nodes, etc.) that will make life easier.

```R
install.packages('rvest')
library('rvest')

```
Now we can create our web scraping function "WebScraper".

```R

WebScraper = function(url, css){
  webpage = read_html(url)
  htmlColumn = html_nodes(webpage, css)
  column = html_text(htmlColumn)
  return(column)
  }

```
Thanks to rvest, the function should seem pretty straighforward -- WebScraper takes as input the uniform resource locator (url) web address and the web page's cascading style sheets (css) related to the data columns of interest as input, then it returns the column in an R friendly format. 

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

If type in View(NFL_dataframe), or if you took a peak at the website before reading this far, you should have noticed that in the "OUTCOME" column of the dataframe contained many entries with the following phrase "Resolution undetermined." For our purposes, we can't use them since we only care about outcomes that categorize players as either guilty or not guilty, so lets remove them.

```R

NFL_dataframe = NFL_dataframe[grepl("Resolution undetermined", 
                                    NFL_dataframe$OUTCOME) == FALSE, ]
NFL_dataframe$GUILTY = NA

```
grepl is a basic [to be continued]
