library('rvest')

NFL_WebScraper = function(url, css){
  webpage = read_html(url)
  html_column = html_nodes(webpage, css)
  column = html_text(html_column)
  return(column)
  }

website = 'https://www.usatoday.com/sports/nfl/arrests/'

NAMES = NFL_WebScraper(website, '.arrest-name')
POS = NFL_WebScraper(website, '.arrest-narrow:nth-child(4)')
TEAM = NFL_WebScraper(website, '.arrest-narrow:nth-child(2)')
CASE = NFL_WebScraper(website, '.arrest-midsize:nth-child(5)')
CATEGORY = NFL_WebScraper(website, '.arrest-midsize:nth-child(6)')
DESCRIPTION = NFL_WebScraper(website, '.left:nth-child(7)')
OUTCOME = NFL_WebScraper(website, '.outcome')

NFL_Dataframe = data.frame("NAMES" = NAMES, "POS" = POS, "TEAM" = TEAM,
                           "CASE" = CASE, "CATEGORY" = CATEGORY, 
                           'DESCRIPTION' = DESCRIPTION, 
                           'OUTCOME' = OUTCOME, stringsAsFactors = FALSE)

#This will removed all the uncertain outcomes
NFL_Dataframe = NFL_Dataframe[grepl("Resolution undetermined", 
                                    NFL_Dataframe$OUTCOME) == FALSE, ]
NFL_Dataframe$GUILTY = NA


