library(RSQLite)
library(DBI)
library(dplyr)
library(httr)
library(jsonlite)
library(glue)
library(rvest)
library(stringr)
library(purrr)
con <- RSQLite::dbConnect(drv    = SQLite(), 
                          dbname = "/Users/UBIL1/Desktop/TUHH/business/00_data/02_chinook/Chinook_Sqlite.sqlite")
dbListTables(con)
tbl(con, "Album")
album_tbl <- tbl(con, "Album") %>% collect()
x <- dbGetQuery(con, 'SELECT * FROM Artist')
dbDisconnect(con)


####

library(glue)
name <- "Fred"
glue('My name is {name}.')

####

resp

rawToChar(resp$content) %>%
  fromJSON()


resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()


#If you want it to be parsed into a list, set it to “parsed”. If you leave it empty, it will automatically try to detect the type and parse it into a list:
content(resp, as = "text")
content(resp, as = "parsed")
content(resp)



token    <- "7TEP1LGPLXMZRU5X"
response <- GET(glue("https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE&apikey={token}"))
response

alphavantage_api_url <- "https://www.alphavantage.co/query"
ticker               <- "WDI.DE"
# You can pass all query parameters as a list to the query argument of GET()
GET(alphavantage_api_url, query = list('function' = "GLOBAL_QUOTE",
                                       symbol     = ticker,
                                       apikey     = Sys.getenv('TOKEN'))
)



###########

# get the URL for the wikipedia page with all S&P 500 symbols
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
# use that URL to scrape the S&P 500 table using rvest
library(rvest)
sp_500 <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # Get the nodes with the id
  html_nodes(css = "#constituents") %>%
  # html_nodes(xpath = "//*[@id='constituents']"") %>% 
  # Extract the table and turn the list into a tibble
  html_table() %>% 
  .[[1]] %>% 
  as_tibble()


#2nd example: Get the 250 top rated movies from IMDB

url  <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
html <- url %>% 
  read_html()


#Get the ranks:
  
  rank <-  html %>% 
  html_nodes(css = ".titleColumn") %>% 
  html_text() %>% 
  # Extrag all digits between " " and ".\n" The "\" have to be escaped
  # You can use Look ahead "<=" and Look behind "?=" for this
  stringr::str_extract("(?<= )[0-9]*(?=\\.\\n)")%>% 
  # Make all values numeric
  as.numeric()
  
  
#Get the title: The title is in the child node with an a tag:
    
    title <- html %>% 
    html_nodes(".titleColumn > a") %>% 
    html_text()
    
    
#Get the year: The year has the classes .titleColumn AND .secondaryInfo. Separate those by a space.
    
    year <- html %>% 
      html_nodes(".titleColumn .secondaryInfo") %>%
      html_text() %>% 
      # Extract numbers
      stringr::str_extract(pattern = "[0-9]+") %>% 
      as.numeric()
    
#Get the people: The people are in the attribute title
    
    people <- html %>% 
      html_nodes(".titleColumn > a") %>% 
      html_attr("title")
    
#Get the ratings:
      
      rating <- html %>% 
      html_nodes(css = ".imdbRating > strong") %>% 
      html_text() %>% 
      as.numeric()

#the number of ratings:
      
      num_ratings <- html %>% 
        html_nodes(css = ".imdbRating > strong") %>% 
        html_attr('title') %>% 
        # Extract the numbers and remove the comma to make it numeric values
        stringr::str_extract("(?<=based on ).*(?=\ user ratings)" ) %>% 
        stringr::str_replace_all(pattern = ",", replacement = "") %>% 
        as.numeric()
      
#Merge everything into a tibble:
        
        imdb_tbl <- tibble(rank, title, year, people, rating, num_ratings)
        
# Language
        resp <- GET(url = "https://www.imdb.com/chart/top/?ref_=nv_mv_250",  
                    add_headers('Accept-Language' = "en-US, en;q=0.5")) 
        html <- content(resp)
        
        
        
#For Loop
        numbers <- c(1:5)
        for (i in numbers) {
          print(i)
        }
        
bike_data_lst[["productDetail"]][["variationAttributes"]][["values"]][[1]][["displayValue"]]


bike_data_lst %>%
  purrr::pluck("productDetail", "variationAttributes", "values", 1, "displayValue")
