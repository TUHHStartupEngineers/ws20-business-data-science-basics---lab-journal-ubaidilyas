#1.0 Loading Libraries ----

library(vroom)
library(tidyverse)
library(data.table)

#2.0 Generating Data and Converting to DT ----

#2.1 For patent ----

col_types_patent <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)

patent_tb <- vroom(
  file       = "patent.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent,
  na         = c("", "NA", "NULL")
)%>% setDT() 

#2.2 For assignee----

col_types_assignee <- list(
  id = col_character(),
  type = col_integer(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
)

assignee_dt <- vroom(
  file       = "assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_assignee,
  na         = c("", "NA", "NULL")
) %>% setDT()


#2.3 For patent_assignee----

col_types_patent_assignee <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
)

patent_assignee_dt <- vroom(
  file       = "patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent_assignee,
  na         = c("", "NA", "NULL")
) %>% setDT()

#2.4 For uspc----

col_types_uspc <- list(
  uuid = col_character(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_character(),
  sequence = col_integer()
  
)

uspc_tb <- vroom(
  file       = "uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types_uspc,
  na         = c("", "NA", "NULL")
) %>% setDT() 


#3.0 Challenge No.1 ----

#Renaming and Merging
setnames(assignee_dt,"id","assignee_id")
combined_data_c1 <- merge(x = assignee_dt, y = patent_assignee_dt, 
                       by    = "assignee_id", 
                       all.x = TRUE, 
                       all.y = FALSE)
#Data Extraction
combined_data_c1 [type == 2 & !is.na(organization), .N, by = organization][
  , max(N), by = organization][
    order(V1, decreasing = TRUE)] %>% 
  head(10)

#4.0 Challenge No.2 ----

#Renaming and Merging
patent_2019_tb<- patent_tb[ lubridate::year(date) == "2019"]

setnames(patent_2019_tb,"id","patent_id")

combined_data_c2 <- merge(x = combined_data_c1, y = patent_2019_tb, 
                          by    = "patent_id", 
                          all.x = TRUE, 
                          all.y = FALSE)

#Data Extraction
combined_data_c2 [lubridate::year(date) == "2019" & !is.na(organization), .N, by = organization][
  , max(N), by = organization][
    order(V1, decreasing = TRUE)] %>% 
  head(10)

#5.0 Challenge No.3 ----

#Renaming and Merging
memory.limit(size = 150000) #due to large vector size

combined_data_c3<-merge(x = combined_data_c1, y = uspc_tb, 
      by    = "patent_id", 
      all.x = TRUE, 
      all.y = FALSE)

#Data Extraction
combined_data_c3 [!is.na(patent_id) & !is.na(mainclass_id), .N, by = mainclass_id][
  , max(N), by = mainclass_id][
    order(V1, decreasing = TRUE)] %>% 
  head(5)