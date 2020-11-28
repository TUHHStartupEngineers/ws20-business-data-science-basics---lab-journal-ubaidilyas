library(vroom)
library(tidyverse)
library(data.table)
### Generating data for ----
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
) 

patent_tb_required <- patent_tb %>% select(id, number, date, title)

### Generating data for assignee----
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

assignee_tb <- vroom(
  file       = "assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_assignee,
  na         = c("", "NA", "NULL")
)
### Generating data for patent_assignee----

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

patent_assignee_tb <- vroom(
  file       = "patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent_assignee,
  na         = c("", "NA", "NULL")
) 
### Generating data for uspc----

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
) 


### Challenge No.1 ----
setnames(assignee_dt,"id","assignee_id")
combined_data <- merge(x = assignee_dt, y = patent_assignee_dt, 
                       by    = "assignee_id", 
                       all.x = TRUE, 
                       all.y = FALSE)

challenge_1<- combined_data [type == 2 & !is.na(organization), .N, by = organization][
  , max(N), by = organization][
    order(V1, decreasing = TRUE)] %>% 
  head(10)

### Challenge No.2 ----
patent_tb_required <- patent_tb %>% select(id, number, date, title)

patent_arranged_tb<-patent_tb %>% select(id, number, date, title) %>%
  mutate() %>%
  separate("date", c("year", "month", "day"), sep = "-") 

a<-d%>% mutate()%>%separate("date", c("Year", "Month", "Day"), sep = "-")

patent_tb_required %>% summarise(across(everything(), ~sum(is.na(.))))

smaller_table <- 
  patent_tb %>% sample_n(6000000) 

patent_tb_required <- smaller_table %>% select(id, number, date, title)

setDT(patent_tb_required)
ilyas<- patent_assignee_dt%>% group_by(location_id)



us_company_dt %>%
  summarise(across(everything(), ~sum(is.na(.))))
patent_assignee_tbl %>%
  summarise(across(everything(), ~sum(is.na(.))))

assignee_tbl[, .N, by="id"]

assignee_tbl_dt[type == 2]


most_patents_tbl<- assignee_tbl %>%
  
  # By argument not necessary, because both tibbles share the same column names
  left_join(y = patent_assignee_tbl, by = c("order_id" = "order_id"))