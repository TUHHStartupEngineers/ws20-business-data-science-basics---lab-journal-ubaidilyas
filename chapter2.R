# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(lubridate)
library(readxl)
# 2.0 Importing Files ----
bikes_tbl <- read_excel("/Users/UBIL1/Desktop/TUHH/business/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("/Users/UBIL1/Desktop/TUHH/business/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("/Users/UBIL1/Desktop/TUHH/business/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 4.0 Joining Data ----
left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))
# 5.0 Wrangling Data ----
bike_orderlines_joined_tbl %>% 
  select(category) %>%
  filter(str_detect(category, "^Mountain")) %>% 
  unique()
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  mutate(total.price = price * quantity) %>%
  select(-...1, -gender) %>%
  select(-ends_with(".id")) %>%
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

#6.0 Chapter 2 Challenges ----

#6.1 Sales by State ----
sales_by_state_tbl <- bike_orderlines_wrangled_tbl %>%
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ", ") %>%
  group_by(state) %>% 
  summarise(sales = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))


sales_by_state_tbl%>%
  ggplot(aes(x = state, y = sales)) +
  geom_col(fill = "#2DC6D6") + 
  geom_label(aes(label = sales_text)) + 
  geom_smooth(method = "lm", se = FALSE) +
 scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title    = "Revenue by State",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

#6.2 Sales by Year and State ----
sales_by_year_location_tbl<- bike_orderlines_wrangled_tbl %>%
  select(order_date, total_price, location) %>%
  mutate(year = year(order_date)) %>%
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ", ") %>%  
  group_by(year, state) %>%
  summarise(sales = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
sales_by_year_location_tbl %>%
  ggplot(aes(x = year, y = sales, fill = state)) +
  geom_col() + 
  geom_smooth(method = "lm", se = FALSE) +  
  facet_wrap(~ state) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and location",
    subtitle = "Each product category has an upward trend",
    fill = "State" # Changes the legend name
  )
