# Open selenium chromedriver.exe first.
# C:\Users\dtaylor\AppData\Local\binman\binman_chromedriver\win32\137.0.7151.55
# port => no port number required. 
# 1) Start chromedriver.exe of same version as chrome browser
# 2) may have to shut down open ports
#   -- identify port and then shut down
# netstat -ano | findstr :4567
# taskkill /pid <what you found> /F 

#setwd("O:/Alaska/Depts/Kenai/OptEng/drt/Well_Integrity/AOGCC Orders Scrape")

library(tidyverse)
library(RSelenium)
library(rvest)
library(netstat)
library(data.table)

aogcc_orders_scrape <- function(){
rs_driver_object <- rsDriver(browser = 'chrome',
                             chromever = '137.0.7151.55',
                             phantomver = NULL,
                             version = 'latest',
                             port = netstat::free_port(random = T))


remDr <- rs_driver_object$client
# remDr$open()
remDr$navigate('http://aogweb.state.ak.us/WebLinkSearch')

# All AOGCC Orders
all_button <- remDr$findElement(
  using = 'xpath', 
  '//button[@name="ALL"]')
all_button$clickElement()
Sys.sleep(2)

# Show All Entries
length_button <- remDr$findElement(
  using = 'xpath', 
  '//*[@id="data-table_length"]/label/select/option[4]')
length_button$highlightElement()
length_button$clickElement()
Sys.sleep(2)

# Identify Table Data
all_orders <- remDr$findElement(
  using = 'id', 
  'data-table')
data_table_html <- all_orders$getPageSource()
page <- read_html(data_table_html %>% unlist())
df <- html_table(page)[[2]]

# Get the order reference
href <- read_html(data_table_html |> unlist()) |> 
  html_nodes('tr') |> 
  html_nodes('a') |> 
  html_attr('href')

# Close the port
remDr$close()

# stop the Selenium server
rs_driver_object$server$stop()

df[['href']] <- href

names(df) <- c('Name', 'Active', 'Date', 
               'Field_Pools', 'Operator', 
               'Description', 'href')

df <- df |> 
  #filter(grepl('Hilcorp', Operator)) |> 
  mutate(
    Name = factor(Name),
    Type = factor(gsub(' .*', '', Name)),
    Date = factor(as.Date(Date, '%Y-%m-%d')),
    Operator = factor(Operator),
    Active = factor(ifelse(Active == '✓', T, F)),
    Fields = gsub('\\Pools.*','', Field_Pools),
    Fields = trimws(gsub('Fields:', '', Fields)),
    Pools = gsub('.*Pools:', '', Field_Pools),
    Pools = trimws(gsub('Pools:', '', Pools))
  ) |> 
  filter(!grepl('duplicate', tolower(Name))) |>
  ungroup() |> 
  select(-Field_Pools)

return(df)
}


