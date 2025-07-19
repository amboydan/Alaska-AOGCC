setwd("O:/Alaska/Depts/Kenai/OptEng/drt/Well_Integrity/AOGCC/Alaska AOGCC")
source('AOGCC_orders_scrape.R')

if(exists('aogcc_rules')) {
  print('aogcc_rules already exist!')
} else {
  aogcc_rules = aogcc_orders_scrape()
}

library(tidyverse)
library(RSelenium)
library(rvest)
library(netstat)
library(data.table)


aogcc_rules_page_count <- function(rls) {
    orders_list <- list()
    for(i in 1:nrow(rls)) {
      
      # special case: duplicate orders
      if(grepl("duplicate",tolower(rls[i, "Name"][[1]]))) {
        df <- data.frame(
          Name = rls[i,'Name'],
          Start_Page = 0,
          Stop_Page = 0
        )
        check_list[[i]] <- df
        next
      }
      
      if(i == 1) {
        rs_driver_object <- rsDriver(browser = 'chrome',
                                     chromever = '137.0.7151.55',
                                     phantomver = NULL,
                                     version = 'latest',
                                     port = netstat::free_port(random = T))
        
        
        remDr <- rs_driver_object$client
      }
      
      # remDr$open()
      href <- as.character(rls[i, 'href'])
      remDr$navigate(href)
      Sys.sleep(0.5)
      
      if(i == 1){
        # Login Button
        login_button <- remDr$findElement(
          using = 'xpath', 
          '//input[@name="LoginButton"]')
        login_button$clickElement()
        Sys.sleep(0.5)
      } 
      
      # get starting page
      pgStart_element <- remDr$findElements(
        using = 'class',
        'PageNumberToolbarInput'
      )
      if(length(pgStart_element) == 0) {
        pgStart_text = 1
      } else {
        pgStart_text = pgStart_element[[1]]$getElementAttribute('value')[[1]]
      }
      pgStart_element
      
      # get document length
      pgCount_element <- remDr$findElements(
        using = 'class',
        'PageNumberToolbarCount'
      )
      if(length(pgCount_element) == 0) {
        pgCount_text = 1
      } else {
        pgCount_text = pgCount_element[[1]]$getElementText()[[1]]
      }
      
      
      # build row
      df <- data.frame(
        Name = rls[i,'Name'],
        Start_Page = pgStart_text,
        Stop_Page = pgCount_text
      )
      
      print(paste0(i, " out of ", nrow(rls), " complete!"))
      orders_list[[i]] <- df
      
      if(i == nrow(rls)) {
        # Log out of AOGCC
        logout_button <- remDr$findElement(
          using = 'xpath', 
          '//a[@id="LogoffLink"]')
        logout_button$clickElement()
        Sys.sleep(1)
        
        # Close the port
        remDr$close()
        
        # stop the Selenium server
        rs_driver_object$server$stop()
        
        # combine and export orders
        df = do.call(rbind, orders_list)
      }
      
      
    }
    return(df)
}

orders_pages <- aogcc_rules_page_count(aogcc_rules)