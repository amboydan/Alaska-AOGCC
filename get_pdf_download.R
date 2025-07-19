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


aogcc_orders_downloads <- function(rls) {
  
    for(i in 1:nrow(rls)) {
      
      # special case: duplicate orders
      if(grepl("duplicate",tolower(rls[i, "Name"][[1]]))) next
  
      # set the desired download location
      eCaps <- list(
        chromeOptions = list(
          prefs = list(
            "profile.default_content_settings.popups" = 0L, # Disable pop-ups
            "download.prompt_for_download" = FALSE,       # Don't ask where to save each file
            "plugins.always_open_pdf_externally" = TRUE,
            "download.default_directory" = normalizePath("O:/Alaska/Depts/Kenai/OptEng/drt/Well_Integrity/AOGCC/orders") # Set the desired download directory
          )
        )
      )
      
      rs_driver_object <- rsDriver(browser = 'chrome',
                                   chromever = '137.0.7151.55',
                                   phantomver = NULL,
                                   version = 'latest',
                                   port = netstat::free_port(random = T),
                                   extraCapabilities = eCaps)
      
      
      remDr <- rs_driver_object$client
      
      # remDr$open()
      href <- as.character(rls[i, 'href'])
      remDr$navigate(href)
      Sys.sleep(0.5)
      
      # Login Button
      login_button <- remDr$findElement(
        using = 'xpath', 
        '//input[@name="LoginButton"]')
      login_button$clickElement()
      Sys.sleep(0.5)
      
      # get document length
      pgCount_element <- remDr$findElements(
        using = 'class',
        'PageNumberToolbarCount'
      )

      if(length(pgCount_element) == 0) {
        pgCount_text = 1
      } else {
        pgCount_text = pgCount_element[[1]]$getElementText()[[1]]
        # if the page count is over 300 then come back 
        if(pgCount_text > 299) {
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
          next
          }
      }

      # PDF download to file
      pdf_download_button <- remDr$findElement(
        using = 'xpath', 
        '//a[@id="PdfDialog_PdfDownloadLink"]')
      pdf_download_button$clickElement()
      Sys.sleep(0.5)
      
      pdf_download_to_file_button <- remDr$findElement(
        using = 'xpath', 
        '//input[@id="PdfDialog_download"]')
      pdf_download_to_file_button$clickElement()
      Sys.sleep(30)
      
      print(paste0(i, " out of ", nrow(rls), " complete!"))

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
        
    }
    return(df)
}

aogcc_orders_downloads(aogcc_rules)