setwd("O:/Alaska/Depts/Kenai/OptEng/drt/Well_Integrity/AOGCC/Alaska AOGCC")
source('AOGCC_orders_scrape.R')

if(exists('aogcc_rules')) {
  print('aogcc_rules already exist!')
} else {
  aogcc_rules = aogcc_orders_scrape()
}


aogcc_rules_stats <- function(rls){
    # Parse out the Order Number ---- 
    name_number <- rls |>
      group_by(Name) |>
      mutate(
        # break 1
        Name = as.character(Name),
        Index1 = gregexpr(Name, pattern = ' ')[[1]][1],
        Order_Num = substr(trimws(Name), Index1 + 1, nchar(Name)),
        # break 2
        Index2 = gregexpr(Order_Num, pattern = ' ')[[1]][1],
        Order_Num = ifelse(
          Index2 == -1, 
          Order_Num, 
          substr(Order_Num, 1, Index2 - 1) # rtn Order_Num
          ),
        Order_Amendment = ifelse(
          Index2 == -1,
          "",
          substr(Name,
                 gregexpr(Name, pattern = ' ')[[1]][2] + 1,
                 nchar(Name)
                 )
        ),
        # break 3
        Index3 = gregexpr(Order_Amendment, pattern = ' ')[[1]][1],
        Order_Amendment_Num = ifelse(
          Index3 == -1,
          "",
          substr(Order_Amendment, 
                 gregexpr(Name, pattern = ' ')[[1]][1] + 1,
                 nchar(Name))
        ),
        Order_Amendment = ifelse(
          Index3 == -1, 
          Order_Amendment, 
          substr(Order_Amendment, 1, Index3 - 1) # rtn Order_Num
        ),
        Order_Amendment_Num = stringr::str_remove(Order_Amendment_Num, "^0+"),
        Order_Amendment = stringr::str_remove(Order_Amendment, "\\."),
        Order_Amendment = stringr::str_remove(Order_Amendment, "^0+")
      ) |>
      select(-Index1, -Index2, -Index3)
    
    # Get the word count ----
    # stats ----
    
    return(name_number)
}
