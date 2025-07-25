setwd("O:/Alaska/Depts/Kenai/OptEng/drt/Well_Integrity/AOGCC/Alaska AOGCC")
source('AOGCC_orders_scrape.R')

if(exists('aogcc_rules')) {
  print('aogcc_rules already exist!')
} else {
  aogcc_rules = aogcc_orders_scrape()
}

# get the order_pages.csv if it exists
order_pages <- read.csv('order_pages.csv', header = T)

aogcc_rules_index <- function(rls) {
    # Parse out the Order Number ---- 
    name_number <- rls |>
      select(Name) |>
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
      ungroup() |>
      select(-Name,-Index1, -Index2, -Index3)
    
    # Get the word count ----
    # stats ----
    
    return(name_number)
}
aogcc_rules_counts <- function(rls) {
  
  pool_counts <- rls |>
    select(Name, Fields, Pools) |>
    mutate(
      # Identify if Rule applies to Oil or Gas or Both
      # A comma delimits btw pools (n + 1 = # pools)
      Pool_Count_0 = ifelse(nchar(Pools) == 0, T, F),
      Pool_Count = ifelse(Pool_Count_0 == F,
                          (stringr::str_count(Pools, pattern =',')) + 1,
                          0),
      Oil_Pool_Count = (stringr::str_count(Pools, pattern ='OIL')),
      Gas_Pool_Count = (stringr::str_count(Pools, pattern ='GAS')),
      # Identify if Rule applies to Different Fields
      Field_Count_0 = ifelse(nchar(Fields) == 0, T, F),
      Field_Count = ifelse(Field_Count_0 == F,
                          (stringr::str_count(Fields, pattern =',')) + 1,
                          0)
    ) |>
    ungroup() |>
    select(-Name, -Fields, -Pools)
  
    return(pool_counts)  
}

join_rules_df <- function(rls) {
  index <- aogcc_rules_index(rls)
  counts <- aogcc_rules_counts(rls)
  
  combine_rules_df <- cbind(rls, index, counts)
  
  return(combine_rules_df)
}

