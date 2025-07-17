setwd("O:/Alaska/Depts/Kenai/OptEng/drt/Well_Integrity/AOGCC/Alaska AOGCC")
source('AOGCC_orders_scrape.R')

if(exists('aogcc_rules')) {
  print('aogcc_rules already exist!')
} else {
  aogcc_rules = aogcc_orders_scrape()
}


aogcc_rules_stats <- function(rls){
  
    name_number <- rls |>
      group_by(Name) |>
      mutate(
        # break 1
        Name = as.character(Name),
        Index1 = gregexpr(Name, pattern = ' ')[[1]][1],
        Number = substr(trimws(Name), Index1 + 1, nchar(Name)),
        # break 2
        Index2 = gregexpr(Number, pattern = ' ')[[1]][1],
        Number = ifelse(
          Index2 == -1, 
          Number, 
          substr(Number, 1, Index2 - 1) # rtn number
          ),
        Amend = ifelse(
          Index2 == -1,
          "",
          substr(Name,
                 gregexpr(Name, pattern = ' ')[[1]][2] + 1,
                 nchar(Name)
                 )
        ),
        # break 3
        Index3 = gregexpr(Amend, pattern = ' ')[[1]][1],
        Amend_Num = ifelse(
          Index3 == -1,
          "",
          substr(Amend, 
                 gregexpr(Name, pattern = ' ')[[1]][1] + 1,
                 nchar(Name))
        ),
        Amend = ifelse(
          Index3 == -1, 
          Amend, 
          substr(Amend, 1, Index3 - 1) # rtn number
        ),
        Amend_Num = stringr::str_remove(Amend_Num, "^0+"),
        Amend = stringr::str_remove(Amend, "\\."),
        Amend = stringr::str_remove(Amend, "^0+")
      ) |>
      select(Name, Type, Number, Amend, Amend_Num)
}
