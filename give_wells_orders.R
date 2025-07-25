setwd("O:/Alaska/Depts/Kenai/OptEng/drt/Well_Integrity/AOGCC/Alaska AOGCC")

if(exists('aogcc_parsed')) {
  print('aogcc_parsed already exist!')
} else {
  source('order_stats.R')
  aogcc_parsed <- join_rules_df(aogcc_rules)
  # join with page numbers if worth it
  aogcc_parsed <- plyr::join(aogcc_parsed, order_pages, by = 'Name') |>
    filter(Active == T)
}

# Get the well data
get_wells_data <- function() {
  wells_data <- read.csv('aogcc_wells.csv', header = T)
  wells_data <- wells_data |> 
    select(Permit, Api, WellName, AreaName, 
           FieldName, PoolNames, CurrentClass,
           CurrentStatus, SpudDate, OperatorName)
  orders <- data.frame(Orders = rep(" ", times = nrow(wells_data)))
  
  aogcc_pools <- aogcc_parsed$Pools
  aogcc_fields <- aogcc_parsed$Fields
  aogcc_active <- aogcc_parsed$Active
  
  for(well in 1:nrow(wells_data)) {
    # wells_data => what we have
    fields = str_split(wells_data[well, 'FieldName'], ', ')[[1]]
    pools = str_split(wells_data[well, 'PoolNames'], ', ')[[1]]
    
    # what we are scanning through
    old_order <- c()
    new_order <- ""
    for(j in 1:length(aogcc_fields)) {
      x <- which(fields %in% aogcc_fields[j])
      y <- which(pools %in% aogcc_pools[j])

      if(!is_empty(x) & !is_empty(y)) {
        new_order <- paste(
          aogcc_parsed[j, "Type"], 
          aogcc_parsed[j, 'Order_Num'],
          aogcc_parsed[j, 'Order_Amendment'], 
          sep = ' ')
        if(!new_order %in% old_order) {
          new_order = c(old_order, new_order)
          old_order = new_order
        }
      } 
    }
    orders[well,"Orders"] <- paste(trimws(new_order), collapse = ', ')
  }
  
  orders_spread <- data.frame(
    AEO = rep("", times = nrow(orders)),
    AIO = rep("", times = nrow(orders)),
    CO = rep("", times = nrow(orders)),
    DIO = rep("", times = nrow(orders)),
    ERIO = rep("", times = nrow(orders)),
    O = rep("", times = nrow(orders)),
    SIO = rep("", times = nrow(orders))
  )
  
  
  for(i in 1:nrow(orders_spread)) {
    well <- orders[i, "Orders"]
    splts <- strsplit(well, ', ')[[1]]
    if(length(splts) == 0) next
    for(j in 1:length(splts)) {
      if(!is_empty(splts[j])){
        splt2 <- strsplit(splts, ' ')[[j]][1]
        if(splt2 == 'AEO') {
          a <- orders_spread[i,'AEO']
          b <- splts[j]
          orders_spread[i,'AEO'] = gsub("^, ", "", paste(a, b, sep = ', '))
        }
        if(splt2 == 'AIO') {
          a <- orders_spread[i,'AIO']
          b <- splts[j]
          orders_spread[i,'AIO'] = gsub("^, ", "", paste(a, b, sep = ', '))
        }
        if(splt2 == 'CO') {
          a <- orders_spread[i,'CO']
          b <- splts[j]
          orders_spread[i,'CO'] = gsub("^, ", "", paste(a, b, sep = ', '))
        }
        if(splt2 == 'DIO') {
          a <- orders_spread[i,'DIO']
          b <- splts[j]
          orders_spread[i,'DIO'] = gsub("^, ", "", paste(a, b, sep = ', '))
        } 
        if(splt2 == 'ERIO') {
          a <- orders_spread[i,'ERIO']
          b <- splts[j]
          orders_spread[i,'ERIO'] = gsub("^, ", "", paste(a, b, sep = ', '))
        }
        if(splt2 == 'O') {
          a <- orders_spread[i,'O']
          b <- splts[j]
          orders_spread[i,'O'] = gsub("^, ", "", paste(a, b, sep = ', '))
        }
        if(splt2 == 'SIO') {
          a <- orders_spread[i,'SIO']
          b <- splts[j]
          orders_spread[i,'SIO'] = gsub("^, ", "", paste(a, b, sep = ', '))
        }

      }
    }
  }
  
  wells_data <- cbind(wells_data, orders_spread)
  
  return(wells_data)
}

check <- get_wells_data()


write.csv(check, 'wells_+_orders.csv', row.names = F)
