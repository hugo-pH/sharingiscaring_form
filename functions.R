# Function to catch errors in a given submit action
catchy <- function(submit_type, f, args){
  
  # User-experience stuff
  shinyjs::disable(submit_type)
  shinyjs::show("submit_msg")
  shinyjs::hide("error")
  
  # Save the data (show an error message in case of error)
  tryCatch({
    f(args)
    # update.current.orders(orderformData())
    shinyjs::reset("form")
    shinyjs::hide("form")
    shinyjs::show("thankyou_msg")
  },
  error = function(err) {
    shinyjs::html("error_msg", err$message)
    shinyjs::show(id = "error", anim = TRUE, animType = "fade")
  },
  finally = {
    shinyjs::enable(submit_type)
    shinyjs::hide("submit_msg")
  })
}







# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}


# load.orders.data  <- function(con, orders_table) {
#   # browser()
#   d_orders <- tbl(con, orders_table) %>% as_data_frame()
#   return(d_orders)
# }
# 
# 

pull.data.from.db <- function(sqlite_path, db_table){
  pool <- dbPool(RSQLite::SQLite(), dbname = sqlite_path)
  d_db <- tbl(pool, db_table) %>% as_data_frame()
  poolClose(pool)
  return(d_db)
}

# load.purchase.data <- function(con, purchases_table) {
#   d_purchases <- tbl(con, purchases_table) %>% as_data_frame()
#   return(d_purchases)
# }



update.current.orders <- function(arg.list) {
  # db_data <- pull.data.from.db(sqlite_path = arg.list$sqlite_path,
                              # db_table = arg.list$orders_table)
  
  pool <- dbPool(RSQLite::SQLite(), dbname = arg.list$sqlite_path)
  # db_insert_into(pool, arg.list$orders_table, arg.list$data %>% mutate(idx = new_idx) , temporary = F)
  db_insert_into(pool, arg.list$orders_table, arg.list$data, temporary = F)
  
  
  if(!(arg.list$new_common_item)){
    new_common_item_df <- arg.list$data %>% 
      select(item) 
    new_common_item <- new_common_item_df %>% pull(item)
    
    db_c_items <- pull.data.from.db(sqlite_path = arg.list$sqlite_path,
                                    db_table = arg.list$common_item_table) %>% pull(item)
    
    if (new_common_item %in% db_c_items){
      pool <- dbPool(RSQLite::SQLite(), dbname = arg.list$sqlite_path)
      db_insert_into(pool, arg.list$commom_item_table, new_common_item_df, temporary = F)  
    }
  }
  poolClose(pool)
}

update.purchase.answers <- function(arg.list) {
  browser()
  ## add item to purchases table
  pool <- dbPool(RSQLite::SQLite(), dbname = arg.list$sqlite_path)
  db_insert_into(pool, arg.list$purchases_table, arg.list$data, temporary = F)
  
  # If item was in orders list, update "purchased" state to true
  if(arg.list$ordered_item){
    item <-  arg.list$data %>% pull(item)
    query<- dbplyr::build_sql("UPDATE orders SET purchased =" , " 'True'", " WHERE item = ", item)
    conn <-  poolCheckout(pool)
    pool::dbSendQuery( conn, query)
    # poolClose(pool)
  }
  if(!(arg.list$new_common_item)){
    new_common_item_df <- arg.list$data %>% 
      select(item) 
    new_common_item <- new_common_item_df %>% pull(item)
    
    db_c_items <- pull.data.from.db(sqlite_path = arg.list$sqlite_path,
                     db_table = arg.list$common_item_table) %>% pull(item)
    
    if (new_common_item %in% db_c_items){
      pool <- dbPool(RSQLite::SQLite(), dbname = arg.list$sqlite_path)
      db_insert_into(pool, arg.list$commom_item_table, new_common_item_df, temporary = F)  
    }
    

  }
  poolClose(pool)
}



remove.entry <- function(arg.list) {
  # browser()
  pool <- dbPool(RSQLite::SQLite(), dbname = arg.list$sqlite_path)
  query <- dbplyr::build_sql("DELETE FROM ", sql(arg.list$table), " WHERE timestamp = " ,arg.list$id)
  conn <-  poolCheckout(pool)
  pool::dbSendQuery( conn, query)
  poolClose(pool)

}



render.tables <- function(sqlite_path, table){
  
  df <- pull.data.from.db(sqlite_path, table) %>% 
    mutate(timestamp = ymd_hms(timestamp)) %>%
    arrange(desc(timestamp)) %>% 
    select(-year, -month, -day) %>% 
    mutate(timestamp = as.character(timestamp)) 
  if(table == "orders"){
    df <- filter(df, purchased == "False")
  }
  DT::renderDataTable(
      d_orders <- df,
    rownames = FALSE,
    options = list(searching = FALSE, lengthChange = FALSE)
  )
}

