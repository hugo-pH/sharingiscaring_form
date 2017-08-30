# server needs_app_db
library(shiny)
library(shinythemes)
library(lubridate)
library(tidyverse)
library(stringr)
library(wesanderson)
library(ggthemes)
library(pool)
library(shinyjs)
library(shinyURL)
theme_set(theme_bw(20))
source("functions.R")

paths <- read_csv("./paths.csv",
                  col_types = cols(file = col_character(),
                                   path = col_character()))

sqlite_path <- paths %>% filter(file == "sqlite") %>% pull(path)
users_path <- paths %>% filter(file == "users") %>% pull(path)

orders_table <- "orders"
purchases_table <- "purchases"
common_items_table <- "common_items"

users_df <- read_csv(users_path,
                     col_types = cols(name = col_character(),
                                      password = col_character()))


users <- users_df %>%  pull(password) %>% as.list()
names(users) <- users_df$name

shinyServer(function(input, output, session) {
  ## Password input. Extracted from https://github.com/aoles/shinypass
  
  shinyURL.server()
  
  USER <- reactiveValues(Logged = FALSE)
  
  observeEvent(input$.login, {
    if (isTRUE(users[[input$.username]] == input$.password)) {
      USER$Logged <- TRUE
    } else {
      show("message")
      output$message = renderText("Invalid user name or password")
      delay(2000, hide("message", anim = TRUE, animType = "fade"))
    }
  })
  
  output$app = renderUI(if (!isTRUE(USER$Logged)) {
    fluidRow(column(
      width = 4,
      offset = 4,
      wellPanel(
        id = "login",
        textInput(".username", "Username:"),
        passwordInput(".password", "Password:"),
        div(actionButton(".login", "Log in"), style =
              "text-align: center;")
      ),
      textOutput("message")
    ))
    
    # End password input
  } else{
    fluidPage(
      theme = shinytheme("sandstone"),
      shinyjs::useShinyjs(),
      # Define action widgets
      fluidRow(column(
        12,
        radioButtons(
          "action",
          "What do you want to do?",
          choices = c(
            "Add order",
            "Remove order" ,
            "Add purchase",
            "Remove purchase"
          ),
          selected = "Add purchase"
        ),
        uiOutput("chooseFormType"),
        shinyjs::hidden(div(
          id = "thankyou_msg",
          h3("Thanks, your response was submitted successfully!"),
          actionLink("submit_another", "Submit another response")
        ))
      )),
      fluidRow(tabsetPanel(
        tabPanel(
          "Summary",
          uiOutput("table.orders"),
          uiOutput("table.purchases")
        )
      ))
      
    )
    
  })
  
  
  
  
  
  
  action <- reactive({
    input$action
  })
  
  
  #########################
  ## render UI functions ##
  #########################
  
  
  output$chooseFormType <- renderUI({
    # Define widgets depending on the user action (add/remove order, add/remove purchase)
    
    if (action() == "Add order") {
      d_c_items <- pull.data.from.db(sqlite_path, common_items_table)
      c_items <-  d_c_items %>% arrange(item) %>%  pull(item)
      
      tagList(
        # Is item in the common list?
        checkboxInput(
          "orders_commonitem_check",
          "The item is in the common item list",
          T
        ),
        # Item is in the common list
        # Show menu with items in common list
        conditionalPanel(
          condition = "input.orders_commonitem_check == true",
          selectInput("ordered_item", "What", c_items)
        ),
        # Item isn't in common list
        # Checkbox to add item to common list
        conditionalPanel(
          condition = "input.orders_commonitem_check == false",
          checkboxInput("add_ordered_item_c_list_check", "Add item to common list?", T)
        ),
        # Show text input to add new item
        conditionalPanel(condition = "input.orders_commonitem_check == false",
                         textInput("ordered_new_item", "What")),
        selectInput(
          "priority",
          "Priority",
          c("urgent", "take it easy"),
          selected = "urgent"
        ),
        actionButton("submit_order", "Submit order", class = "btn-primary"),
        
        shinyjs::hidden(
          span(id = "submit_msg", "Submitting..."),
          div(id = "error",
              div(
                br(), tags$b("Error: "), span(id = "error_msg")
              ))
        )
      )
    } else if (action() == "Add purchase") {
      d_c_items <- pull.data.from.db(sqlite_path, common_items_table)
      c_items <-  d_c_items %>% arrange(item) %>%  pull(item)
      d_orders <- pull.data.from.db(sqlite_path, orders_table)
      orders_items <-  d_orders %>% pull(item)
      
      tagList(
        # Is item in the orders list?
        checkboxInput(
          "item_orderslist_check",
          "The item has been already ordered",
          F
        ),
        # Is item in the common list?
        checkboxInput(
          "purchase_commonitem_check",
          "The item is in the common item list",
          T
        ),
        # Item is in the orders list, show menu with items in orders list
        conditionalPanel(
          condition = "input.item_orderslist_check == true",
          selectInput("purchased_ordered_item", "What",
                      orders_items)
        ),
        # Item is in the common list and is NOT in the orders list
        # Show menu with items in common list
        conditionalPanel(
          condition = "input.item_orderslist_check == false & input.purchase_commonitem_check == true",
          selectInput("purchased_c_list_item", "What", c_items)
        ),
        # Item isn't in common list or orders list
        # Checkbox to add item to common list
        conditionalPanel(
          condition = "input.item_orderslist_check == false & input.purchase_commonitem_check == false",
          checkboxInput("add_item_c_list_check", "Add item to common list?", T)
        ),
        # Show text input to add new item
        conditionalPanel(condition = "input.item_orderslist_check == false & input.purchase_commonitem_check == false",
                         textInput("purchased_new_c_list_item", "What")),
        # Text input for price
        textInput("price", "How much"),
        # Submit button
        actionButton("submit_purchase", "Submit purchase", class = "btn-primary"),
        
        shinyjs::hidden(
          span(id = "submit_msg", "Submitting..."),
          div(id = "error",
              div(
                br(), tags$b("Error: "), span(id = "error_msg")
              ))
        )
      )
    } else if (action() == "Remove order") {
      tagList(
        uiOutput("showOrdersID"),
        actionButton("remove_order", "Remove order", class = "btn-primary"),
        shinyjs::hidden(
          span(id = "submit_msg", "Submitting..."),
          div(id = "error",
              div(
                br(), tags$b("Error: "), span(id = "error_msg")
              ))
        )
      )
    } else if (action() == "Remove purchase") {
      tagList(
        uiOutput("showPurchasesID"),
        actionButton("remove_purchase", "Remove purchase", class = "btn-primary"),
        shinyjs::hidden(
          span(id = "submit_msg", "Submitting..."),
          div(id = "error",
              div(
                br(), tags$b("Error: "), span(id = "error_msg")
              ))
        )
      )
    }
  })
  
  output$showPurchasesID <- renderUI({
    d_purchases <-
      pull.data.from.db(sqlite_path = sqlite_path, db_table = purchases_table)
    timestamp <-
      d_purchases %>% mutate(timestamp = ymd_hms(timestamp)) %>%
      arrange(desc(timestamp)) %>% pull(timestamp)
    
    selectInput("remove.purchase.id",
                "Select timestamp of item to be removed",
                timestamp)
  })
  
  output$showOrdersID <- renderUI({
    d_orders <-
      pull.data.from.db(sqlite_path, orders_table) %>% filter(purchased == "False")
    timestamp <-
      d_orders %>% mutate(timestamp = ymd_hms(timestamp)) %>%
      arrange(desc(timestamp)) %>% pull(timestamp)
    
    selectInput("remove.order.id",
                "Select ID of item to be removed",
                timestamp)
  })
  
  
  
  
  # Tables: orders and purchases
  
  # Allow user to download purchases
  output$downloadPurchBtn <- downloadHandler(
    filename = function() {
      sprintf("%s.csv", humanTime())
    },
    content = function(file) {
      write.csv(pull.data.from.db(sqlite_path, purchases_table),
                file,
                row.names = FALSE)
    }
  )
  # Allow user to download orders
  output$downloadOrdersBtn <- downloadHandler(
    filename = function() {
      sprintf("%s.csv", humanTime())
    },
    content = function(file) {
      write.csv(pull.data.from.db(sqlite_path, orders_table),
                file,
                row.names = FALSE)
    }
  )
  
  output$table.orders <- renderUI({
    div(
      h2("Current orders"),
      downloadButton("downloadOrdersBtn", "Download Orders"),
      br(),
      br(),
      DT::dataTableOutput("OrdersData")
    )
  })
  
  output$table.purchases <- renderUI({
    div(
      h2("Previous purchases"),
      downloadButton("downloadPurchBtn", "Download purchases"),
      br(),
      br(),
      DT::dataTableOutput("PurchasesData")
      
    )
  })
  
  
  #######################
  ### End of UI stuff ###
  #######################
  
  # Show the previous user inputs
  output$OrdersData <- render.tables(sqlite_path, orders_table)
  output$PurchasesData <-
    render.tables(sqlite_path, purchases_table)
  
  
  
  output$plot.time.purchases <- renderPlot({
    d_purchases <- pull.data.from.db(sqlite_path, purchases_table)
    
    d_purchases %>%
      arrange(date) %>%
      mutate(time_d =  as.numeric(difftime(date, date[1], units = "day"))) %>%
      group_by(name) %>%
      mutate(paid = cumsum(price)) %>%
      ggplot(aes(x = time_d, y = paid, colour = name)) +
      theme_solarized(20) +
      scale_color_manual(name = "Who", values = wes_palette("Cavalcanti")) +
      theme(legend.position = "top") +
      geom_point(size = 5) +
      geom_line() +
      xlim(0, 31) +
      ylim(0, NA) +
      xlab("Time (days)") +
      ylab("Money-money €")
    
    
  })
  
  output$plot.total.purchases <- renderPlot({
    d_purchases <- pull.data.from.db(sqlite_path, purchases_table)
    
    d_purchases %>%
      group_by(name) %>%
      summarise(total = sum(amount)) %>%
      ggplot(aes(x = name, y = total, colour = name)) +
      theme_solarized(20) +
      scale_color_manual(values = wes_palette("Cavalcanti")) +
      geom_point(size = 5) +
      theme(legend.position = "none") +
      ylim(0, NA) +
      xlab("Who") +
      ylab("Money-money €")
    
  })
  
  output$table.total.paid <- renderDataTable({
    d_purchases <- pull.data.from.db(sqlite_path, purchases_table) %>%
      mutate(date = as.character(date))
    
    d_purchases %>%
      group_by(person) %>%
      summarise(total = sum(amount))
    
  })
  
  output$adjust.payments <- renderTable({
    # browser()
    d_purchases <- pull.data.from.db(sqlite_path, purchases_table)
    
    total.per.person <- d_purchases %>%
      group_by(person) %>%
      summarise(total = sum(amount))
    
    total.month <- sum(total.per.person$total)
    total.month.per.person <-
      total.month / length(unique(total.per.person$name))
    out.table <- total.per.person %>%
      mutate(
        total_paid_month = total.month,
        total_to_paid_per_person = total.month.per.person,
        balance = total_to_paid_per_person - total
      )
    out.table
  })
  
  ####################
  ### Server stuff ###
  ####################
  
  
  
  
  
  ### Retrieve forms data ###
  
  ### Orders
  # Gather all the form inputs (and add timestamp)
  orderformData <- reactive({
    # browser()
    data <- data_frame(
      # idx = NA,
      timestamp = as.character(now()),
      year = year(now()),
      month = month(now()),
      day = day(now()),
      person = input$.username,
      item = NA,
      priority = input$priority,
      purchased = "False"
    )
    if(input$orders_commonitem_check){
      data$item <- input$ordered_item
    }else{
      data$item <- input$ordered_new_item
    }
    data
  })
  
  ### Purchases
  # Gather all the form inputs (and add timestamp)
  purchaseformData <- reactive({
    data <- data_frame(
      timestamp = as.character(now()),
      year = year(now()),
      month = month(now()),
      day = day(now()),
      person = input$.username,
      item = NA,
      amount = as.numeric(input$price)
    )
    
    if(input$item_orderslist_check == T){
      data$item <- input$purchased_ordered_item
    }else if(input$item_orderslist_check == F & input$purchase_commonitem_check == T){
      data$item <- input$purchased_c_list_item
    }else if(input$item_orderslist_check == F & input$purchase_commonitem_check == F){
      data$item <- input$purchased_new_c_list_item
    }
    
    data
  })
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit_order, {
    # browser()
    catchy(
      "submit_order",
      f = update.current.orders,
      args = list(
        data = orderformData(),
        sqlite_path = sqlite_path,
        orders_table = orders_table,
        common_items_table = common_items_table,
        new_common_item = input$add_ordered_item_c_list_check
      )
    )
  })
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit_order, {
    # Update orders table
    output$OrdersData <- render.tables(sqlite_path, orders_table)
    
  })
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit_purchase, {
    # browser()
    catchy(
      "submit_purchase",
      f = update.purchase.answers,
      args  = list(
        data = purchaseformData(),
        sqlite_path = sqlite_path,
        orders_table = orders_table,
        purchases_table = purchases_table,
        common_items_table = common_items_table,
        ordered_item = input$item_orderslist_check,
        new_common_item = input$add_item_c_list_check
      )
    )
  })
  
  observeEvent(input$submit_purchase, {
    # Update orders table
    output$OrdersData <- render.tables(sqlite_path, orders_table)
    # Update purchases table
    output$PurchasesData <-
      render.tables(sqlite_path, purchases_table)
  })
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$remove_order, {
    catchy(
      "remove_order",
      f = remove.entry,
      args = list(
        data = pull.data.from.db(sqlite_path = sqlite_path,
                                 db_table = orders_table),
        sqlite_path = sqlite_path,
        table = orders_table,
        # purchases_table = purchases_table,
        id = input$remove.order.id
      )
    )
  })
  # When the Submit button is clicked, submit the response
  observeEvent(input$remove_order, {
    # Show the responses already submitted
    output$OrdersData <- render.tables(sqlite_path, orders_table)
  })
  
  
  observeEvent(input$remove_purchase, {
    catchy(
      "remove_purchase",
      f = remove.entry,
      args = list(
        data = pull.data.from.db(sqlite_path = sqlite_path, db_table = purchases_table),
        sqlite_path = sqlite_path,
        # table = orders_table,
        table = purchases_table,
        id = input$remove.purchase.id
      )
    )
  })
  observeEvent(input$remove_purchase, {
    # browser()
    output$PurchasesData <-
      render.tables(sqlite_path, purchases_table)
    
  })
  # submit another response
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })
  
})