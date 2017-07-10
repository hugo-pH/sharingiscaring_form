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

# credentials <- list("test" = "202cb962ac59075b964b07152d234b70")

# Connect to sqlite database
sqlite_path <- "./home_stuff.db"
orders_table <- "orders"
purchases_table <- "purchases"

users <- read_csv("./users.csv") 
# %>% unlist() %>% as.character()


credentials <- users %>%  pull(password) %>% as.list()
names(credentials) <- users$name

shinyServer(function(input, output, session){
  
  
  
  shinyURL.server()
  
  USER <- reactiveValues(Logged = FALSE)
  
  observeEvent(input$.login, {
    # browser()
    if (isTRUE(credentials[[input$.username]]==input$.password)){
      USER$Logged <- TRUE
    } else {
      show("message")
      output$message = renderText("Invalid user name or password")
      delay(2000, hide("message", anim = TRUE, animType = "fade"))
    }
  })
  
  output$app = renderUI(
    if (!isTRUE(USER$Logged)) {
      fluidRow(column(width=4, offset = 4,
                      wellPanel(id = "login",
                                textInput(".username", "Username:"),
                                passwordInput(".password", "Password:"),
                                div(actionButton(".login", "Log in"), style="text-align: center;")
                      ),
                      textOutput("message")
      ))
    } else {
    #   # Sidebar with a slider input for number of bins
    #   sidebarLayout(
    #     sidebarPanel(
    #       sliderInput("bins",
    #                   "Number of bins:",
    #                   min = 1,
    #                   max = 50,
    #                   value = 30),
    #       shinyURL.ui()
    #     ),
    #     
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #       plotOutput("distPlot")
    #     )
    #   )
    #   
    # }
      
      fluidPage(
        theme = shinytheme("sandstone"),
        shinyjs::useShinyjs(),
        title = "The street of the children's dike",
        div(id = "header",
            h1("Sharing is caring"),
            h4("")),
        fluidRow(
          column(12,
                 radioButtons("action", "What do you want to do?", 
                              choices = c("Add order", "Remove order" , "Add purchase", "Remove purchase"), 
                              selected = "Add purchase"),
                 uiOutput("chooseFormType"), 
                 shinyjs::hidden(
                   div(
                     id = "thankyou_msg",
                     h3("Thanks, your response was submitted successfully!"),
                     actionLink("submit_another", "Submit another response")
                   )
                 )
          )
        ),
        fluidRow(
          tabsetPanel(
            tabPanel("Summary", uiOutput("table.orders"), uiOutput("table.purchases"))
            # tabPanel("Purchases on time", plotOutput("plot.time.purchases")),
            # tabPanel("Total month", plotOutput("plot.total.purchases")),
            # tabPanel("Balance", tableOutput("adjust.payments"))
            
          )
        )
        
      )
      
    }
  )
  
  
  
  
  
  
  action <- reactive({
    input$action
  })
  
  
  #########################
  ## render UI functions ##
  #########################
  
  
  output$chooseFormType <- renderUI({
    
    if(action() == "Add order"){
      tagList(
        selectInput("name", "Who",
                    c(" ",  users)),
        textInput("item", "What"),
        selectInput("priority", "Priority",
                    c("",  "urgent", "take it easy")),
        
        actionButton("submit_order", "Submit order", class = "btn-primary"),
        
        shinyjs::hidden(
          span(id = "submit_msg", "Submitting..."),
          div(id = "error",
              div(br(), tags$b("Error: "), span(id = "error_msg"))
          )
        )
      )
    }else if(action() == "Add purchase"){
      tagList(
        # id = "form_buy",
        h3 = "I bought something",
        selectInput("buy.name", "Who",
                    c(" ",  users)),
        checkboxInput("newitemcheck", "The item was not in the list", T ),
        uiOutput("new.item.check"),
        ## TODO input for bought objects already in the list
        conditionalPanel(
          condition = "input.newitemcheck == true",
          textInput("new.purchase.item", "What")
          
        ),
        textInput("price", "How much"),
        
        actionButton("submit_purchase", "Submit purchase", class = "btn-primary"),
        
        shinyjs::hidden(
          span(id = "submit_msg", "Submitting..."),
          div(id = "error",
              div(br(), tags$b("Error: "), span(id = "error_msg"))
          )
        )
      )
    }else if(action() == "Remove order"){
      tagList(
        uiOutput("showOrdersID"),
        actionButton("remove_order", "Remove order", class = "btn-primary"),
        shinyjs::hidden(
          span(id = "submit_msg", "Submitting..."),
          div(id = "error",
              div(br(), tags$b("Error: "), span(id = "error_msg"))
          )
        )
      )
    }else if(action() == "Remove purchase"){
      tagList(
        uiOutput("showPurchasesID"),
        actionButton("remove_purchase", "Remove purchase", class = "btn-primary"),
        shinyjs::hidden(
          span(id = "submit_msg", "Submitting..."),
          div(id = "error",
              div(br(), tags$b("Error: "), span(id = "error_msg"))
          )
        )
      )
    }
  })
  
  
  
  
  output$new.item.check <- renderUI({
    # browser()
    d_orders <- pull.data.from.db(sqlite_path, orders_table)
    items <-  d_orders %>% pull(item)
    conditionalPanel(
      condition = "input.newitemcheck == false",
      selectInput("list.purchase.item", "What",
                  items)
    )
  })
  
  
  output$showPurchasesID <- renderUI({
    d_purchases <- pull.data.from.db(sqlite_path = sqlite_path, db_table = purchases_table)
    timestamp <-  d_purchases %>% pull(timestamp)
    
    
    selectInput("remove.purchase.id", "Select timestamp of item to be removed",
                timestamp)
  })
  
  output$showOrdersID <- renderUI({
    d_orders <- pull.data.from.db(sqlite_path, orders_table) %>% filter(purchased == "False")
    timestamp <-  d_orders %>% pull(timestamp)
    
    selectInput("remove.order.id", "Select ID of item to be removed",
                timestamp)
  })
  
  # Show the responses already submitted
  output$OrdersData <-render.tables(sqlite_path, orders_table)
  
  
  output$PurchasesData <- render.tables(sqlite_path, purchases_table)
  
  
  # Allow user to download responses
  output$downloadPurchBtn <- downloadHandler(
    filename = function() { 
      sprintf("%s.csv", humanTime())
    },
    content = function(file) {
      write.csv(pull.data.from.db(sqlite_path, purchases_table), file, row.names = FALSE)
    }
  )  
  
  # Allow user to download responses
  output$downloadOrdersBtn <- downloadHandler(
    filename = function() { 
      sprintf("%s.csv", humanTime())
    },
    content = function(file) {
      write.csv(pull.data.from.db(sqlite_path, orders_table), file, row.names = FALSE)
    }
  )  
  
  output$table.orders <- renderUI({
      div(
        h2("Current orders"),
        downloadButton("downloadOrdersBtn", "Download Orders"), br(), br(),
        DT::dataTableOutput("OrdersData") 
      )
  })


  
  output$table.purchases <- renderUI({
    div(
      id = "adminPanel",
      h2("Previous purchases"),
      downloadButton("downloadPurchBtn", "Download purchases"), br(), br(),
      DT::dataTableOutput("PurchasesData")
      
    )
  })
  
  
  output$plot.time.purchases <- renderPlot({
    d_purchases <- pull.data.from.db(sqlite_path, purchases_table)
    
    d_purchases %>% 
      arrange(date) %>% 
      mutate(
        time_d =  as.numeric(difftime(date, date[1], units= "day"))
      ) %>% 
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
      summarise(
        total = sum(amount) 
      ) %>%  
      ggplot(aes(x = name, y = total, colour = name)) +
      theme_solarized(20) +
      scale_color_manual(values = wes_palette("Cavalcanti")) +
      geom_point(size = 5) +
      theme(legend.position = "none") +
      ylim(0, NA) +
      xlab("Who") +
      ylab("Money-money €")
    
  })
  
  output$table.total.paid <-renderDataTable({
    d_purchases <- pull.data.from.db(sqlite_path, purchases_table) %>% 
      mutate(date = as.character(date))
    
    d_purchases %>% 
      group_by(person) %>% 
      summarise(
        total = sum(amount) 
      )
    
  })
  
  output$adjust.payments <-renderTable({
    # browser()
    d_purchases <- pull.data.from.db(sqlite_path, purchases_table)
    
    total.per.person <- d_purchases %>% 
      group_by(person) %>% 
      summarise(
        total = sum(amount) 
      )
      
    total.month <- sum(total.per.person$total)
    total.month.per.person <- total.month / length(unique(total.per.person$name))
    out.table <- total.per.person %>% 
      mutate(total_paid_month = total.month,
             total_to_paid_per_person = total.month.per.person,
             balance = total_to_paid_per_person - total)
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
      person = input$name,
      item = input$item,
      priority = input$priority,
      purchased = "False"
    )
    data
  })
  
  ### Purchases
  # Gather all the form inputs (and add timestamp)
  purchaseformData <- reactive({
    # browser()
    data <- data_frame(
      # idx = NA,
      timestamp = as.character(now()),
      year = year(now()),
      month = month(now()),
      day = day(now()),
      person = input$buy.name,
      item = NA,
      amount = as.numeric(input$price)
    )
    
    if(input$newitemcheck){
      data$item <- input$new.purchase.item
    }else{
      
      data$item <- input$list.purchase.item
    }
    data
  })

  # When the Submit button is clicked, submit the response
  observeEvent(input$submit_order, {
    # browser()
    catchy("submit_order", f = update.current.orders, 
           args = list(data = orderformData(),
                       sqlite_path = sqlite_path,
                       orders_table = orders_table
                       ))
  })
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit_order, {
    # Update orders table
    output$OrdersData <- render.tables(sqlite_path, orders_table)

  })
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit_purchase, {
    # browser()
    catchy("submit_purchase", 
           f = update.purchase.answers, 
            args  = list(data = purchaseformData(),
                         sqlite_path = sqlite_path,
                         orders_table = orders_table,
                         purchases_table = purchases_table,
                         new_item = input$newitemcheck
                         ))
  })
  
  observeEvent(input$submit_purchase, {
    # Update orders table
    output$OrdersData <- render.tables(sqlite_path, orders_table)
    # Update purchases table
    output$PurchasesData <- render.tables(sqlite_path, purchases_table)
  })
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$remove_order, {
    catchy("remove_order", 
           f = remove.entry, 
           args = list(
             data = pull.data.from.db(sqlite_path = sqlite_path,
                                     db_table = orders_table), 
             sqlite_path = sqlite_path,
             table = orders_table,
             # purchases_table = purchases_table,
             id = input$remove.order.id))
  })
  # When the Submit button is clicked, submit the response
  observeEvent(input$remove_order, {
    # Show the responses already submitted
    output$OrdersData <- render.tables(sqlite_path, orders_table)
  })
  
  
  observeEvent(input$remove_purchase, {
    catchy("remove_purchase", 
           f = remove.entry, 
           args = list(
             data = pull.data.from.db(sqlite_path = sqlite_path, db_table = purchases_table), 
             sqlite_path = sqlite_path,
             # table = orders_table,
             table = purchases_table,
             id = input$remove.purchase.id))
  })
  observeEvent(input$remove_purchase, {
    # browser()
    output$PurchasesData <- render.tables(sqlite_path, purchases_table)
    
  })
  # submit another response
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })
  
}
)