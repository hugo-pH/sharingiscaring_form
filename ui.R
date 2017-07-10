library(shinyjs)

shinyUI(fluidPage(
  # Add Javascript
  tags$head(
    tags$link(rel="stylesheet", type="text/css",href="style.css"),
    tags$script(type="text/javascript", src = "md5.js"),
    tags$script(type="text/javascript", src = "passwdInputBinding.js")
  ),
  useShinyjs(),
  
  titlePanel("Password protected Shiny app"),
  
  uiOutput("app")
))