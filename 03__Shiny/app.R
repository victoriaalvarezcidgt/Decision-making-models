library(shiny)
library(shinydashboard)
library(shinyWidgets)

# Defining UI
ui <- dashboardPage(
  
  # Title
  dashboardHeader(title = "Credit Risk Models"),
  
  # Defining sideboard menu
  dashboardSidebar(
    # Adding contents
    sidebarMenu(
      menuItem("Home Page", tabName = "Home"),
      menuItem("Page 2", tabName = "page2")
    )),
  
  # Defining dashboard body
  dashboardBody(
    # Adding body contents
    tabItems(
      tabItem(tabName = "Home",
      
      # Adding text
      h2("This is the home page"),
      p("This is the content of the home page")
    ),
    tabItem(tabName = "page2",
      h2("Welcome to Page 2!"),
      p("This is the content of Page 2.")
      )
  )
)
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)
