library(shiny)
library(shinydashboard)
library(shinyWidgets)

logo_path <- "./03__Shiny/01__Logos/"

# Defining UI
ui <- dashboardPage(
  
  # Title
  dashboardHeader(title = "Credit Risk Models"),
  
  # Defining sideboard menu
  dashboardSidebar(
    # Adding clickable contents to the sidebar
    sidebarMenu(
      menuItem("Home Page", tabName = "Home"),
      menuItem("Data Input", tabName = "Data Input"),
      menuItem("Feaure Selection", tabName = "Feature Selection")
    ) # End of sidebarMenu()
  ), # End of dashboardSidebar()
  
  # Defining the dashboard body
  dashboardBody(
    # Adding body contents
    tabItems(
      # Home page with placeholder text
      tabItem(tabName = "Home",
      h2("This is the home page"),
      p("This is the content of the home page")
      ), # End of tabItem() {Home Page}
      
      # Data Input
      tabItem(tabName = "Data Input",
      h2("This is where we will input data"),
      p("This is the content of the data inputing page.")
      ), # End of tabItem() {Data input}
    
      # Feature Selection
      tabItem(tabName = "Feature Selection",
      h2("This is where we will do feature selection"),
      p("This is the contents of the feature selection page")
      ) # End of tabItem() {Feature Selection}
    
    ) # End of tabItems()
    ) # End of dashboardBody()
  ) # End of dashboardPage()

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)
