library(shinydashboard)
library(ggplot2)

dashboardPage(
  dashboardHeader(title = "College Football"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Information", tabName = "info", icon = icon("th")),
      menuItem("Data Exploration", tabName = "explore", icon = icon("th"),
                menuSubItem("Teams", tabName = "teams"),
                menuSubItem("Games", tabName = "games"),
                menuSubItem("Venues", tabName = "venues")
               ),
      menuItem("Unsupervised Learning", tabName = "unsupervised", icon = icon("th")),
      menuItem("Modeling", tabName = "model", icon = icon("th"),
                menuSubItem("Model 1", tabName = "model1"),
                menuSubItem("Model 2", tabName = "model2")
               ),
      menuItem("Data", tabName = "data", icon = icon("th")),
      menuItem("Source API", icon = icon("file-code-o"),href = "https://collegefootballdata.com/")
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "teams",
              fluidRow(
                box(tableOutput("tableTeams"))
              )
      ),
      
      # Second tab content
      tabItem(tabName = "venues",
              fluidRow(
                box(tableOutput("tableVenues"))
              )
      )
    )
  )
)
