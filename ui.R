library(shinydashboard)
library(ggplot2)

dashboardPage(
  dashboardHeader(title = "College Football"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Teams", tabName = "teams", icon = icon("th")),
      menuItem("Venues", tabName = "venues", icon = icon("th"))
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
