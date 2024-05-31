library(shiny)
library(shinydashboard)
library(DT)

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(
    title = div(img(src = "PP_monogram_kontur_niebieski.png", height = "50px", style = "margin-right: 10px"),
                "Titanic Dataset")
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Age", tabName = "age", icon = icon("life-ring")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              h2("Home Dashboard"),
              fluidRow(
                column(4,
                       # Add 20px to match the margin of the other boxes
                       box(width = 12, status = "primary", solidHeader = TRUE, height = "270px", style = "display: flex; justify-content: space-around; flex-direction: column; height: 100%", 
                           sliderInput("ageRange", "Age Range:", min = 0, max = 100, value = c(0, 100), step = 1),
                           sliderInput("fareRange", "Fare Range:", min = 0, max = 500, value = c(0, 500))
                       )
                ),
                column(4,
                       box(width = 12, status = "primary", solidHeader = TRUE, height = "270px", style = "display: flex; justify-content: space-around; flex-direction: column; height: 100%", 
                           sliderInput("sibSp", "Siblings/Spouses Aboard:", min = 0, max = max(titanic_train$SibSp, na.rm = TRUE), value = c(0, max(titanic_train$SibSp, na.rm = TRUE)), step = 1),
                           sliderInput("parch", "Parents/Children Aboard:", min = 0, max = max(titanic_train$Parch, na.rm = TRUE), value = c(0, max(titanic_train$Parch, na.rm = TRUE)), step = 1)
                       )
                ),
                column(4,
                       fluidRow(
                         box(width = 6, status = "primary", solidHeader = TRUE, height = "125px",
                             checkboxGroupInput("class", "Class:", choices = c("1st" = 1, "2nd" = 2, "3rd" = 3), selected = 1:3)
                         ),
                         box(width = 6, status = "primary", solidHeader = TRUE, height = "125px",
                             selectInput("sex", "Sex:", choices = c("All", "male", "female"), selected = "All")
                         )
                       ),
                       fluidRow(
                         box(width = 6, status = "primary", solidHeader = TRUE, height = "125px",
                             checkboxGroupInput("embarked", "Embarked:", choices = c("C", "Q", "S"), selected = c("C", "Q", "S"))
                         ),
                         box(width = 6, status = "primary", solidHeader = TRUE, height = "125px",
                             radioButtons("survived", "Survived:", choices = c("All", "Yes" = 1, "No" = 0), selected = "All")
                         )
                       )
                )
              ),
              fluidRow(
                box(title = "Titanic Data Table", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("table")
                )
              )
      ),
      tabItem(tabName = "age",
              h2("Age Analysis"),
              p("This section provides an analysis of the age distribution of passengers on the Titanic."),
              box(
                title = "Age Distribution",
                status = "primary",
                solidHeader = TRUE,
                plotOutput("plotSurvivalRate")
              ),
              # number of age bins select with slider
              box(
                title = "Select Age Bins",
                status = "primary",
                solidHeader = TRUE,
                sliderInput("age_bins", "Number of Age Bins:",
                            min = 2, max = 20, value = 5)
              ),
              box(
                title = "Embarkation by age",
                status = "primary",
                solidHeader = TRUE,
                plotOutput("plotAgeEmbarked")
              ),
              box(
                title = "Sex by age",
                status = "primary",
                solidHeader = TRUE,
                plotOutput("plotAgeSex")
              ),
              box(
                title = "Fare by age",
                status = "primary",
                solidHeader = TRUE,
                plotOutput("plotAgeFare")
              )
      ),
      tabItem(tabName = "about",
              h2("About"),
              p("This dashboard was created as part of an assignment for the Data Visualization course."),
              p("The dataset used in this dashboard is the Titanic dataset, which is available in the 'titanic' package."),
              p("The authors are Mateusz Idziejczak 155842 and Mateusz Stawicki 155900")
      )
    )
  )
)
