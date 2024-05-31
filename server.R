library(shiny)
library(titanic)  # Load this library to get the Titanic dataset
library(DT)
library(dplyr)    # Ensure dplyr is loaded for data manipulation
library(ggplot2)
library(plotly)

# Load Titanic data
data("titanic_train", package = "titanic")

# Define server logic required to draw visualizations and table filtering
server <- function(input, output, session) {
  # Calculate min and max for Age and Fare
  min_age <- min(titanic_train$Age, na.rm = TRUE)
  max_age <- max(titanic_train$Age, na.rm = TRUE)
  min_fare <- min(titanic_train$Fare, na.rm = TRUE)
  max_fare <- max(titanic_train$Fare, na.rm = TRUE)
  
  # Update the sliders with the correct ranges
  updateSliderInput(session, "ageRange", min = min_age, max = max_age, value = c(min_age, max_age))
  updateSliderInput(session, "fareRange", min = min_fare, max = max_fare, value = c(min_fare, max_fare))
  
  # Filter the dataset based on inputs
  filteredData <- reactive({
    req(titanic_train)
    df <- titanic_train
    
    if (!is.null(input$ageRange)) {
      df <- df %>% filter(Age >= input$ageRange[1], Age <= input$ageRange[2])
    }
    
    if (!"All" %in% input$class) {
      df <- df %>% filter(Pclass %in% input$class)
    }
    
    if (input$sex != "All") {
      df <- df %>% filter(Sex == input$sex)
    }
    
    if (!all(c("C", "Q", "S") %in% input$embarked)) {
      df <- df %>% filter(Embarked %in% input$embarked)
    }
    
    if (input$survived != "All") {
      df <- df %>% filter(Survived == as.numeric(input$survived))
    }
    
    if (!is.null(input$fareRange)) {
      df <- df %>% filter(Fare >= input$fareRange[1], Fare <= input$fareRange[2])
    }
    
    if (!is.null(input$sibSp)) {
      df <- df %>% filter(SibSp >= input$sibSp[1], SibSp <= input$sibSp[2])
    }
    
    if (!is.null(input$parch)) {
      df <- df %>% filter(Parch >= input$parch[1], Parch <= input$parch[2])
    }
    
    df
  })
  
  # Render the filtered data table
  output$table <- renderDataTable({
    filteredData()
  })
  
  # Render the Titanic data table
  output$dataTable <- renderTable({
    titanic_train
  })
  
  # Render the passenger class distribution plot
  output$plotClass <- renderPlot({
    ggplot(titanic_train, aes(x = factor(Pclass))) +
      geom_bar(fill = "steelblue") +
      labs(title = "Passenger Class Distribution",
           x = "Passenger Class",
           y = "Count")
  })
  
  # Render the age distribution plot
  output$plotAge <- renderPlot({
    ggplot(titanic_train, aes(x = Age)) +
      geom_histogram(fill = "steelblue", bins = 30) +
      labs(title = "Age Distribution",
           x = "Age",
           y = "Count")
  })
  
  # Render the plot
  output$plotAgeSurvived <- renderPlot({
    ggplot(titanic_train, aes(x = Age, color = factor(Survived))) +
      geom_line(stat = "count") +
      labs(title = "Survived by Age",
           x = "Age",
           y = "Count") +
      # colors
      scale_fill_manual(values = c("red", "green"))
  })
  
  # Render the survival rate plot for input age bins. 
  output$plotSurvivalRate <- renderPlot({
    # Define age bins
    age_bins <- input$age_bins
    
    # Create a new column for age bins
    titanic_train$age_bin <- cut(titanic_train$Age, breaks = age_bins)
    
    # Calculate survival rate for each age bin
    survival_rate <- titanic_train %>%
      group_by(age_bin) %>%
      summarise(survival_rate = mean(Survived))
    
    # Plot the survival rate with line plot
    ggplot(survival_rate, aes(x = age_bin, y = survival_rate)) +
      geom_bar(stat="identity", color = "steelblue") +
      labs(title = "Survival Rate by Age",
           x = "Age Bin",
           y = "Survival Rate") + # 45 degree angle
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$plotAgeEmbarked <- renderPlot({
    # Define age bins
    age_bins <- input$age_bins
    
    # Create a new column for age bins
    titanic_train$age_bin <- cut(titanic_train$Age, breaks = age_bins)
    
    # Calculate embarkation for each age bin
    embarkation <- titanic_train %>%
      group_by(age_bin, Embarked) %>%
      summarise(count = n())
    
    # Plot the embarkation by age
    ggplot(embarkation, aes(x = age_bin, y = count, fill = Embarked)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Embarkation by Age",
           x = "Age Bin",
           y = "Count") +
      scale_fill_manual(values = c("C" = "red", "Q" = "green", "S" = "blue")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$plotAgeSex <- renderPlot({
    # Define age bins
    age_bins <- input$age_bins
    
    # Create a new column
    titanic_train$age_bin <- cut(titanic_train$Age, breaks = age_bins)
    
    # Calculate
    column <- titanic_train %>%
      group_by(age_bin, Sex) %>%
      summarise(count = n())
    
    # Plot
    ggplot(column, aes(x=age_bin, y=count, fill = Sex)) +
      geom_bar(stat="identity", position = "dodge") +
      labs(title = "Sex in age groups",
           x = "Age Bin",
           y = "Count") +
      scale_fill_manual(values = c("male" = "yellow", "female" = "orange")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$plotAgeFare <- renderPlot({
    # Define age bins
    age_bins <- input$age_bins
    
    # Create a new column
    titanic_train$age_bin <- cut(titanic_train$Age, breaks = age_bins)
    
    # Calculate
    column <- titanic_train %>%
      group_by(age_bin) %>%
      summarise(mean_fare = mean(Fare))
    
    # replace age_bin with numeric values
    agebins <- column$age_bin
    column$age_bin <- rank(column$age_bin)
    
    # Plot line
    ggplot(column, aes(x=age_bin, y=mean_fare)) +
      geom_line(color = "steelblue") +
      labs(title = "Fare by age",
           x = "Age Bin",
           y = "Mean Fare") +
      scale_x_continuous(breaks = column$age_bin, labels = agebins) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}