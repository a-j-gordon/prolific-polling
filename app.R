library(shiny)
library(DT)
library(plotly)
library(reshape2)

# Define question data
question_data <- data.frame(
  question_id = c("Registered", "PartyUpcoming", "ApprovalGovernment", "Lonley", "Sad", "Anxious", 
                  "Optimistic", "Discourse", "PoliticalClimate", "TrustMSM", "ReprsentedMSM", 
                  "AITools", "AIEndanger", "AIRegulation", "AITrust", "AIUnderstanding", 
                  "ConcernEnvironment", "CarbonFootprint", "GovernmentClimate", "ImpactCostOfLiving", 
                  "GovernmentCostOfLiving"),
  question_text = c("Are you currently registered to vote in the national (general) election?",
                    "Which party do you intend to vote for in the upcoming general election?",
                    "Do you approve or disapprove of the current Government's record to date?",
                    "How lonely have you felt on a typical day over the last two weeks?",
                    "How sad or happy have you felt on a typical day over the last two weeks?",
                    "How anxious have you felt on a typical day over the last two weeks?",
                    "How pessimistic or optimistic are you about the future of humanity over the next 25 years?",
                    "How would you rate the tone of political discourse in the UK over the last two weeks?",
                    "How divisive do you think the political climate has been over the past two weeks?",
                    "How much do you trust the mainstream news media?",
                    "How often have you felt that your views are represented in mainstream media over the last two weeks?",
                    "How much do you use AI tools in your day-to-day work?",
                    "How concerned are you that AI may endanger your work/livelihood?",
                    "Do you believe that AI is being regulated and safeguarded with the public interest at heart?",
                    "How much do you trust in AI?",
                    "How much do you understand about what AI is in its current state?",
                    "How concerned are you about the state of the environment and climate change?",
                    "Do you believe individuals have a responsibility to reduce their carbon footprint and environmental impact?",
                    "Do you believe the government is doing enough to combat climate change?",
                    "How much have you struggled with the cost of living over the last two weeks?",
                    "Do you feel that the government is doing enough to combat the cost of living crisis?")
)

# UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
           .title-container {
             text-align: left;
           }
           .title-image {
             margin-bottom: 5px; /* Adjust as needed */
           }
           .subtitle {
             font-size: 15px; /* Adjust font size as needed */
             text-align: left;
           }
           ")
    )
  ),
  titlePanel(
    div(class = "title-container",
        img(src = "https://raw.githubusercontent.com/a-j-gordon/prolific-polling/main/Prolific%20Blue.svg", height = "30px", class = "title-image"),
        h2("Polling Data Analysis"),
        p("Data collected from N=2,000 UK Prolific participants for each timepoint", class = "subtitle")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("question_id", "Question ID:", choices = NULL),
      selectInput("demo_of_interest", "Demographic of Interest:", choices = NULL),
      actionButton("show_results", "See Results"),
      dataTableOutput("question_table"),  # Display question table in sidebar
      style = "text-align: left;"  # Adjust sidebar text alignment
    ),
    mainPanel(
      plotlyOutput("poll_plot"),
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Define hardcoded options for question_id and demo_of_interest
  hardcoded_question_id <- c("Registered","PartyUpcoming","ApprovalGovernment","Lonley","Sad","Anxious","Optimistic","Discourse","PoliticalClimate","TrustMSM",
                             "ReprsentedMSM","AITools","AIEndanger","AIRegulation","AITrust","AIUnderstanding","ConcernEnvironment","CarbonFootprint","GovernmentClimate",
                             "ImpactCostOfLiving","GovernmentCostOfLiving")
  hardcoded_demo_of_interest <- c("National","Female","Male","18-24", "25-49","50-64","65+","Asian","Black","Mixed","Other Ethnicity","White","Brexit Party","Conservatives","Greens","Labour","Liberal Democrats","Other Party","SNP")
  
  observe({
    updateSelectInput(session, "question_id", choices = hardcoded_question_id)
  })
  
  observe({
    # Define custom categories and choices
    custom_demo_categories <- list(
      "National"= c("National"),
      "Age" = c("18-24", "25-49", "50-64", "65+"),
      "Gender" = c("Female", "Male"),
      "Ethnicity" = c("Asian", "Black", "Mixed", "Other Ethnicity", "White"),
      "2019 Vote" = c("Brexit Party", "Conservatives", "Greens", "Labour", "Liberal Democrats", "Other Party", "SNP")
    )
    
    # Merge custom categories with hardcoded choices
    custom_choices <- c(custom_demo_categories, hardcoded_demo_of_interest)
    
    updateSelectInput(session, "demo_of_interest", choices = custom_choices)
  })
  
  # Display question table
  output$question_table <- renderDataTable({
    datatable(
      question_data,
      colnames = c("Question ID", "Question Asked"),
      options = list(
        paging = FALSE,  # Disable paging
        searching = FALSE,  # Disable searching
        ordering = TRUE,  # Keep ordering if needed
        dom = 't',  # Remove length menu and other controls
        scrollX = TRUE,
        responsive = TRUE,  # Make the table responsive
        rownames = NULL
      )
    )
  })
  
  observeEvent(input$show_results, {
    question_id <- input$question_id
    demo_of_interest <- input$demo_of_interest
    
    # Read in the data
    url.build <- paste0("https://raw.githubusercontent.com/a-j-gordon/prolific-polling/main/", question_id, ".csv")
    graphing.data <- read.csv(url.build)
    colnames(graphing.data)[colnames(graphing.data) == "Demographic.Level"] <- "Demographic"
    
    # read in question information
    mqr <- read.csv("https://raw.githubusercontent.com/a-j-gordon/prolific-polling/main/mqr.csv")
    
    if(question_id == "Registered"){
      colnames(graphing.data) <- c("Date","Demographic","No and I wont be registering to vote","Not yet but planning to register", "Yes")
    }
    
    graphing.data <- melt(graphing.data)
    graphing.data$Demographic <- as.factor(graphing.data$Demographic)
    graphing.data$variable <- gsub("\\.", " ", graphing.data$variable)
    matching_row <- mqr[mqr$question_id == question_id, ]
    question_text <- matching_row$question_text
    response_options <- matching_row$response_options
    
    # Create plot
    if (length(demo_of_interest) > 1) {
      graphing.data <- subset(graphing.data, Demographic == demo_of_interest[1] | Demographic == demo_of_interest[2])
    } else {
      graphing.data <- subset(graphing.data, Demographic == demo_of_interest)
    }
    
    vec <- unique(graphing.data$Date)
    split_vec <- strsplit(vec, "\\.")
    middle_nums <- sapply(split_vec, function(x) as.numeric(x[2]))
    first_nums <- sapply(split_vec, function(x) as.numeric(x[1]))
    sorted_dates <- vec[order(middle_nums, first_nums)]
    graphing.data$Date <- factor(graphing.data$Date, levels = sorted_dates)
    response_vector <- strsplit(response_options, ":")[[1]]
    graphing.data$variable <- factor(graphing.data$variable, levels = response_vector)
    
    output$poll_plot <- renderPlotly({
      gg <- ggplot(graphing.data, aes(x = Date, y = value, group = variable, text = paste(value,"%"))) +
        geom_line(aes(color = variable)) +
        geom_point(aes(color = variable), size = 3) +
        labs(x = "Polling Date (dd.m.yy)", y = "Percent of Responses", color = "Response", title = question_text) +
        theme_minimal() +
        theme(
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank(),  # Remove minor grid lines
          panel.grid.major.x = element_line(color = "lightgrey"),
          axis.text = element_text(size = 10, face = "bold"),
          axis.title = element_text(size = 12, face = "bold", vjust = 1),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 12, face = "bold"),
          title = element_text(size = 10, face = "bold", hjust = 0.5)
        )
      
      # Convert ggplot object to plotly object
      ggplotly(gg, tooltip = "text")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server, options = list(name = "prolific-polling-app"))
