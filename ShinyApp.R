library(ROCit)
library(shiny)
library(dplyr)
library(tidyr)
library(knitr)
library(caret)
library(ggplot2)
library(class)
library(e1071)

# Define UI
ui <- fluidPage(
  
  # App title
  titlePanel("Algorithm Simulator"),
  
  # Sidebar with file input and action button
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Step 1: Choose CSV File",
                accept = c(".csv")
      ),
      
      numericInput("nrows", "Number of rows to display:", value = 18),
      
      br(),
      br(),
      
      actionButton("transform", "Step 2: Transform Data"),
      helpText("The system will first factorize the categroical variables, then standarize numeric variables"),

      
      br(),
      br(),
      
      selectInput("response_var", "Step 3: Select a binomial response and running T-Test on all other variables",
                  choices = NULL),
      helpText("After select a binomial variable, the system will automatically split the data accroding to two binomial response and running T test on all other variable in the data to determine their significance"),
      
      actionButton("T_test", "Submit"),
      
      br(),
      br(),
      br(),
      br(),
      br(),
      
      
      selectInput("algorithm", "Step 4:  Select an algorithm for simulation:",
                  choices = c("Knn", "Naive Bayes", "Linear Regression"),
                  selected = "Knn"),
      helpText("After selecting an algorithm, the system will automatically select the variable deemed significant and run a split test according to the algorithm you selected"),
      
      conditionalPanel(
        condition = "input.algorithm == 'Knn'",
        numericInput("k", "Value K:", value = 3),
        numericInput("Threshhold","Threshold:",value = 0.5, step = 0.1),
        
      ),
      
      # Input for lambda for naive_bayes
      conditionalPanel(
        condition = "input.algorithm == 'Naive Bayes'",
        numericInput("split","Split:", value = 0.7,step = 0.1),
        numericInput("Threshhold2","Threshold:",value=0.5,step = 0.1),
      ),
      
      # Input for intercept for linear regression
      conditionalPanel(
        condition = "input.algorithm == 'Linear Regression'",
        numericInput("split","Split:", value = 0.7,step = 0.1),
        numericInput("Threshold3","Threshold:",value = 0.5,step = 0.1),
      ),
      
      actionButton("run_algorithm", "Run"),
    ),
    
    # Main panel with output table
    mainPanel(
      tabsetPanel(
        tabPanel("Data", tableOutput("data_table")),
        tabPanel("Transformed Data", tableOutput("transformed_table")),
        tabPanel("Significant Variables", tableOutput("Significant"))
      ),
      
      tabsetPanel(
        tabPanel("Test Summary", verbatimTextOutput("Test_Summary")),
      ),
      
       plotOutput("aucplot")
       
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Read data from file
  data <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, stringsAsFactors = FALSE)
  })
  
  
  
  observe({
    updateSelectInput(session, "response_var", 
                      choices = c("", names(data())), 
                      selected = "")
  })
  
  
    
  # Transform categorical variables to factors with integer levels
  transformed_data = reactive({
    req(input$transform)
    data() %>%
      mutate_if(is.character, as.factor) %>%
      mutate_if(is.factor, ~as.integer(factor(.)) - 1) %>%
      mutate_if(is.numeric, scale)
  })
  
  
  Ttestdata = reactive({
    req(input$response_var)
    req(input$T_test)
    
   
    data_split <- transformed_data() %>%
      mutate(response = ifelse(as.numeric(factor(.data[[input$response_var]])) == 1, "Yes", "No"))
    

    ttest_df <- data_split %>%
      select(-{input$response_var}) %>%
      select_if(~ !all(is.na(.)))%>%
      pivot_longer(cols = -response, names_to = "variable", values_to = "value") %>%
      group_by(variable) %>%
      summarize(pvalue = sprintf("%.10f", t.test(value ~ response)$p.value))%>%
      filter(pvalue <= 0.05)
  })


  
  testsummarydata = reactive({
    req(input$algorithm)
    req(input$run_algorithm)
    
    data_split2 = transformed_data() %>%
      select(Ttestdata()$variable, input$response_var)
    
    if (!is.factor(data_split2[[input$response_var]])) {
      data_split2[[input$response_var]] = as.numeric(factor(data_split2[[input$response_var]]))-1
    }
    
    sample_rows <- sample(dim(data_split2)[1], dim(data_split2)[1] * input$split)
    train_sample <- data_split2[sample_rows, ]
    test_sample <- data_split2[-sample_rows, ]
    
   if (input$algorithm == "Knn") {
    
      knncv <- knn.cv(train = data_split2[, -which(colnames(data_split2) == input$response_var)], cl = as.numeric(data_split2[, input$response_var]), k = input$k, prob = TRUE)
      probs = ifelse(knncv == "0",attributes(knncv)$prob, 1- attributes(knncv)$prob)
      NewClass = ifelse(probs > input$Threshhold, "0", "1")
      cm=confusionMatrix(table(NewClass,data_split2[, input$response_var]))
      return(cm)
   }
  
    
    
      else if (input$algorithm == "Naive Bayes") {
      nb <- naiveBayes(train_sample[,-which(colnames(train_sample) == input$response_var)], train_sample[, input$response_var])
      nb_pred <- predict(nb, test_sample[,-which(colnames(test_sample) == input$response_var)],type="raw")
      nb3modelclass = ifelse(nb_pred[,1]>input$Threshhold2,"0","1")
      cm2=confusionMatrix(table(nb3modelclass,test_sample[, input$response_var]))
      return(cm2)
      }
    
      else if (input$algorithm =="Linear Regression") {
        formula_str <- paste(input$response_var, "~ .")
        fit=glm(as.formula(formula_str), data=train_sample,family = binomial)
        lmprob=predict(fit,test_sample,type="response")
        lmclass=ifelse(lmprob>input$Threshold3,1,0)
        cm3=confusionMatrix(table(lmclass,test_sample[, input$response_var]))
        return(cm3)
        #a=rocit(score=as.numeric(as.factor(lmclass)),class=as.numeric(test_sample$Attrition))
        #a$AUC
        #plot(a)
      }
    #probs = ifelse(knncv == "0",attributes(knncv)$prob, 1- attributes(knncv)$prob)
    
    #NewClass = ifelse(probs > 0.9, "0", "1")
    
    # Train and test the selected model on the data
    # (insert code to train and test model here)
    
    # Return a summary of the model performance
    # (insert code to summarize model performance here)
  
    })
  
  aucplot <- reactive({
    req(input$run_algorithm, input$response_var)
    
    data_split3 = transformed_data() %>%
      select(Ttestdata()$variable, input$response_var)
    
    if (!is.factor(data_split3[[input$response_var]])) {
      data_split3[[input$response_var]] = as.numeric(factor(data_split3[[input$response_var]]))-1
    }
     
    if (input$algorithm == "Knn") {
      
      knncv <- knn.cv(train = data_split3[, -which(colnames(data_split3) == input$response_var)], cl = as.numeric(data_split3[, input$response_var]), k = input$k, prob = TRUE)
      probs = ifelse(knncv == "0",attributes(knncv)$prob, 1- attributes(knncv)$prob)
      NewClass = ifelse(probs > input$Threshhold, "0", "1")
      a=rocit(score = as.numeric(as.factor(NewClass)), class = as.numeric(data_split3[[input$response_var]]))
      return(plot(a))}
    else if (input$algorithm == "Naive Bays"){
      nb <- naiveBayes(train_sample()[,-which(colnames(train_sample()) == input$response_var)], train_sample()[, input$response_var])
      nb_pred <- predict(nb, test_sample[,-which(colnames(test_sample) == input$response_var)],type="raw")
      nb3modelclass = ifelse(nb_pred[,1]>input$Threshhold2,"0","1")
      a2=rocit(score = as.numeric(as.factor(nb3modelclass)), class = as.numeric(test_sample[[input$response_var]]))
      return(plot(a2))
    }
    else if (input$algorithm == "Linear Regression"){
      formula_str <- paste(input$response_var, "~ .")
      fit=glm(as.formula(formula_str), data=train_sample,family = binomial)
      lmprob=predict(fit,test_sample,type="response")
      lmclass=ifelse(lmprob>input$Threshold3,1,0)
      a3=rocit(score=as.numeric(as.factor(lmclass)),class=as.numeric(test_sample[[input$response_var]]))
      return(plot(a3))
    }
    
  })
  
    
   # if (input$algorithm == "Knn") {
   #   knncv <- knn.cv(train = data_split2[, -which(colnames(data_split2) == input$response_var)], cl = as.numeric(data_split2[, input$response_var]), k = input$k, prob = TRUE)
   #   probs <- ifelse(knncv == "0", attributes(knncv)$prob, 1 - attributes(knncv)$prob)
   #   NewClass <- ifelse(probs > input$Threshhold, "0", "1")
   # }
    
   # a <- rocit(score = as.numeric(as.factor(NewClass)), class = as.numeric(data_split2[[input$response_var]]))
   # plot(a)
  
  
  # Display transformed data in a table
  
  
  output$transformed_table <- renderTable({  
    transformed_data() %>% slice(1:input$nrows)
  })
  
  output$data_table = renderTable({
    data() %>% slice(1:input$nrows)
  })
  
  output$Significant = renderTable({
    Ttestdata() %>% slice(1:input$nrows)
    
  })
  
  output$Test_Summary = renderPrint({  
    testsummarydata()
    
  })
  
  output$aucplot=renderPlot({
    aucplot()
  })
  
}

# Run the app
shinyApp(ui, server)
