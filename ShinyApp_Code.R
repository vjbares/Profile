
# setwd("ProfileApp")

library(shiny)
library(JM)
library(ggplot2)

#load joint model
load(file="joint5.rda")

#march validaiton
dat_val_may <- read.csv("dat_val_may.csv.gz")
dat_val_may <- dat_val_may[,c("id","months","dates","s","gender","age","marital_status","marital_status2"
                                      ,"med_indicator","med_total","start_bmi","prev_pct_WL_cum","prev_note_count","prev_MonthlyRecordings"
                              ,"pct_WL_mo")]

dat_val_may1 <- dat_val_may[which(is.na(dat_val_may$pct_WL_mo)==0 & as.Date(dat_val_may$dates)<"2016-05-01"),] 

data09a <- read.csv("data09a.csv.gz")











##########     UI     ##########

ui <- navbarPage("Profile App",
                 
                 tabPanel("Graphs",
                          sidebarLayout(
                            
                            # input user ID
                            sidebarPanel(
                              textInput("id", label = h3("Member ID:"), value = ""),
                              actionButton("subButton","Submit")),
                            
                            mainPanel(plotOutput("long_plot"),
                                      plotOutput("surv_plot"))
                          )
                          
                 ),
                 
                 
                 tabPanel("Baseline",
                          div(tableOutput("table_one"), style="font-size:120%")
                 ),
                 
                 
                 tabPanel("Most Recent",
                          h4("Last Month of Activity: ", strong(textOutput("last_date", inline=TRUE))), 
                          br(),
                          div(tableOutput("table_two"), style="font-size:120%")
                 ),
                 
                 
                 tabPanel("Projections",
                          selectInput("mon", label = h3("Months in Program"),
                                      choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6,
                                                     "7" = 7, "8" = 8, "9" = 9, "10" = 10, "11" = 11, "12" = 12),
                                      selected = 12),
                          br(),
                          h4(" NOTE: These values are ", em(textOutput("result_text", inline=TRUE)), " values"),
                          div(tableOutput("table_three"), style="font-size:120%")
                 )
                 
                 
)













##########     SERVER     ##########

server <- function(input, output) {
  
  
  model <- joint5
  
  
  
  
  id <- eventReactive(input$subButton,{input$id})
  
  
  development <- reactive(dat_val_may1[which(dat_val_may1$id==id()),])
  
  max_time <- reactive(max(development()$months))
  next_times <- reactive(max_time()+1)
  times <- reactive(next_times():12)
  
  
  
  ### BASELINE
  #ID, gender, age, start date, starting weight, starting BMI
  table1 <- reactive({
    tab1 <- data09a[which(data09a$id==id() & data09a$months==0)
                    ,c("id","gender","age","height","first_date","first_weight","start_bmi","marital_status","pct_WL_mo")]
    tab1
  })
  
  
  ### MOST RECENT
  #last recorded weight date, last recorded weight, last coach meeting, coach meetings last month,
  #total weight lost so far, percentage of weight lost so far, weight lost last month, BMI last month, months in program
  table2 <- reactive({
    tab2 <- data09a[which(data09a$id==id() & data09a$months==max_time())
                    ,c("dates","months","weight","note_count","pct_WL_cum","pct_WL_mo","bmi", "MonthlyRecordings")]
    tab2$WL_cum <- tab2$weight - table1()$first_weight
    tab2
  })
  
  
  # prediction for wl  
  table3 <- reactive({ 
    #
    longPrbs <- predict(model, newdata = development(), type = "Subject", interval = "confidence", returnData = TRUE, FtTimes=times())
    #
    tmp1 <- longPrbs[which(longPrbs$months<=max_time()),c("id","months","pct_WL_mo")]
    tmp1$se.fit <- 0
    tmp1$low <- tmp1$pct_WL_mo
    tmp1$upp <- tmp1$pct_WL_mo
    tmp1$actual <- 1
    #
    tmp2 <- longPrbs[which(longPrbs$months>max_time()),c("id","months","pred","se.fit","low","upp")]
    tmp2$actual <- 0
    colnames(tmp2) <- colnames(tmp1)
    longPrbs2 <- data.frame(rbind(tmp1,tmp2))
    
    
    
    # conditional survival probabilities at specific time points
    set.seed(123)
    survPrbs <- survfitJM(model, newdata = development(), survTimes = times())
    #
    survPrbs2 <- data.frame(t(matrix(unlist(survPrbs$summaries),  ncol=(12-max_time()), byrow=TRUE)))
    colnames(survPrbs2) <- c("times","Mean","Median","Lower","Upper")
    #
    tmp3 <- as.data.frame(matrix(c(1:max_time(), rep(1,4*max_time())), ncol=5, byrow=FALSE))
    colnames(tmp3) <- colnames(survPrbs2)
    #
    survPrbs3 <- data.frame(rbind(tmp3, survPrbs2))
    
    
    
    ### PROJECTIONS
    #interactive table from drop down box for month
    #only include future months
    #probability of missing, projected percentage of weight loss, cumulative percentage of weight loss, projected weight, projected BMI
    tab3 <- merge(survPrbs3[,c("times","Mean","Lower","Upper")], longPrbs2[,c("id","months","pct_WL_mo","low","upp","actual")], by.x="times", by.y="months")
    
    for(i in 1:12){
      if(i==1){ 
        tab3[i,"prev_weight"] <- table1()[,"first_weight"]+((table1()[,"pct_WL_mo"]/100)*table1()[,"first_weight"]) 
      }
      else tab3[i,"prev_weight"] <- tab3[(i-1),"weight"]
      tab3[i,"weight"] <- tab3[i,"prev_weight"]+((tab3[i,"pct_WL_mo"]/100)*tab3[i,"prev_weight"])	
    }
    
    
    tab3$pct_WL_cum <- ((tab3$weight - table1()$first_weight) / table1()$first_weight) * 100
    tab3$bmi <- (tab3$weight / (table1()$height^2)) * 703
    tab3$WL_cum <- tab3$weight - table1()$first_weight
    
    
    tab3
    
    
  })
  
  
  output$long_plot <- renderPlot({
    #create the graph
    ggplot(table3(), aes(times, weight)) +
      geom_line(size=2, aes(colour=times<=(max_time()+1))) +
      geom_line(size=2, aes(colour=times>=(max_time()+1))) +
      geom_point(size=3, fill="white", aes(shape=times>=(max_time()+1))) +
      scale_shape_manual(values=c(16, 21)) +
      scale_color_manual(values=c("royalblue1","firebrick1")) +
      xlab("Months in Program") +	
      ylab("Weight (pounds)") + 
      scale_x_continuous(breaks = seq(1, 12, 1)) +
      theme(
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey90"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        legend.position = "none"
      )
    
  })
  
  
  
  
  
  
  output$surv_plot <- renderPlot({
    #finally create the graph
    ggplot(table3(), aes(times, 1-Mean)) +
      geom_line(size=2, aes(colour=times<=(max_time()+1))) +
      geom_line(size=2, aes(colour=times>=(max_time()+1))) + 
      geom_point(size=3, fill="white", aes(shape=times>=(max_time()+1))) +
      scale_shape_manual(values=c(16, 21)) +
      scale_color_manual(values=c("royalblue1","firebrick1")) +
      xlab("Months in Program") +	
      ylab("Probability of Missing") + 
      scale_x_continuous(breaks = seq(1, 12, 1)) +
      scale_y_continuous(breaks = seq(0, 1, 0.1), limits=c(0,1)) +
      theme(
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey90"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        legend.position = "none"
      )
    
  })
  
  
  
  
  
  
  #baseline
  table_one_values <- reactive({
    data.frame(Variable = c("Gender",
                            "Age",
                            "Starting Date",
                            "Fist Weight",
                            "Starting BMI"),
               Value = c(as.character(table1()[,2]), 
                         as.numeric(table1()[,3]),
                         format(as.Date(table1()[,5]), "%B %d, %Y"),
                         round(table1()[,6],1),
                         round(table1()[,7],1)))
  })
  
  output$table_one <- renderTable({
    table_one_values()
  })
  
  
  
  
  
  output$last_date <- renderText(format(as.Date(table2()[,1], origin="1970-01-01"), "%B %Y"))
  
  #most recent
  table_two_values <- reactive({
    data.frame(Variable = c("Months in Program",
                            "Weight",
                            "Coach Meetings",
                            "Cumulative Weight Loss Percentage",
                            "BMI",
                            "Monthly Weight Recordings",
                            "Cumulative Weight Lost"),
               Value = c( table2()[,2],
                          paste0(round(table2()[,3],1)," pounds"),
                          table2()[,4],
                          paste0(round(as.numeric(table2()[,5]),1),"%"),
                          round(as.numeric(table2()[,7]),1),
                          table2()[,8],
                          paste0(round(as.numeric(table2()[,9]),1)," pounds")
               ))
  })
  
  output$table_two <- renderTable({
    table_two_values()
  })
  
  
  
  
  #depends on drop-down box - projections or actuals
  prob_miss <- reactive(1-table3()[which(table3()$times==input$mon),]$Mean)
  pct_WL <- reactive(table3()[which(table3()$times==input$mon),]$pct_WL_mo)
  proj_cum_pct_WL <- reactive(table3()[which(table3()$times==input$mon), "pct_WL_cum"])
  proj_WL_cum <- reactive(table3()[which(table3()$times==input$mon), "WL_cum"])
  proj_weight <- reactive(table3()[which(table3()$times==input$mon), "weight"])
  proj_bmi <- reactive(table3()[which(table3()$times==input$mon), "bmi"])
  
  
  output$result_text <- renderText( ifelse(table3()[which(table3()$times==input$mon),"actual"]==0, "projected", "actual") )
  
  table_three_values <- reactive({
    data.frame(Variable = c("Probability of Missing",
                            "Monthly Weight Loss Percentage",
                            "Cumulative Weight Loss Percentage",
                            "Cumulative Weight Loss",
                            "Weight",
                            "BMI"),
               Value = c(round(as.numeric(prob_miss()),4),
                         paste0(round(as.numeric(pct_WL()),2), "%"),
                         paste0(round(as.numeric(proj_cum_pct_WL()),2), "%"),
                         paste0(round(as.numeric(proj_WL_cum()),2), " pounds"),
                         paste0(round(as.numeric(proj_weight()),1)," pounds"),
                         round(as.numeric(proj_bmi()),2)
               ))
  })
  
  output$table_three <- renderTable({
    table_three_values()
  })
  
  
  
  
}




  
  
  
  
  
  
  
  

# Run the application 
shinyApp(ui = ui, server = server)

