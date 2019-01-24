# A shiny web app to explore the discharge record of the Gribble Gap watershed in Cullowhee, NC
# J.P. Gannon
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(EcoHydRology)
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

#load data
#setwd("~/OneDrive - Western Carolina University/Gribble Gap Discharge/GG_Discharge")
#Q <- read.csv("GribbleGap_Q_to_SEP17.csv")

##When updating data remember to add NA values for gaps... Jan 1 2016 and May 1
Q <- read.csv("GGQ_to_JAN19_hourly.csv")
Q$time <- strptime(Q$time, format = "%Y-%m-%d %H:%M:%S")
Q$time <- as.POSIXct(Q$time)
#Q$GGwsd <- Q$disch * 0.2009302 #1 L/s = 0.2009302 mmd
#Q_narm <- Q[-which(is.na(Q$disch)==TRUE),]
Q_narm <- Q

P <- read.csv("Cullowhee Precip 2015-2018.csv")
P$Date <- strptime(P$Date, format = "%m/%d/%y")
P$Date <- as.POSIXct(P$Date)

#make static ECDF of entire data record
ecdf_all <- ecdf(Q$disch)
Qrange_all <- seq(0,max(Q$disch, na.rm = TRUE),.1)
Probs_all <- ecdf_all(Qrange_all)

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Gribble Gap Discharge (beta)"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput(inputId = "type", label = strong("Parameter"),
                                choices = c("Discharge (L/s)","Discharge (mm/day)","Water Temperature (C)"),
                                selected = "Discharge (L/s)"),
                    
                    # Select date range to be plotted
                    dateRangeInput("date", strong("Date range"), start = "2015-08-01", end = "2019-01-10",
                                   min = "2015-08-01", max = "2019-1-10"),
                    
                    # Select whether to overlay smooth trend line
                    checkboxInput(inputId = "log", label = strong("Log primary y axis."), value = FALSE),
                    checkboxInput(inputId = "bf", label = strong("Show calculated baseflow."), value = FALSE),
                    numericInput(inputId = "filter", label = "Filter parameter for baseflow calculation", value = 0.925, min = 0, max = 1),
                    checkboxInput(inputId = "temp", label = strong("Show water temperature."), value = FALSE),
                    #checkboxInput(inputId = "precip", label = strong("Show precipitation.(Temporarily from wunderground, gaps present)"), value = TRUE),
                    sliderInput(inputId = "pwidth", label = strong("Precip bar width (note: precip data is incomplete)"), min = 1, max = 10, value = 1),
                    sliderInput(inputId = "mag", label = strong("Plot text magnification."), min = 0.5, max = 2, value = 1),
                     
                    
                    numericInput(inputId = "horizLine", label = "Add a horizontal line at:", value = NA, min = 0, max = 100),
                    
                    conditionalPanel(condition = "input.log == true"
                                     
                                     # ,HTML("Log Y axis.")
                    )
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "500px"),
                    textOutput(outputId = "desc")#,
                    #tags$a(href = "https://www.google.com/finance/domestic_trends", "Source: Google Domestic Trends", target = "_blank")
                  )
                )
)

# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_data <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    
    Q %>%
      filter(time > as.POSIXct(input$date[1]) & time < as.POSIXct(input$date[2]))
    
    })
  
  sel_bf <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    
    Q_narm$BF <- BaseflowSeparation(Q_narm$GGwsd[is.na(Q_narm$GGwsd)==FALSE], input$filter)$bt
    Q_narm$BFQ <- BaseflowSeparation(Q_narm$disch[is.na(Q_narm$disch)==FALSE], input$filter)$bt
   
    #make static ECDF of entire bf data record
    #bfecdf_all <- ecdf(Q_narm$BFQ)
    #bfQrange_all <- seq(0,max(Q_narm$BFQ, na.rm = TRUE),.1)
    #bfProbs_all <- bfecdf_all(bfQrange_all)
    
   Q_narm %>%
      filter(time > as.POSIXct(input$date[1]) & time < as.POSIXct(input$date[2]))
    
  })
  
  sel_bfecdf <- reactive({
    req(input$filter)
    #validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    #validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    
    #Q_narm$BF <- BaseflowSeparation(Q_narm$GGwsd[is.na(Q_narm$GGwsd)==FALSE], input$filter)$bt
    Q_narm$BFQ <- BaseflowSeparation(Q_narm$disch[is.na(Q_narm$disch)==FALSE], input$filter)$bt
    
    #make static ECDF of entire bf data record
    bfecdf_all <- ecdf(Q_narm$BFQ)
    bfQrange_all <- seq(0,max(Q_narm$BFQ),.1)
    bfProbs_all <- bfecdf_all(bfQrange_all)
   
    BFECDF <- cbind(bfProbs_all,bfQrange_all)
  })
  
  selected_weather <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    
    P %>%
      filter(Date > as.POSIXct(input$date[1]) & Date < as.POSIXct(input$date[2]))
  })
  
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
     
    if(input$type == "Discharge (L/s)") {
      toplot <- selected_data()$disch
      bf <- sel_bf()$BFQ
      }
    if(input$type == "Discharge (mm/day)") {
      toplot <- selected_data()$GGwsd
      bf <- sel_bf()$BF
    }
    if(input$type == "Water Temperature (C)") toplot <- selected_data()$TEMPERATURE
    logged <- ''
    if(input$log) logged <- 'y'
    
    
    xlow <- as.POSIXct(input$date[1])
    xhigh <- as.POSIXct(input$date[2]) 
    
    #calculate total discharge in mm
    
    perhour <- selected_data()$GGwsd/24 #days to hours
    
    Qtotal <- sum(perhour, na.rm = TRUE) #* numofdays 
    Qtotal <- round(Qtotal, 2)
      
    Ptotal <- round(sum(selected_weather()$Precmm, na.rm = TRUE),2)
    
    layout(matrix(c(1,2,3,1,2,4), 3, 2), heights = c(1,3,3))
    
    color = "#434343"
    
    #calculate 10% of max for precip and discharge to adjust position of plotting text
    Ptext <- max(selected_weather()$Precmm, na.rm = TRUE) - (max(selected_weather()$Precmm, na.rm = TRUE)/10)
    Qtext <- max(toplot, na.rm = TRUE) - (max(toplot, na.rm = TRUE)/20)
    
    par(mar = c(0,4,0,4), cex = input$mag)
    #precip plot
    plot(x = selected_weather()$Date[selected_weather()$Precmm>0], y = selected_weather()$Precmm[selected_weather()$Precmm>0], 
           type = 'h', xlim = c(xlow, xhigh), lend = 2,lwd = input$pwidth,xlab = '', xaxt = 'n', ylab = "Precip (mm)",
           rev(range(selected_weather()$Precmm[selected_weather()$Precmm>0])))
    text(xlow,Ptext,labels = paste("Total Precip =", Ptotal, "mm"), pos = 4)
    
    par(mar = c(2, 4, 0, 4))
    plot(x = selected_data()$time, y = toplot, type = "l",
         ylab = input$type, col = color, fg = color, col.lab = color, 
         col.axis = color, xlim = c(xlow, xhigh), log = logged
         )
    text(xlow,Qtext,labels = paste("Total Discharge =", Qtotal, "mm"), pos = 4)
    abline(h = input$horizLine, col = "gray")
    
    # Display only if log is checked
   # if(input$log){
    #  par(mar = c(0,4,0,4))
      #precip plot
    #  plot(x = selected_weather()$Date[selected_weather()$Precmm>0], y = selected_weather()$Precmm[selected_weather()$Precmm>0], 
    #       type = 'h', xlim = c(xlow, xhigh), lend = 2,lwd = input$pwidth,xlab = '', xaxt = 'n', ylab = "Precip (mm)",
   #        rev(range(selected_weather()$Precmm[selected_weather()$Precmm>0])))
   #   par(mar = c(2, 4, 0, 4))
    #  plot(x = selected_data()$time, y = toplot, type = "l",
    #       xlab = "Date", ylab = input$type, col = color, fg = color, col.lab = color, col.axis = color,
    #       log = 'y', xlim = c(xlow, xhigh))
   # }
    
    #display only if BF is checked
    if(input$bf & input$type != "Water Temperature (C)"){
      points(x = sel_bf()$time, y = bf, type = "l",
           xlab = "", ylab = '', col = "blue", fg = color, col.lab = color, col.axis = color
           ,yaxt = 'n',xaxt = 'n')
    }
    
    #display only if temp is checked
    if(input$temp){
      par(new = TRUE)
      plot(x = selected_data()$time, y = selected_data()$TEMPERATURE, type = "l",
           xlab = "", ylab = '', col = "red", fg = color, col.lab = color, col.axis = color
           ,yaxt = 'n',xaxt = 'n',xlim = c(xlow, xhigh))
      axis(side = 4, labels = TRUE, col = "red")
      mtext("Temperature (C)", side=4, line = 3)
    } 
    
    #discharge ecdf calcs for selected data
    ecdf_user <- ecdf(selected_data()$disch)
    Qrange_sel <- seq(0,max(selected_data()$disch, na.rm = TRUE),.1)
    probs_user <- ecdf_user(Qrange_sel)
    
    #baseflow ecdf calcs for selected data
    bfecdf_user <- ecdf(sel_bf()$BFQ)
    bfQrange_sel <- seq(0,max(sel_bf()$BFQ),.1)
    bfprobs_user <- bfecdf_user(bfQrange_sel)
    
    #c(bottom, left, top, right)
    par(mar = c(4, 4, 1, 4))
    
    plot(Qrange_all, Probs_all*100, type = 'l', ylab = "Probability of Non-Exceedance (%)", xlab = "Discharge (L/s)",lwd=2)
    legend("bottomright", legend = c("Full Time Period", "Selected Time Period"), lwd = 2, col = c("black", "red"))
    points(Qrange_sel, probs_user*100, type = 'l', col = "red", lwd = 2)
    
    if(input$bf){
    plot(sel_bfecdf()[,2], sel_bfecdf()[,1]*100, type = 'l',lwd = 2, ylab = "Probability of Non-Exceedance (%)", xlab = "Baseflow (L/s)") 
    points(bfQrange_sel, bfprobs_user*100, type = 'l', lwd = 2, col = "red")
    }
    })
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)

