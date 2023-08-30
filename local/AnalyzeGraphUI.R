library(ggplot2)
library(scales)
library(shiny)
library(stats)
library(pracma)

ui <- fluidPage(
  titlePanel("Neuronal Live Imaging Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("dataFile", "Choose CSV Data File"),
      numericInput("slopeThreshold", "Slope Threshold", value = 1.15),
      sliderInput("countThreshold", "Count Threshold", min = 1, max = 20, value = 10),
      checkboxInput("visualizeROIs", "Visualize ROIs", value = FALSE),
      actionButton("runAnalysis", "Run Analysis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("ROI Plots", plotOutput("linePlot")),
        tabPanel("Histograms", plotOutput("histPlot")),
        tabPanel("Statistics", verbatimTextOutput("statistics")),
        tabPanel("Results", verbatimTextOutput("results"))
      )
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$runAnalysis, {
    dataFile <- input$dataFile$datapath
    fileName <- basename(dataFile)
    dataValues <- read.csv(dataFile, fileEncoding="UTF-8-BOM")

    typeCounts <- rep(0, 7) # To store counts for each type
    
    for(i in 1:60) {
      roiName = paste("IntDen", i, sep="")
      roiMatrix = cbind(dataValues[1], dataValues[i + 1])
      colnames(roiMatrix) = c("Time", "IntDen")
      type = "Unknown"
      m = lm(formula = IntDen~Time, data = roiMatrix)
      slope = summary(m)$coefficients[2, 1]
      if (slope > 1.15) {
        trend = "- Increase"
        spike = any(diff(roiMatrix[,2]) > 1.5)
        type = spike ? "Type 3" : "Type 4"
        typeCounts[as.integer(substr(type, 5, 5))] <- typeCounts[as.integer(substr(type, 5, 5))] + 1
      } else if (slope < -1.15) {
        trend = "- Decrease"
        dip = any(diff(roiMatrix[,2]) < -1.5)
        type = dip ? "Type 5" : "Type 6"
        typeCounts[as.integer(substr(type, 5, 5))] <- typeCounts[as.integer(substr(type, 5, 5))] + 1
      } else {
        trend = "- Unchanged"
        colMedian = median(roiMatrix[,2])
        colAvg = mean(roiMatrix[,2])
        if (any(roiMatrix[,2] >= (colAvg * 1.15))) {
          type = "Type 2"
        } else if (any(roiMatrix[,2] >= (colMedian * 10))) {
          type = "Type 1"
        } else {
          type = "Other"
        }
        typeCounts[as.integer(substr(type, 5, 5))] <- typeCounts[as.integer(substr(type, 5, 5))] + 1
      }
      if (input$visualizeROIs) {
        linePlot = ggplot(data = dataValues, aes_string(x="Time", y=roiName)) +
          geom_line() +
          geom_point() +
          ylim(0,NA) +
          ggtitle(paste(fileName, roiName, "ROI Density Graph")) +
          labs(tag = type, subtitle = trend, caption = paste("Slope: ", slope)) +
          xlab("Time") +
          ylab("Density") +
          geom_smooth(method='lm', formula=y~x) 
        print(linePlot)
      }
    }

    # Descriptive Statistics
    descriptiveStats <- data.frame(
      ROI = colnames(dataValues)[-1],
      Mean = sapply(dataValues[-1], mean),
      Median = sapply(dataValues[-1], median),
      SD = sapply(dataValues[-1], sd),
      Variance = sapply(dataValues[-1], var)
    )
    
    # Autocorrelation
    autocorrValues <- sapply(dataValues[-1], function(x) acf(x, plot = FALSE)$acf[2])
    
    # Fourier Transform
    freqComponents <- sapply(dataValues[-1], function(x) {
      abs(fft(x))[2:(length(x)/2)]
    })
    
    # Histogram Visualization for each ROI
    output$histPlot <- renderPlot({
      ggplot(dataValues, aes_string(x = roiName)) +
        geom_histogram(binwidth = 10) +
        ggtitle(paste("Histogram for", roiName))
    })
    
    # Display descriptive statistics
    output$statistics <- renderText({
      print(descriptiveStats)
    })
    
    # Display the results
    output$results <- renderText({
      cat("Type 1 Count: ", typeCounts[1], "\n",
          "Type 2 Count: ", typeCounts[2], "\n",
          "Type 3 Count: ", typeCounts[3], "\n",
          "Type 4 Count: ", typeCounts[4], "\n",
          "Type 5 Count: ", typeCounts[5], "\n",
          "Type 6 Count: ", typeCounts[6], "\n",
          "Other Count: ", typeCounts[7], "\n")
    })
  })
}

shinyApp(ui = ui, server = server)
