library(ggplot2)
require(scales)
#options(digits=3)

dataFile = file.choose()
fileName = basename(dataFile)
dataValues = read.csv(dataFile, fileEncoding="UTF-8-BOM")

#print(dataValues)

type1Count = 0 # Type 1: Very short-duration transient increase with large spike (< 10 seconds)
type2Count = 0 # Type 2: Medium-duration transient increase (returns to baseline within 50-100 seconds)
type3Count = 0 # Type 3: Sudden sustained increase (within 30 seconds)
type4Count = 0 # Type 4: Gradual sustained increase
type5Count = 0 # Type 5: Gradual increase followed by sudden decrease (within 30 seconds)
type6Count = 0 # Type 6: Gradual sustained decrease
otherCount = 0 # Other: No change / Inconclusive

increaseCount = 0
decreaseCount = 0
unchangedCount = 0

for(i in 1:60) {

  roiName = paste("IntDen", i, sep="")
  roiMatrix = cbind(dataValues[1], dataValues[i + 1])
  colnames(roiMatrix) = c("Time", "IntDen")
  
  #print(roiMatrix)
  
  type = "Unknown"
  
  m = lm(formula = IntDen~Time, data = roiMatrix)
  slope = summary(m)$coefficients[2, 1]
  slopePrint = paste("Slope: ", slope, sep = " ")
  
    if (slope > 1.15) {
      
      trend = "- Increase"
      spike = FALSE
      count = 0
      
      for (j in 1:465) {
        miniSlope = 0
        if (j > 10) { miniSlope = roiMatrix[j,2] / roiMatrix[j-10,2] }
        if (miniSlope >= 1.5){
          count = count + 1
          print(paste("Type 3 IntDen: ", i, " + Time: ", j, miniSlope, sep = " "))
        }
        if (count >= 5) {
          spike = TRUE
        }
      }
      
      if (spike == TRUE) { 
        type = "Type 3" 
        type3Count = type3Count + 1
      }
      
      else { 
        type = "Type 4" 
        type4Count = type4Count + 1
      }
      
    }
  
    else if (slope < -1.15) { 
      
      trend = "- Decrease"
      dip = FALSE
      count = 0
      
      for (j in 1:465) {
        miniSlope = 0
        if (j > 10) { miniSlope = roiMatrix[j,2] / roiMatrix[j-10,2] }
        if (miniSlope <= 0.6 && miniSlope > 0){
          count = count + 1
          print(paste("Type 5 IntDen: ", i, " + Time: ", j, miniSlope, sep = " "))
        }
        if (count >= 5) {
          dip = TRUE
        }
      }
      
      if (dip == TRUE) { 
        type = "Type 5" 
        type5Count = type5Count + 1
      }
      
      else {
        type = "Type 6"
        type6Count = type6Count + 1    
      }
      
    }
    
    else { 
      
      trend = "- Unchanged"
      colMedian = median(roiMatrix[,2])
      colAvg = mean(roiMatrix[,2])
      
      count = 0
      
      for(j in 1:465) {
        if (roiMatrix[j,2] >= (colAvg * 1.15)) { count = count + 1 }
        if (count >= 10) { type = "Type 2"}
        if (roiMatrix[j,2] <= (colAvg * 1.15)) { count = 0 }
      }
      
      if (type == "Type 2") {
        type2Count = type2Count + 1
      }
      
      else {
        type = "Other"
        otherCount = otherCount + 1 
      }
      
    }
    
    for(j in 1:465) {
      if (roiMatrix[j,2] >= (colMedian * 10)) { 
        if (type == "Type 2") { type2Count = type2Count - 1 }
        if (type == "Type 3") { type3Count = type3Count - 1 }
        if (type == "Type 4") { type4Count = type4Count - 1 }
        if (type == "Type 5") { type5Count = type5Count - 1 }
        if (type == "Type 6") { type6Count = type6Count - 1 }
        if (type == "Other") { otherCount = otherCount - 1 }
        
        type = "Type 1"
      }
    }
  
    if (type == "Type 1") { type1Count = type1Count + 1 }
    
  
  linePlot = ggplot(data = dataValues, aes_string(x="Time", y=roiName)) + geom_line() + geom_point() + ylim(0,NA)
  linePlot = linePlot + ggtitle(paste(fileName, roiName, "ROI Density Graph", sep=" ")) + 
    labs(tag = type, subtitle = trend, caption = slopePrint) + xlab("Time") + ylab("Density") + 
        geom_smooth(method='lm', formula=y~x) 
          #+ facet_wrap(~ ROI) 
  
  print(linePlot)

}

print(paste("Type 1 Count: ", type1Count, sep = " "))
print(paste("Type 2 Count: ", type2Count, sep = " "))
print(paste("Type 3 Count: ", type3Count, sep = " "))
print(paste("Type 4 Count: ", type4Count, sep = " "))
print(paste("Type 5 Count: ", type5Count, sep = " "))
print(paste("Type 6 Count: ", type6Count, sep = " "))
print(paste("Other Count: ", otherCount, sep = " "))



# if (positive trendline) { #Trend increase
#   
#   if (sudden) #Type 3 - Sudden sustained increase
#     
#   else #Type 4 - Gradual sustained increase
#   
# }
# 
# else if (negative trendline) { #Trend decrease
#   
#   if (sudden) #Type 5 - Gradual increase followed by sudden decrease
#     
#   else #Type 6 - Gradual sustained decrease
#   
# } 
# 
# else if (flat trendline) { #Transient
#   
#   if (short) #Type 1 - Very short-duration transient increase
#     
#   else #Type 2 - Medium-duration transient increase
#   
# }
# 
# else { #No change / Inconclusive
#   
# }

