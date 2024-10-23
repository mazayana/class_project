getwd()

#load necessary packages
install.packages("stats")
install.packages("graphics")
install.packages("grDevices")
install.packages("datasets")
install.packages("utils")
install.packages("methods")
install.packages("base")

library("stats")
library("graphics")
library("grDevices")
library("datasets")
library("utils")
library("methods")
library("base")
library(carData)
  #reference for graphs: https://www.rstudio.com/wp-content/uploads/2016/10/how-big-is-your-graph.pdf
  #Accessed on 4 December 2022


#load data set
  data(UN)

#Understanding data frame (descriptive analysis)
  dim(UN) #213 observations with 7 variables
  ?UN #This data set contains values for national health, welfare, and education statistics for 213
  #countries.

attach(UN)
  
#Constructing the graph
#subsetting variables to work with ppgdp, infantMortality and region
  ppgdp_infmort <- as.data.frame(cbind(ppgdp, infantMortality))
  ppgdp_infmort$region <- UN$region

detach(UN)
attach(ppgdp_infmort)

#run an output command before plotting. 
  pdf(file = "UN_plot_reconstructed.pdf",
      width = 7, height = 7)
  #In the end, the graphs will be exported to a pdf file in my working directory
  
  layout.graph <- matrix(c(1,3,2,3), ncol = 2) 
  #I would like to create a matrix with 4x2 dimension, but the matrix will only has a rank of 
  #3, since I would like to show only 3 graphs.
  layout(mat = layout.graph, height = c(0.6,0.4)) #Creating position layout for the graphs  

#First graph: GDP per capita and Infant Mortality
  #Step 1: creating a new vector containing colors for each unique value in variable region
    col.veq <- c("darkmagenta", "magenta", "red", "orange", "yellowgreen", "green", "turquoise1", 
                 "blue")[region];col.veq 
    #R will automatically assign each colors to each unique value in alphabetical order
    
    table(region, col.veq) #ensuring that each colors represents one unique value
  
  #Step 2: creating a new vector containing symbols for each unique value in variable region
    pch.veq <- c(0, 1, 2, 3, 4, 5, 6, 8)[region];pch.veq  
    #follows the same logic as in step 1
    
    table(region,pch.veq) #ensuring that each symbols represents one unique value
  
  #Step 3: constructing the main plot commands
    plot(ppgdp, infantMortality, 
       col = col.veq, 
       pch = pch.veq, 
       cex = 1,
       lwd = 1,
       xlab = "Gross Domestic Product per Capita",
       ylab = "Infant Mortality", 
       col.axis = "white", 
       las = 1) 
  #I need to rearrange the axis labels in the x-axis, but when I remove the x-axis labels 
  #with command xaxt ="n", for some reason it does not produce the desired results. 
  #So, I make the color of the labels in the x and y axis white (whiteout the values)
  #and create new axis labels for both x and y axis.
  
  x <- c("0","","40000","","80000","") #creating new vector for the length of the x-axis labels
  y <- seq(0, 120, by = 20) #creating new vector for the length of the y-axis labels
  
  axis(1, at=x, labels=x) #inserting labels for x-axis
  axis(2, at=y, labels=seq(0,120, by = 20), las = 2) #inserting labels for y-axis

#Second graph: Region and Infant Mortality
  boxplot(infantMortality ~ region, data = ppgdp_infmort,
          col = c("darkmagenta", "magenta", "red", "orange", "yellowgreen", "green", 
                  "turquoise1", "blue"), 
          border = "black",
          xaxt = "n",
          xlab = "Region",
          ylab = "Infant Mortality",
          lwd = 1,
          las = 1,
          horizontal = FALSE) 
  
#legend
  #Step 1: exctracting countries name from variable region
    legend.chr <- as.data.frame(table(region));legend.chr
    legend.plot <- legend.chr[1:8,1]
  
  #Step 2: creating the legend as a new plot
    plot.new()
    #This code helps me to place the legend in the third layout 
    legend("top", legend = legend.plot, ncol = 4, cex =1,
           pch = c(0, 1, 2, 3, 4, 5, 6, 8), col=c("darkmagenta", "magenta", "red", "orange", 
                                                  "yellowgreen", "green", 
                                                  "turquoise1", "blue"),
           text.col = c("darkmagenta", "magenta", "red", "orange", 
                        "yellowgreen", "green", 
                        "turquoise1", "blue"))

dev.off()  
#To export the graphs into the desired output as mentioned before

detach(ppgdp_infmort)

  
  #End of task 3 