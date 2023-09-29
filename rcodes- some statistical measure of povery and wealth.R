### 1 Pareto type 1 distribution
#Pareto type 1 distribution for various alpha = 1, 2, 3, infinity
library(ggplot2)
library (VGAM)
# Function to calculate Pareto Type I density
dpareto <- function (x, shape) {
  shape / x ^ (shape + 1)}
# Define alpha values
alphas <- c (1, 2, 3, Inf)
# Custom color palette for the curves
custom_colors <- c ("blue", "green", "orange", "red")
# Generate data for the curves
x_values <- seq(0.1, 10, by = 0.1)
df <- data.frame(
  x <- rep (x_values, length(alphas)),
  alpha <- rep (alphas, each = length(x_values)),
  density <- dpareto (x = rep (x_values, length(alphas)), shape = rep (alphas, each =
                                                                         length(x_values))))
# Create the plot using ggplot2
p <- ggplot (df, aes (x = x, y = density, color = factor(alpha))) +
  geom_line (size = 0.5) +
  xlim (0, 10) +
  ylim (0, 5) +
  labs (title = "Pareto Type I Distribution",
        x = "X",
        y = "Density") +
  scale_color_manual(name = "alpha", values = custom_colors) +
theme (axis.line = element_line(size = 0.5)) +
  scale_x_continuous(breaks = seq(0, 10, 1), labels = seq(0, 10, 1))
# Draw perpendicular lines from x = 1 to each curve
perpendicular_lines <- lapply (alphas, function(alpha) {
  geom_segment (aes(x = 1, y = 0, xend = 1, yend = dpareto(1, alpha)),
                linetype = "dashed", color = "black")
})
# Add the perpendicular lines to the plot
p + perpendicular_lines


### Pareto Distribution of Shape 1.16
#R scripts for the Lorenz curve and Gini coefficient with shape parameter 1.16 (80-20) rule
library (VGAM)
library(ggplot2)
##Generate a random sample following Pareto distribution (80/20 rule) with a shape parameter of 1.16
set.seed(123)
sample_size <- 10000
shape <- 1.16
incomes <- rpareto(sample_size, shape = shape)
sorted_incomes <- sort(incomes)
Cumulative_Income_Share <- cumsum(sorted_incomes) / sum(sorted_incomes)
Population_Share <- seq_along(sorted_incomes) / length(sorted_incomes)
lorenz_data <- data.frame(Cumulative_Income_Share, Population_Share)

ggplot(lorenz_data, aes(x = Population_Share, y = Cumulative_Income_Share)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "red") +
  labs(x = "Population Share", y = "Cumulative Income Share", title = "Lorenz Curve of shape 1.16") +
  theme(axis.line = element_line(size = 0.5)) +
  
     geom_segment(aes(x = 0.2, y = 0, xend = 0.2, yend = Cumulative_Income_Share[Population_Share == 0.2]),
                  color = "blue", linetype = "dotted") +
     geom_segment(aes(x = 0, y = Cumulative_Income_Share[Population_Share == 0.2],
                      xend = 0.5, yend = Cumulative_Income_Share[Population_Share == 0.2]),
                  color = "blue", linetype = "dotted") +
     geom_segment(aes(x = 0.8, y = 0, xend = 0.8, yend = Cumulative_Income_Share[Population_Share == 0.8]),
                  color = "blue", linetype = "dotted") +
     geom_segment(aes(x = 0, y = Cumulative_Income_Share[Population_Share == 0.8],
                      xend = 0.8, yend = Cumulative_Income_Share[Population_Share == 0.8]),
                  color = "blue", linetype = "dotted") +
     geom_point(data = lorenz_data[Population_Share == 0.2, ],
                aes(x = Population_Share, y = Cumulative_Income_Share), color = "blue", size = 3) +
     geom_point(data = lorenz_data[Population_Share == 0.8, ],
                aes(x = Population_Share, y = Cumulative_Income_Share), color = "blue", size = 3)+
     theme (axis.line = element_line(size = 0.5))+
     geom_text (aes(x=0.3, y= 0.5, label= "Line of perfect Equality"))+
     geom_text (aes(x=0.8, y= 0.2, label= "Lorenz Curve"))+
     scale_x_continuous (breaks = seq(0, 1, 0.10), labels = seq(0, 1, 0.10)) +
     scale_y_continuous (breaks = seq(0, 1, 0.10), labels = seq(0, 1, 0.10))
   library(ineq)
   Gini(sorted_incomes)
        
    ###  SHAPE 1
        # Lorenz curve, Gini index, and Hoover for shape 1,
        library(VGAM)
        ###for reproducibility
        set.seed(123)
        sample_size <- 10000
        shape <-1
        scale <- 24000
        # Generate income data
        incomes <- rpareto(sample_size, shape = shape, scale = scale)
        sorted_incomes <- sort(incomes)
        Cumulative_Income_Share <- cumsum(sorted_incomes) / sum(sorted_incomes)
        Population_Share <- seq_along(sorted_incomes) / length(sorted_incomes)
        lorenz_data <- data.frame(Cumulative_Income_Share, Population_Share)
        ### GINI COEFFICIENT
        #using the inequality function
        library(ineq)
        Gini_index<- Gini(sorted_incomes)
        Gini_index
        #Hoover index
        #Calculate the vertical distance between the Lorenz curve and the 45-degree line
        lorenz_data$distance <- abs(lorenz_data$Cumulative_Income_Share -
                                      lorenz_data$Population_Share)
        # Find the maximum distance
        max_distance <- max(lorenz_data$distance)
        max_distance
        # Find the corresponding points for maximum distance
        max_distance_points <- lorenz_data[lorenz_data$distance == max_distance, ]
        library(ggplot2)
        # Plot the Lorenz curve and the maximum distance indicated by a line
        ggplot(lorenz_data, aes(x = Population_Share, y = Cumulative_Income_Share)) +
          geom_line(color = "black") +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "red") +
          geom_segment(aes(x = max_distance_points$Population_Share, y =
                             max_distance_points$Cumulative_Income_Share,
                           xend = max_distance_points$Population_Share, yend =
                             max_distance_points$Population_Share),
                       color = "blue", linetype = "solid", size = 1.5) +
          geom_segment(aes(x = max_distance_points$Population_Share, y =
                             max_distance_points$Population_Share,
                           xend = 0, yend = max_distance_points$Population_Share),
                       color = "green", linetype = "dotted", size = 1) +
          geom_segment(aes(x = max_distance_points$Population_Share, y =
                             max_distance_points$Cumulative_Income_Share,
                           xend = max_distance_points$Population_Share, yend = 0),
                       color = "green", linetype = "dotted", size = 1) +
          geom_segment(aes(x = max_distance_points$Population_Share, y =
                             max_distance_points$Cumulative_Income_Share,
                           xend = 0, yend = max_distance_points$Cumulative_Income_Share),
                       color = "green", linetype = "dotted", size = 1) +
          geom_text(aes(x = max_distance_points$Population_Share, y =
                          max_distance_points$Population_Share,
                        label = max_distance_points$Population_Share), vjust = -0.5, hjust = 1, color = "black") +
          geom_text(aes(x = max_distance_points$Population_Share, y =
                          max_distance_points$Cumulative_Income_Share,
                        label = max_distance_points$Cumulative_Income_Share), vjust = 1, hjust = -0.5, color =
                      "blue") +
          geom_text(aes(x = max_distance_points$Population_Share, y =
                          max_distance_points$Cumulative_Income_Share,
                        label = max_distance_points$Cumulative_Income_Share), vjust = -1, hjust = 0.5, color =
                      "black") +
          labs(x = "Population Share", y = "Cumulative Income Share", title = "LORENZ_CURVE OF SHAPE 1") +
          theme(axis.line = element_line(size = 0.5))+
          geom_text(aes(x=0.95, y= 0.7, label="Hoover Index"))+
          geom_text(aes(x=0.6, y= 0.1, label ="Lorenz curve"))+
          geom_text(aes(x= 0.3, y=0.55, label="Line of Perfect Equality"))+
        scale_x_continuous(breaks = seq(0, 1, 0.10), labels = seq(0, 1, 0.10)) +
          scale_y_continuous(breaks = seq(0, 1, 0.10), labels = seq(0, 1, 0.10))
        
        
    ### SHAPE 1.5
        # Lorenz curve, Gini index and Hoover for shape 1.5
        library(VGAM)
        set.seed(127)
        sample_size <- 10000
        shape <-1.5
        scale <- 24000
        #generate income data
        incomes <- rpareto(sample_size, shape = shape, scale = scale)
        sorted_incomes <- sort(incomes)
        Cumulative_Income_Share <- cumsum(sorted_incomes) / sum(sorted_incomes)
        Population_Share <- seq_along(sorted_incomes) / length(sorted_incomes)
        lorenz_data <- data.frame(Cumulative_Income_Share, Population_Share)
        ### GINI COEFFICIENT
        #using the inequality function
        library(ineq)
        Gini_index<- Gini(sorted_incomes)
        Gini_index
        ### PIKETTY_RATIO
        piketty_top_10_ratio <-1- (Cumulative_Income_Share[Population_Share ==0.9])
        piketty_bottom_50_ratio <- Cumulative_Income_Share[Population_Share == 0.5]
        piketty_top_10_bottom_50_ratio <- piketty_top_10_ratio/piketty_bottom_50_ratio
        piketty_top_10_bottom_50_ratio
        
        #Hoover index
        #Calculate the vertical distance between the Lorenz curve and the 45-degree line
        lorenz_data$distance <- abs(lorenz_data$Cumulative_Income_Share -
                                      lorenz_data$Population_Share)
        # Find the maximum distance
        max_distance <- max(lorenz_data$distance)
        max_distance
        # Find the corresponding points for maximum distance
        max_distance_points <- lorenz_data[lorenz_data$distance == max_distance, ]
        library(ggplot2)
        # Plot the Lorenz curve and the maximum distance indicated by a line
        ggplot(lorenz_data, aes(x = Population_Share, y = Cumulative_Income_Share)) +
          geom_line(color = "black") +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "red") +
          geom_segment(aes(x = max_distance_points$Population_Share, y =
                             max_distance_points$Cumulative_Income_Share,
                           xend = max_distance_points$Population_Share, yend =
                             max_distance_points$Population_Share),
                       color = "blue", linetype = "solid", size = 1.5) +
          geom_segment(aes(x = max_distance_points$Population_Share, y =
                             max_distance_points$Population_Share,
                           xend = 0, yend = max_distance_points$Population_Share),
                       color = "yellow", linetype = "dotted", size = 1) +
          geom_segment(aes(x = max_distance_points$Population_Share, y =
                             max_distance_points$Cumulative_Income_Share,
                           xend = max_distance_points$Population_Share, yend = 0),
                       color = "orange", linetype = "dotted", size = 1) +
          geom_segment(aes(x = max_distance_points$Population_Share, y =
                             max_distance_points$Cumulative_Income_Share,
                           xend = 0, yend = max_distance_points$Cumulative_Income_Share),
                       color = "yellow", linetype = "dotted", size = 1) +
        geom_text(aes(x = max_distance_points$Population_Share, y =max_distance_points$Population_Share,label = max_distance_points$Population_Share), 
                  vjust = -0.5, hjust = 1, color = "yellow") +
          geom_text(aes(x = max_distance_points$Population_Share, y = max_distance_points$Cumulative_Income_Share,
                label = max_distance_points$Cumulative_Income_Share), vjust = 1, hjust = -0.5, color ="blue") +
          geom_text(aes(x = max_distance_points$Population_Share, y =
                          max_distance_points$Cumulative_Income_Share,
                        label = max_distance_points$Cumulative_Income_Share), vjust = -1, hjust = 0.5, color = "yellow")+
          geom_text(aes(label = ifelse(Population_Share == 0.5, Cumulative_Income_Share, "")),
                    vjust = -0.5, hjust = -0.5, color = "blue") +
          geom_text(aes(label = ifelse(Population_Share == 0.9, Cumulative_Income_Share, "")),
                    vjust = -0.5, hjust = 1.5, color = "blue") +
          geom_segment(aes(x = 0.5, y = 0, xend = 0.5, yend = Cumulative_Income_Share[Population_Share == 0.5]),
                       color = "blue", linetype = "dotted") +
          geom_segment(aes(x = 0, y = Cumulative_Income_Share[Population_Share == 0.5],
                           xend = 0.5, yend = Cumulative_Income_Share[Population_Share == 0.5]),
                       color = "blue", linetype = "dotted") +
          geom_segment(aes(x = 0.9, y = 0, xend = 0.9, yend = Cumulative_Income_Share[Population_Share == 0.9]), color = "blue", 
                       linetype = "dotted") +
          geom_segment(aes(x = 0, y = Cumulative_Income_Share[Population_Share == 0.9],
                           xend = 0.9, yend = Cumulative_Income_Share[Population_Share == 0.9]),
                       color = "blue", linetype = "dotted") +
          geom_point(data = lorenz_data[Population_Share == 0.5, ],
                     aes(x = Population_Share, y = Cumulative_Income_Share), color = "blue", size = 3) +
          geom_point(data = lorenz_data[Population_Share == 0.9, ],
                     aes(x = Population_Share, y = Cumulative_Income_Share), color = "blue", size = 3) +
          labs(x = "Population Share", y = "Cumulative Income Share", title = "LORENZ CURVE OF SHAPE 1.5") +
          theme(axis.line = element_line(size = 0.5))+
        geom_text(aes(x=0.5, y=0.7, label="Line of Perfect Equality"))+
          geom_text(aes(x=0.5, y=0.1, label ="Lorenz Curve"))+
          geom_text(aes(x=0.9, 0.65, label="Hoover Index"))+
          scale_x_continuous(breaks = seq(0, 1, 0.10), labels = seq(0, 1, 0.10)) +
          scale_y_continuous(breaks = seq(0, 1, 0.10), labels = seq(0, 1, 0.10))
        
        
    ### SHAPE INFINITY
        # Lorenz curve, Gini index and hoover for shape INFINITY,
        library(VGAM)
        ###for reproducibility
        set.seed(123)
        sample_size <- 10000
        shape <-Inf
        scale <- 24000
        #generate income data
        incomes <- rpareto(sample_size, shape = shape, scale = scale)
        sorted_incomes <- sort(incomes)
        Cumulative_Income_Share <- cumsum(sorted_incomes) / sum(sorted_incomes)
        Population_Share <- seq_along(sorted_incomes) / length(sorted_incomes)
        lorenz_data <- data.frame(Cumulative_Income_Share, Population_Share)
        ### GINI COEFFICIENT
        #using the inequality function
        library(ineq)
        Gini_index<- Gini(sorted_incomes)
        Gini_index
        #Hoover index
        #Calculate the vertical distance between the Lorenz curve and the 45-degree line
        lorenz_data$distance <- abs(lorenz_data$Cumulative_Income_Share -
                                      lorenz_data$Population_Share)
        # Find the maximum distance
        max_distance <- max(lorenz_data$distance)
        max_distance
        library(ggplot2)
        # Plot the Lorenz curve
        ggplot(lorenz_data, aes(x = Population_Share, y = Cumulative_Income_Share)) +
          geom_line(color = "black") +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "red") +
          labs (x = "Population Share", y = "Cumulative Income Share", title = "LORENZ CURVE OF INFINITY") +
          theme(axis.line = element_line(size = 0.5))+
          geom_text(aes(x=0.3, y=0.5, label="Line of Perfect Equality"))+
          scale_x_continuous(breaks = seq(0, 1, 0.10), labels = seq(0, 1, 0.10)) +
          scale_y_continuous(breaks = seq(0, 1, 0.10), labels = seq(0, 1, 0.10))
   
        
         ###Income After Tax 2020- 2021
        Income <-c (41100,10500,207000,158000,121000,121000,75100,48300,22000,
                    20500,17700,15700,30500)
        n <-length (Income)
        scale<- min (Income)
        print(scale)
        sum_logs <- sum (log (Income/ scale))
        shape<- round(n/sum_logs,4)
        shape
        sorted_incomes <- sort (Income)
        Cumulative_Income_Share <- cumsum(sorted_incomes) / sum(sorted_incomes)
        Population_Share <- seq_along(sorted_incomes) / length(sorted_incomes)
        lorenz_data <- data.frame(Cumulative_Income_Share, Population_Share)
        ## Gini Coefficient
        library(ineq)
        Gini_Coefficient=round (Gini(Income),4)
        Gini_Coefficient
        # Calculate the y-coordinates where segments intersect with the curves
        yend_05 <- round(approx(lorenz_data$Population_Share, lorenz_data$Cumulative_Income_Share,
                                xout = 0.5)$y,4)
        yend_09 <- round(approx(lorenz_data$Population_Share, lorenz_data$Cumulative_Income_Share,
                                xout = 0.9)$y,4)
        ###Piketty ratio
        piketty_top_10_ratio <-1- (yend_09)
        piketty_bottom_50_ratio <- yend_05
        piketty_top_10_bottom_50_ratio <- round(piketty_top_10_ratio/piketty_bottom_50_ratio,4)
        piketty_top_10_bottom_50_ratio
        ##plot of the hoover_index
        # Calculate the vertical distance between the Lorenz curve and the 45-degree line
        lorenz_data$distance <- round(abs(lorenz_data$Cumulative_Income_Share -
                                            lorenz_data$Population_Share),4)
        # Find the maximum distance
        max_distance <- max(lorenz_data$distance)
        max_distance
        # Find the corresponding points for maximum distance
        max_distance_points <- round(lorenz_data[lorenz_data$distance == max_distance, ],4)
        # Plot the Lorenz curve with the maximum distance indicated by a line and traces
        ggplot(lorenz_data, aes(x = Population_Share, y = Cumulative_Income_Share)) +
          geom_line(color = "blue", size = 1) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
          labs(title = "Lorenz curve of Personal Income After Tax 2020- 2021", x = "Population Share", y =
                 "Cumulative Income Share") +
          geom_segment(aes(x = 0.5, y = 0, xend = 0.5, yend = yend_05),
                       color = "blue", linetype = "dashed",size= 0.5) +
          geom_segment(aes(x = 0, y = yend_05, xend = 0.5, yend = yend_05),
                       color = "blue", linetype = "dashed", size= 0.5) +
          geom_segment(aes(x = 0.9, y = 0, xend = 0.9, yend = yend_09),
                       color = "blue", linetype = "dashed",size= 0.5) +
          geom_segment(aes(x = 0, y = yend_09, xend = 0.9, yend = yend_09),
                       color = "blue", linetype = "dashed",size= 0.5) +
          geom_text(aes(x = 0.5, y = yend_05, label = yend_05),
                    vjust = -1, hjust = -0.5, size = 3, show.legend = FALSE) +
          geom_text(aes(x = 0.9, y = yend_09, label = yend_09),
                    vjust = -1, hjust = 1.5, size = 3, show.legend = FALSE) +
          geom_segment(aes(x = max_distance_points$Population_Share,
                           y = max_distance_points$Cumulative_Income_Share),
                       xend = max_distance_points$Population_Share,
                       yend = max_distance_points$Population_Share,
                       color = "green", linetype = "solid", size = 1.5) +
          geom_segment(aes(x = max_distance_points$Population_Share, y =
                             max_distance_points$Population_Share,
                           xend = 0, yend = max_distance_points$Population_Share),
                       color = "black", linetype = "dashed", size = 0.5) +
          geom_segment(aes(x = max_distance_points$Population_Share, y =
                             max_distance_points$Cumulative_Income_Share,
                           xend = 0, yend = max_distance_points$Cumulative_Income_Share),
                       color = "black", linetype = "dashed", size = 0.5) +
          geom_text(aes(x = max_distance_points$Population_Share, y =
                          max_distance_points$Population_Share,
                        label = max_distance_points$Population_Share), vjust = -0.5, hjust = 1, color = "red") +
          geom_text(aes(x = max_distance_points$Population_Share, y =
                          max_distance_points$Cumulative_Income_Share,
                        label = max_distance_points$Cumulative_Income_Share), vjust = -1, hjust = 0.5, color =
                      "red") +
          theme(axis.line = element_line(size = 0.1))+
          geom_text(aes(x=0.725, y= 0.5, label ="Hoover Index"))+
          geom_text(aes(x=0.75, y= 0.9, label ="Line of Perfect Inequality"))+
          geom_text(aes(x=0.8, y= 0.3, label ="Lorenz Curve"))+
          scale_x_continuous(breaks = seq(0, 1, 0.10), labels = seq(0, 1, 0.10)) +
          scale_y_continuous(breaks = seq(0, 1, 0.10), labels = seq(0, 1, 0.10))
      
        
    #### Identified Personal Wealth 2001-2003
        Wealth <-c (10000, 25000, 45000, 67000, 93000, 120000, 152000,200000, 315000)
        n <-length (Wealth)
        scale<- 1000
        print(scale)
        sum_logs <- sum (log(Wealth/ scale))
        shape<- round(n/sum_logs,4)
        shape
        sorted_Wealth <- sort (Wealth)
        Cumulative_Wealth_Share <- cumsum(sorted_Wealth) / sum(sorted_Wealth)
        Population_Share <- seq_along(sorted_Wealth) / length(sorted_Wealth)
        lorenz_data <- data.frame(Cumulative_Wealth_Share, Population_Share)
        ## Gini Coefficient
        library(ineq)
        Gini_Coefficient=round (Gini(Wealth),4)
        Gini_Coefficient
        # Calculate the y-coordinates where segments intersect with the curves
        yend_05 <- round (approx(lorenz_data$Population_Share, lorenz_data$Cumulative_Wealth_Share,
                                 xout = 0.5)$y,4)
        yend_09 <- round (approx(lorenz_data$Population_Share, lorenz_data$Cumulative_Wealth_Share,
                                 xout = 0.9)$y,4)
        
        ###Piketty ratio
        piketty_top_10_ratio <-1- (yend_09)
        piketty_bottom_50_ratio <- yend_05
        piketty_top_10_bottom_50_ratio <- round(piketty_top_10_ratio/piketty_bottom_50_ratio,4)
        piketty_top_10_bottom_50_ratio
        ##plot of the Hoover index
        # Calculate the vertical distance between the Lorenz curve and the 45-degree line
        lorenz_data$distance <- round (abs (lorenz_data$Cumulative_Wealth_Share -
                                              lorenz_data$Population_Share),4)
        # Find the maximum distance
        max_distance <- max(lorenz_data$distance)
        max_distance
        # Find the corresponding points for maximum distance
        max_distance_points <- round(lorenz_data[lorenz_data$distance == max_distance, ],4)
        # Plot the Lorenz curve with the maximum distance indicated by a line and traces
        ggplot(lorenz_data, aes(x = Population_Share, y = Cumulative_Wealth_Share)) +
          geom_line(color = "blue", size = 1) +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
          labs(title = "Lorenz curve of Indentified Personal Wealth 2001-2003", x = "Population Share", y =
                 "Cumulative Wealth Share") +
          geom_segment(aes(x = 0.5, y = 0, xend = 0.5, yend = yend_05),
                       color = "blue", linetype = "dashed",size= 0.5) +
          geom_segment(aes(x = 0, y = yend_05, xend = 0.5, yend = yend_05),
                       color = "blue", linetype = "dashed", size= 0.5) +
          geom_segment(aes(x = 0.9, y = 0, xend = 0.9, yend = yend_09),
                       color = "blue", linetype = "dashed",size= 0.5) +
          geom_segment(aes(x = 0, y = yend_09, xend = 0.9, yend = yend_09),
                       color = "blue", linetype = "dashed",size= 0.5) +
          geom_text(aes(x = 0.5, y = yend_05, label = yend_05),
                    vjust = -1, hjust = -0.5, size = 3, show.legend = FALSE) +
        geom_text(aes(x = 0.9, y = yend_09, label = yend_09),
                  vjust = -1, hjust = 1.5, size = 3, show.legend = FALSE) +
          geom_segment(aes(x = max_distance_points$Population_Share,
                           y = max_distance_points$Cumulative_Wealth_Share),
                       xend = max_distance_points$Population_Share,
                       yend = max_distance_points$Population_Share,
                       color = "green", linetype = "solid", size = 1.5) +
          geom_segment(aes(x = max_distance_points$Population_Share, y =
                             max_distance_points$Population_Share,
                           xend = 0, yend = max_distance_points$Population_Share),
                       color = "black", linetype = "dashed", size = 0.5) +
          geom_segment(aes(x = max_distance_points$Population_Share, y =
                             max_distance_points$Cumulative_Wealth_Share,
                           xend = 0, yend = max_distance_points$Cumulative_Wealth_Share),
                       color = "black", linetype = "dashed", size = 0.5) +
          geom_text(aes(x = max_distance_points$Population_Share, y =
                          max_distance_points$Population_Share,
                        label = max_distance_points$Population_Share), vjust = -0.5, hjust = 1, color = "red") +
          geom_text(aes(x = max_distance_points$Population_Share, y =
                          max_distance_points$Cumulative_Wealth_Share,
                        label = max_distance_points$Cumulative_Wealth_Share), vjust = -1, hjust = 0.5, color =
                      "red") +
          theme(axis.line = element_line(size = 0.1))+
          geom_text(aes(x=0.66, y= 0.5, label ="Hoover Index"))+
          geom_text(aes(x=0.70, y= 0.9, label ="Line of Perfect Inequality"))+
          geom_text(aes(x=0.74, y= 0.3, label ="Lorenz Curve"))+
          scale_x_continuous(breaks = seq(0, 1, 0.10), labels = seq(0, 1, 0.10)) +
          scale_y_continuous(breaks = seq(0, 1, 0.10), labels = seq(0, 1, 0.10))
        
        