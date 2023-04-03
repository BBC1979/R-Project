library(ggplot2)
library(knitr)
library(patchwork)
library(dplyr)
library(gghighlight)
library(report)
library(psych)
library(forcats)
library(readr)
library(table1)

# import data giving new datasheet name ie, wine_data
wine_data <- read.csv("C:/HGEN611PROJECT/revised_wine_data.csv")

# Plot 1

  wine_data %>%                                    # processed data for the analysis
  count(quality.cat) %>%                           # it pipes the chlorides categorical variable
  mutate(quality_ordered = fct_reorder
  (.f = quality.cat, .x =n)) %>%                   # display the variable where factor is mapped to position
  ggplot(aes(x = quality_ordered, y =n)) +         # mapped the variable orderly
  geom_col(fill="#f68060") +                       # It fill the orange color in the plot
  coord_flip() +                                   # It drawing the data points in the plot including the plot margins
  labs(title ="Distribution of wine quality in dataset",# title of the plot
  x ="Alcohol quality(based on rating)", y="Frequency",     # add the names of variables in x and y axis
  caption="Note: Rating A=>6.5, B=>4.9 and C=<4.8")+              # write notes on the bottom of the plot
  theme(plot.title = element_text(size = 15,
  hjust= 0.5,colour = "#1b98e0"))                  # Add the color and adjust size and
                                                   # fix the title at the center

  
  # plot 2 (geom_summary for alcohol sulfates)
  
  wine_data %>% 
    ggplot (aes(x= alcohol, y= sulphates, fill= alcohol.cat))+
    stat_summary(fun.data= mean_se, geom="histo")+
    labs(x= "alcohol",y= "sulphates")

 # Plot 
  wine_data %>% +
    ggplot(aes(x= chlorides, y= quality, fill= chlorides.cat )) +
    stat_summary(fun.data= mean_se, geom="bar")+
    labs(title = "Scatter plot between citric acid and pH",
         caption = "Citic acid concentration in wine", 
         x="citric acid", y = "pH")
  # Br plot for qualitypf
  wine_data %>% 
  count(quality.cat) %>% 
    mutate(quality_ordered = fct_reorder(.f = quality.cat, .x = n)) %>%
    ggplot(aes(x = quality_ordered, y = n)) +
    geom_col(fill="#f68060") +
    coord_flip() +
    xlab("quality") + ylab("count") + 
    labs(title = "Figure 2: Bar graph for quality of wine",
         caption = "Dataset: Wine") +  
    theme_bw()
  
  # Plot 2
  # Creating the histogram based on the alcohol categories
  wine_data%>%                                                 # processed data
  ggplot(aes(x=alcohol, col=style))+                           # mapped the variables in data
  geom_histogram(col="black", fill= "red", size=1)+            # create the histogram plot
  gghighlight(max(alcohol)>10) +                               # Highlighted the maximum alcohol amount more then 10
  labs(title="Distribution of alcohol in wine",
  x="Percentage of alcohol in wine by volume",y="Frequency")+  # write the foot notes
  facet_wrap(~style) +                                         # makes long ribbon of the panels and wraps it into 2d
  theme(plot.title = element_text(size = 15,
  hjust= 0.5,colour = "#1b98e0"))                              # add the color and fix the size on the title and fix at the center
 
# Plot 3
  wine_data %>%                                          # processed data from the raw data set.
  mutate(style= fct_reorder(style, sulphates)) %>%       # create the factor is mapped to the position based on sulphates values
  ggplot(aes(x=style, y=sulphates, fill=style))+         # mapping the variables using aes function to crate the ggplot
  geom_boxplot(varwidth = TRUE)+                         # creates the box plot of the given variables.
  scale_y_continuous(limits=c(0.2,1.6)) +                # customize the y- axis of the given plot.
  theme(legend.position = "right")+                      # adjust the theme on right side of the figure
  labs(title="Box plot for sulphates in wine",
  x= "Types of wine",y= "sulphates(mg/L)",
  caption = "Note: mg= milligram,L= litre") +            # Add the title and names on the x and y axis
  theme(plot.title = element_text(size =15,              # add color, the size of title and fix at the
  hjust= 0.5,colour = "#1b98e0"))                        # center.
  

   # Plot 4
 
  wine_data %>%                                    # revised wine data set
  ggplot(aes(x=volatile_acidity, 
  y = fixed_acidity, col=style)) +                 # mapping the variables for ggplot
  geom_point()+                                    # create the scatter plot of given variables
  theme(legend.position ="right")+                 # add the theme on right side of the image
  labs(title="Scatter plot of volatile and fixed acidity of wine", # title for the plot
  x="fixed acidity(mg/L)",y="volatile acidity(gm/L)",
  caption = "Note:mg = milligram, gm= gram,L= litres")+            # notes on the bottom 
  theme(plot.title = element_text(size=15,
  hjust= 0.5,colour="#1b98e0"))                     # add the color, fix the text size on title and adjust  
                                                    # at the center
  
  # Plot 5
  
  p1<- wine_data %>%                           # processed data
  ggplot(aes(factor(quality),
  alcohol, fill=quality.cat))+                 # mapped the position based on the factor variables
  geom_boxplot()+                              # create the box plot based on the values of variables
  labs(x="wine quality",y="alcohol(gm/l)")+    # write the title on the figure 
                                               # write the foot notes in the plot
  theme(plot.caption=element_text(size=8))+    # write the theme with caption and adjust the size 
  theme(plot.title = element_text(size = 8,
  hjust= 0.5,colour = "#1b98e0"))              # add and fix the size of    
                                               # title and placed at the center
  
  # Plot 6
  
  p2<- wine_data %>%                            # it pipes processed data for the plot 
  ggplot(aes(factor(quality), residual_sugar, 
  fill=quality.cat))+                           # mapped the position of the variables
  geom_boxplot()+                               # crate the box plot of given 
                                                # values of the variables
  labs(x= "wine quality",y= "residual sugar(gm/l)")+
  theme(plot.caption=element_text(size=8))+     # write the theme and fix the size on the plot
  theme(plot.title = element_text(size = 8,
  hjust= 0.5,colour = "#1b98e0"))               # add the color and fix the    
                                                # size and place the title at the center
  
  # Plot 7 combined plot using patchwork function
   (p1+p2) +                             # place the two plots together based 
                                         # on patchwork function
    plot_layout(guides='collect')+       # arrange the plot properly
    plot_annotation(tag_levels= c('A'),  # order the figure serially
                    tag_prefix= 'Fig.',  # add fig caption
                    tag_sep= '.',        # add punctuation mark after the figure
                    tag_suffix=':')      # add the semicolon after the figure
  
  # Demographic tables
  # Create the demographic table of wine data set using table1 function
  
  # Demographic table for numeric variables
  wine_dt<-wine_data %>%                                          # create the new data name
  select(.,-fixed_acidity,-volatile_acidity,-free_sulfur_dioxide, # select only specific numeric variables for the table
           -citric_acid,-residual_sugar,-total_sulfur_dioxide,
           -quality.cat, -alcohol.cat,-chlorides.cat)
  wine_dt$style<-factor(wine_dt$style)                            # factorize the wine style
  table_numeric<-table1(~density +  chlorides+pH +
  sulphates + alcohol + quality | style,data=wine_dt,
  digits=2,topclass="Rtable1-grid")                               # create the numeric demographic descriptive table
  table_numeric                                                   # call th numeric table
  
  
  # Demographic table for categorical variables
  
  wine_dt<-wine_data %>%                                              # create new data name
  select(.,-fixed_acidity,-volatile_acidity,-free_sulfur_dioxide,     # select only the categorical variables 
           -citric_acid,-residual_sugar,-total_sulfur_dioxide,-pH,
           -chlorides,-alcohol,-quality,-density,-sulphates)      
  wine_dt$style<-factor(wine_dt$style)                                # factor the style because it separate red and white wine          
  table_categorical<-table1(~chlorides.cat+alcohol.cat+               # variables using table1 function
  quality.cat|style,data=wine_dt,digits=2,topclass="Rtable1-grid")    # create the table of categorical 
  table_categorical                                                   # Call the categorical table
  
  
  
  