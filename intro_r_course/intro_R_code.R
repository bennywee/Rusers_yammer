###########################################################

# Session 1: Introduction to R
# By Benjamin Wee
# @BenwWee

##########################################################

# How do I run a line of code from a script? Press control and enter (Ctl + Enter) and R will run
# the line of code where the blinking cursor is located and move to the next line. If you want to 
# run multiple lines of code at the same time, highlight the lines of code you want to run and 
# press control and enter. Alt + enter will run the same line of code without moving to the next line.

# Some basic commands and arithmitic----------------------------------------------------------------

print("Hello World") # Prints "Hello World" to the console
print("Ben is making the complex simple")
1+1
2+2
3*3

# Comments-------------------------------------------------------------------------------------------

  ## Anything to the right of a hash on a line is a comment. R will ignore comments when it runs a script. 
  ## This means you can annotate your code to remind yourself or someone what your code is doing!

  1+1 # This is one plus one

# Assigning variables---------------------------------------------------------------------------------

  ## We can asign anything as an object of variable. Datasets, results, graphs etc. This is central to
  ## understanding how R works.

  1+1                         # Wait what if I want to *use* this result for more analysis?
  x <- 1+1                    # Now the result is stored in the object/variable `x`
  value = 4*(5+3)             # You can also use the `=` sign to assign variables or objects
  
  x             # The letter x now contains the result of 1+1. Executing this line will print the result to the console
  value
  
  ## Now you can perform commands over these objects
  x + value
  x * value
  
# Installing and loading libraries--------------------------------------------------------------------
  
  ## Install these packages from the internet. After you run these two lines, feel free to comment them out
  ## Once installed you do not need to do this again (unless the package needs an upgrade)
  install.packages("tidyr")
  install.packages("dplyr")
  install.packages("ggplot2")
  install.packages("readxl")
  
  ## Loading the packages into R, you will need to do this everytime you start a new R session
  library(tidyr)      # For cleaning data
  library(dplyr)      # For summarising and manipulating data. Allows us to use the %>% (pipe) below
  library(ggplot2)    # For beautiful data visualiastion.
  library(readxl)     # For reading various types of spreadsheets into R
  
# Loading in Data-------------------------------------------------------------------------------------

  ## First set your working directory. This tells R where all your files are that you want to work with
  setwd("")
  
  ## Read in the Excel spreadsheets. Assign them to variables "quantity" and "prices"
  quantity <- read_excel("quantity.xlsx")
  prices <- read_excel("prices.xlsx")

  ## Let's see some basic summary statistics for our data. What do you notice?
  summary(quantity)  
  summary(prices)   
  
  ## We can also write the above code using pipes %>%. If we want to perform multiple, 
  ## sequential transformations on our data, we can pipe them through several functions 
  ## at one time. When reading code and coming across %>%, read it as "then". In the singular case,
  ## read this as piping the quantity data set into the summary function. 
  
  ## TIP: Ctl + m is a shortcut for pipes. Try it here:
  
  quantity %>% summary()
  prices %>% summary()
  
# Data cleaning-----------------------------------------------------------------------------------------
  
  # Let's have a look at the prices dataset
  View(prices)

  ## Let's fully clean the prices dataset
 prices_clean <- 
                  prices %>% 
                  gather(year, prices, 3:6) %>%            # "Gather" columns 3 to 6 and create variables "year" and prices
                  unite(Dates, year, Date, sep = "/") %>%  # "Unite" Dates with the year variable to create a full date variable
                  filter(prices > 0,                       # "Filter" out negative prices
                         !is.na(prices))                   # is.na() checks for NAs, ! is a NOT oprator. We do NOT want any NAs
  
  prices_clean %>% summary() 
  
  # If we didn't use pipes, this is what the code would look like
  prices_clean <- tidyr::gather(prices, year, prices, 3:6) 
  prices_clean <- tidyr::unite(prices_clean, Dates, year, Date, sep = "/") 
  prices_clean <- dplyr::filter(prices_clean, prices > 0, !is.na(prices))  
                  
  ## Let's do the same for the quantity dataset
  View(quantity)
  
    quantity_clean <- 
                    quantity %>% 
                    gather(year, quantity, 3:6) %>% 
                    unite(Dates, year, Date, sep = "/") %>% 
                    filter(!is.na(quantity))
                    
  quantity_clean %>% summary()
  
# The rule for clean data: Columns as variables, rows as observations.
  
# Data manipulation-----------------------------------------------------------------------------------------
  
  ## Join the two datasets together and calculate revenue
  data <- left_join(prices_clean, quantity_clean, by = c("Dates", "States")) %>% # Join the two datasets together
          mutate(revenue = prices * quantity,               # Create a new variable by "mutating" existing data.
                 years = as.numeric(substr(Dates, 1, 4)))   # Create a variable for years. Challenege is to figure out what I did here
  
  View(data)
  
  ## Calculate average revenue for each state in each year
  avg_revenue <- data %>% 
                 group_by(States, years) %>%                # Take each unique pair of states and years
                 summarise(average_revenue = mean(revenue)) # For each unique grouping, calculate the average
  
  print(avg_revenue)
  
# Data Visualisation-------------------------------------------------------------------------------------
  
  ## We will use the ggplot2 package for data visualisation. gg stands for "grammar of graphics".
  ## The philosophy of graph building using this package is to build plots in 'layers'. 
  ## Each line of code adds on extra layers and dimensions to the plot. 
  
  ## Visualising average revenue in one chart
  
  ggplot(avg_revenue, aes(x = years, y = average_revenue)) + # Start with setting the data and the x and y axis (Aesthetics)
    geom_line(aes(colour = States), size = 1.2) +            # Choose the type of output, in this case, a line graph coloured by state
    theme_minimal() +                                        # Change the "theme" of the graph to remove grey background
    labs(title = "Average Revenue for each state",
         subtitle = "Brisbane seems to have a structurally larger revenue base",
         x = "Years",
         y = "Average Revenue")
  
  ## How about in three separate charts?
    ggplot(avg_revenue, aes(x = years, y = average_revenue)) + 
    geom_line(aes(colour = States), size = 1.2) +               
    theme_minimal() +
    labs(title = "Average Revenue for each state",
         subtitle = "Brisbane seems to have a structurally larger revenue base",
         x = "Years",
         y = "Average Revenue") +
      facet_wrap(~States) # Same code as above, except we added this line

# A statistics lesson: When correlation does not imply correlation----------------------------------------------------
    
    ## For this example, let's redefine quantity as "quantity demanded" at each given price.
    
    # Let's visualise a scatterplot of prices and quantity with a line of best fit
    ggplot(data, aes(x = quantity, y = prices)) + # Set x and y axis
    geom_point(alpha = 0.5) +
    theme_minimal() +
    geom_smooth(method = lm, linetype = "dashed", se = FALSE) +
    labs(title = "Demand curve estimation",
         subtitle = "What is wrong with this curve? How do would you interpret it?",
         x = "Demand",
         y = "Prices")
    
    
  # Let's colour in the dots based on the state they belong from.
   ggplot(data, aes(x = quantity, y = prices)) + # Set x and y axis
    geom_point(alpha = 0.5, aes(colour = States)) +
     theme_minimal() +
    geom_smooth(method = lm, linetype = "dashed", se = FALSE, colour = "black") +  
         labs(title = "Demand curve estimation",
         subtitle = "Suddenly, the line of best fit doesn't seem to make sense",
         x = "Demand",
         y = "Prices")
    
   # Let's give a line of best fit for each State's prices/demand
 # plot1 <-  
    ggplot(data, aes(x = quantity, y = prices)) + # Set x and y axis
    geom_point(alpha = 0.5, aes(colour = States)) +
    theme_minimal() +
    geom_smooth(method = lm, linetype = "dashed", se = FALSE, colour = "black") +  
    geom_smooth(method = lm, aes(colour = States), se = FALSE) +
         labs(title = "This is an example of Simpson's paradox",
         subtitle = "The sign of the correlation changes when we aggregate the data",
         x = "Demand",
         y = "Prices")
    
  ## Just a technical note, I changed the label of "quantity" to "demand" for the sake of simplicity.
  ## If I have left it as quantity, each point would represent a quantity "purchased" at that particular price.
  ## In some economic models, it is often assumed that each point in the scatterplot represents a "equilibrium"
  ## point, the intersection of supply and demand. Supply and demand estimation requires higher level econometrics.
   
   

     
# Fancy visualisation-----------------------------------------------------------------------------------
  
  ## I will show this during class, don't want to ruin my surprise ;P
  
  install.packages(c("plotly", "ggthemes"))
  library(plotly)
  library(ggthemes)
   
  ggplotly()
   
# Appendix------------------------------------------------------------------
  ## This is the code I used to make the datasets. Essentially I simulated fake data from 
  ## normal/gaussian distributions and adjusted and intercepts of each state while keeping
  ## the conditional means constant.
  
  ## Experienced programemrs may notice the way I produced the data is slightly inefficient 
  ## (I could've done it in a much more straightforward way). This is the product of changing
  ## my mind on what I wanted to do in the data wrangling exercises and thus, variants
  ## of what the raw/messy dataset looked like.
  
  simultation <- function(State, intercept, quantity_mean){
    set.seed(1)
    m <- -1
    b <- intercept
    quantity <- as.data.frame(round(rnorm(1461, quantity_mean, 5)))
    prices <- as.data.frame(m * quantity + b + rnorm(dim(quantity)[1], mean = 0, sd = 5))
    States <- rep(State, dim(quantity)[1])
    Dates <- as.character(rep(seq(as.Date("2015/01/01"), as.Date("2018/12/31"), "day")))
    data <- cbind(Dates, quantity, prices, States)
    names(data) <- c("Dates", "quantity", "prices", "States")
    return(data)
  }
  
  data <- rbind(simultation("Melbourne", intercept=77, quantity_mean = 60),
                simultation("Sydney", intercept=100, quantity_mean = 70),
                simultation("Brisbane", intercept=120, quantity_mean = 80))

  
  ## Separate Date into day/month/year
  dates_sep <- data %>% 
    separate(Dates, sep = "-",  c("Years", "Months", "Days")) %>% 
    unite("Date", c("Months", "Days"), sep = "/") %>% 
    select(Years, Date) 
  
  ## Combine and get rid of Date variable
  quantity <- cbind(dates_sep, data) %>% 
    select(-prices, -Dates) %>% 
    spread(Years, quantity)
  
  prices <- cbind(dates_sep, data) %>% 
    select(-quantity, -Dates) %>% 
    spread(Years, prices)
    


 