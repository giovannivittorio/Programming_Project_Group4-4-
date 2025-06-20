###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          1                               ####
####                                                                       ####
###############################################################################
###############################################################################


##################### Set up environment, install packages, call on libraries


install.packages("devtools")
install.packages("rnaturalearth")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("zoo")
install.packages("maps")
install.packages("timechange")

library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(rnaturalearth)
library(zoo)






################################
###upload data as data frame####
################################

senators_full_data<-read.csv("data/Copyofcongress-trading-all.csv")
senators_control_data <- read.csv("data/Copyofcongress-trading-all.csv")
congresstrades <- read.csv("data/Copyofcongress-trading-all.csv")
#### eliminate duplicate rows

senators_full_data <- distinct(senators_full_data)




###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          2                               ####
####                                                                       ####
###############################################################################
###############################################################################





##                 Clean Up Data
############################################################
########### extract senators transaction size ##############
############################################################
########### change trade date format #######################
############################################################

library(lubridate)

library(stringr)

################## Modify senators_full_data column entries to extract useful data

### Text date --> Date object
### Trade size range text --> Numeric mean of trade size boundaries
### Excess return adjusted for type of transaction (sale/ purchase)
### scale down excess return
### add trade month column

senators_full_data <- senators_full_data|>
  
  mutate( Traded = as.Date(Traded, format = "%A, %B %d, %Y"), # character to date object
          
          
          Trade_year = format(Traded, "%Y"), # new trade_year column
          
          Trade_Size_USD = Trade_Size_USD |>
            str_extract_all( "\\$?\\d{1,3}(,\\d{3})*(\\.\\d+)?|\\$?\\d+(\\.\\d+)?"),
          
          # now  Trade_size_usd column is a list, entries are 2 D character vectors containing boundaries of the trade size range
          
          # modify Trade_Size_USD column to contain numeric mean of range boundaries
          
          Trade_Size_USD = sapply(Trade_Size_USD, function(x) {
            nums <- as.numeric(gsub("[^0-9]", "", x))  # Extract numbers, replace everything that is not a digit between 0 and 9 with nothing
            mean(nums, na.rm = TRUE) #calculate mean 
          }),
          
          Trade_month = floor_date(as.Date(Traded), unit = "month") # add trade month column
          
          
          
  ) 

##### we are interested in the realised returns of congress members, therefore we only use Sale transactions --> ignore purchases and exchanges

senators_full_data <- senators_full_data |>
  filter(Transaction %in% c("Sale", "Sale (Full)"))



###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          3                               ####
####                                                                       ####
###############################################################################
###############################################################################


##########################################################################
#   Calculating weighted average excess return for each year     
#
#      detect anomalous years                                        
#
##########################################################################




library(dplyr)

#remove NA values of trans size and excess return from data

senators_full_data <- senators_full_data |> 
  filter(!is.na(Trade_Size_USD), !is.na(excess_return))

### calculate weighted average return per senator per year in new data frame

senators_full_data_by_year <- senators_full_data |>
  group_by(Trade_year) |>
  summarize(weighted_average_excess_return = sum(Trade_Size_USD*excess_return, na.rm = T)/sum(Trade_Size_USD, na.rm = T ), .groups = "drop" )









###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          4                               ####
####                                                                       ####
###############################################################################
###############################################################################

#################################################
##    import SP 500 return data into a data frame
#################################################


######## assign market annual return data frame, skip first 15 lines of files (metadata) to get proper
########## columns names, Actual data starts at line 16

library(readr)

readLines("data/sp-500-historical-annual-returns.csv", n=20)  ##check where data starts

SP_500_annual_returns <- read_csv("data/sp-500-historical-annual-returns.csv" , skip = 15) ##skip metadata

SP_500_annual_returns <- as.data.frame(SP_500_annual_returns) ##set data to class = dataframe



################## formatting date column to only years (date column entries are class == date)

SP_500_annual_returns[,"date"] <- format(SP_500_annual_returns[,"date"], "%Y")



###### changing value column name

colnames(SP_500_annual_returns)[2] <- "SP_500_return"










###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          5                               ####
####                                                                       ####
###############################################################################
###############################################################################



##########################################################
#
#
#     Sum S&P 500 data to excess return of congress members to get an estimate of the raw return
# 
#      since excess return = net return - s&p 500 return
#
#
#########################################################


### Merge SP 500 data with senators weighted avg return data

senators_full_data_by_year <- left_join(senators_full_data_by_year, SP_500_annual_returns, by = c("Trade_year"="date"))



#### calculate percent difference between  yearly SP 500 return (market return) and
#### senator weighted average yearly return

senators_full_data_by_year <- senators_full_data_by_year |>
  
  mutate(net_return_magnitude = weighted_average_excess_return + SP_500_return,
         
         net_return_percentage = ifelse(SP_500_return > net_return_magnitude,
                                        -100*abs(SP_500_return - net_return_magnitude)/abs(SP_500_return),
                                        100*abs(net_return_magnitude - SP_500_return)/abs(SP_500_return)),
         
         explanation = ifelse (net_return_percentage >0, 
                               paste0(sprintf("%.2f" , net_return_percentage), " % more"),
                               paste0(sprintf("%.2f" , -net_return_percentage), " % less") )
         
  )                      
#

### arrange by abnormal return size

senators_full_data_by_year <- arrange(senators_full_data_by_year, desc(net_return_magnitude ) )













###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          6                               ####
####                                                                       ####
###############################################################################
###############################################################################



########################################
##########     temporal plot          ##
########################################


## load monthly sp 500 data 

sp_500_monthly_full_data <- read.csv("data/sp500_monthly.csv")

## change Date column from character to date object

sp_500_monthly_full_data<- sp_500_monthly_full_data |>
  mutate(Date = as.Date(Date))

# month of October is not displayed properly month = 01, instead of month =10 modify that and change it

library(lubridate)

position <- 10   # first October is in 10th row of the list

while( position < 1833 ) {
  
  temporary_date <- sp_500_monthly_full_data$Date[position]  
  month(temporary_date) <- 10
  sp_500_monthly_full_data$Date[position] <- temporary_date  ## modify temp date month from 01 to 10 then assign back to date vector
  
  position <- position +12 ## go 12 month forward
  
}

#change column name to something more intuitive

colnames(sp_500_monthly_full_data)[2] <- "price"



## calculate monthly return by using lag function 

sp_500_monthly_full_data <- sp_500_monthly_full_data |>
  mutate(price_prior_month = lag(price),
         sp_500_monthly_return = 100*((price + Dividend - price_prior_month )/price_prior_month ) )

## create dataframe with only date and return, remove excess columns

sp_500_monthly_returns <- subset(sp_500_monthly_full_data, select = c(Date, sp_500_monthly_return))

## make sure date column is date object

sp_500_monthly_returns <- sp_500_monthly_returns |>
  mutate(Date =as.Date(Date))


## fill missing market return data using interpolation (avoid gaps in line plot)

library(zoo)

sp_500_monthly_returns$sp_500_monthly_return <- na.approx(sp_500_monthly_returns$sp_500_monthly_return, na.rm = FALSE)


#########################################################
########################################################

################## get average senator return per month 

##########################################################
#########################################################


### add trade month column to senators full data

library(lubridate)


senators_full_data <- senators_full_data |>
  mutate(Trade_month = floor_date(as.Date(Traded), unit = "month"))


## create new data frame with weighted average return per month for entire senate

senators_weighted_average_excess_returns_monthly <- senators_full_data |> 
  group_by(Trade_month) |>
  summarize(total_weight = sum(Trade_Size_USD, na.rm = TRUE),
            weighted_average_excess_return_monthly = sum(Trade_Size_USD*excess_return, na.rm = T)/sum(Trade_Size_USD, na.rm = T ), .groups = "drop" )


## merge datasets 

senators_weighted_average_excess_returns_monthly <- senators_weighted_average_excess_returns_monthly |>
  left_join(sp_500_monthly_returns, by = c("Trade_month" = "Date"))

##############################################################

# reshape data frame to plot two lines in ggplot() geomline(
##### get something like:
###
###
##   01-01-2012    return 1    sp500
##   01-01-2012    return a    senators
##   01-02-2012    return 2    sp500
##   01-02-2012    return b    senators
##     ...           ...        ...
##########################################
## use function pivot_longer

library(tidyr)

reshaped_monthly_returns <- senators_weighted_average_excess_returns_monthly |>
  pivot_longer(
    cols = c(weighted_average_excess_return_monthly, sp_500_monthly_return),
    values_to = "Return",
    names_to = "Series"
  )

###################
###################
# create line plot 
##################

library(ggplot2)

reshaped_monthly_returns |>
  filter(!is.na(Return)) |> #remove NA returns
  ggplot( aes(x=Trade_month , y=Return, group=Series, color=Series)) +
  geom_line(linewidth = 1.2) +
  
  scale_x_date(
    name = "\nYears",
    breaks = as.Date(paste0(2012:2024, "-01-01")),
    limits = as.Date(c("2012-01-01", "2024-12-31")) ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) ) + 
  
  scale_color_discrete(name = "Monthly returns on investment\n",
                       labels = c("sp_500_monthly_return" = "S&P 500\n(risk free return rate)\n" , "weighted_average_excess_return_monthly" = "\nUS Congress\n\n(net return - risk free retrun rate)")) + 
  geom_vline(xintercept = as.Date("2020-03-01"), 
             linetype = "dashed",
             color = "black", 
             linewidth = 0.1) +
  
  annotate("text", 
           x = as.Date("2020-03-01"), 
           y = 500, 
           label = "COVID 19",
           hjust = 1.1, 
           angle = 0, 
           size = 3.5,
           color = "black")

#



########################## covid 19 Zoom in line plot


reshaped_monthly_returns[165:206,] |>
  filter(!is.na(Return)) |> #remove NA returns
  ggplot( aes(x=Trade_month , y=Return, group=Series, color=Series)) +
  geom_line(linewidth = 1.2) +
  
  scale_x_date(
    name = "\nYears",
    limits = as.Date(c("2019-06-01", "2021-01-01")) ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) ) + 
  
  scale_color_discrete(name = "Monthly returns on investment\n",
                       labels = c("sp_500_monthly_return" = "S&P 500\n(risk free return rate)\n" , "weighted_average_excess_return_monthly" = "\nUS Congress\n\n(net return - risk free retrun rate)")) + 
  geom_vline(xintercept = as.Date("2020-03-01"), 
             linetype = "dashed",
             color = "black", 
             size = 0.1) +
  
  annotate("text", 
           x = as.Date("2020-03-01"), 
           y = 150, 
           label = "COVID 19",
           hjust = 1.1, 
           angle = 0, 
           size = 3.5,
           color = "black")

#








###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          7                               ####
####                                                                       ####
###############################################################################
###############################################################################

#### net return magnitude bar chart

ggplot(senators_full_data_by_year, aes(x = as.factor(Trade_year),
                                       y = net_return_magnitude)) +
  
  geom_col() +
  
  labs(x = "\nTrade Year",
       y = "Net return Magnitude\n",
       title = "Net yearly weighted average US Congress return \n")

#### abnormal return percentage bar chart


ggplot(senators_full_data_by_year, aes(x = as.factor(Trade_year),
                                       y = net_return_percentage)) +
  
  geom_col() +
  
  labs(x = "\nTrade Year",
       y = "Net return Percentage\n",
       title = "Proportion of market performance achieved by senate (%)\n")







###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          8                               ####
####                                                                       ####
###############################################################################
###############################################################################


######## heatmap 2019 (highest net return)

########################## 2013 heatmap (abnormal return per state)

return_per_state_2019 <- senators_full_data |> 
  filter(Trade_year == "2019")|>
  group_by(State)|>
  summarize(weighted_average_excess_return = sum(Trade_Size_USD*excess_return, na.rm = T)/sum(Trade_Size_USD, na.rm = T ), .groups = "drop" )

### no need to calculate net return size (ie sum excess return with market return, intensity ranking would stay the same, since for a single year)

## remove Hawaii and Alaska

### remove alaska and hawaii

return_per_state_2019 <- return_per_state_2019 |>
  filter(!(State %in% c("Alaska", "Hawaii")))

####### install shapefile pacakges to get US states Map


devtools::install_github("ropensci/rnaturalearthhires")

library(rnaturalearth)



## import shapefile object of US states map: 

us_states_map = ne_states(country ="united states of america", returnclass = "sf" )


################## create heatmap

# merge map and returns per state data

us_states_returns_map_2019 <- us_states_map |>
  left_join(return_per_state_2019, by = c("name" = "State")) |>
  mutate(Return = replace_na(weighted_average_excess_return, 0))|> ## replace NA for missing states with 0
  filter(!(name %in% c("Alaska", "Hawaii")))




### plot heatmap

ggplot(us_states_returns_map_2019, aes(fill = weighted_average_excess_return)) +
  geom_sf( color = "navy", size = 0.2 ) +
  scale_fill_gradient(low = "lightblue", high = "navy", na.value = "white",
                      name = "Excess return\nby state in \n2019 (%)"
  )







###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          9                               ####
####                                                                       ####
###############################################################################
###############################################################################


########################## 2015 heatmap (highest net return percentage per state)

return_per_state_2015 <- senators_full_data |> 
  filter(Trade_year == "2015")|>
  group_by(State)|>
  summarize(weighted_average_excess_return = sum(Trade_Size_USD*excess_return, na.rm = T)/sum(Trade_Size_USD, na.rm = T ), .groups = "drop" )

###no need to calculate net return size (ie sum excess return with market return, intensity ranking would stay the same, since for a single year)

## remove Hawaii and Alaska

### remove alaska and hawaii

return_per_state_2015 <- return_per_state_2015 |>
  filter(!(State %in% c("Alaska", "Hawaii")))

####### install shapefile pacakges to get US states Map


devtools::install_github("ropensci/rnaturalearthhires")

library(rnaturalearth)



## import shapefile object of US states map: 

us_states_map = ne_states(country ="united states of america", returnclass = "sf" )


################## create heatmap

# merge map and returns per state data

us_states_returns_map_2015 <- us_states_map |>
  left_join(return_per_state_2015, by = c("name" = "State")) |>
  mutate(Return = replace_na(weighted_average_excess_return, 0))|> ## replace NA for missing states with 0
  filter(!(name %in% c("Alaska", "Hawaii")))




### plot heatmap

ggplot(us_states_returns_map_2015, aes(fill = weighted_average_excess_return)) +
  geom_sf( color = "navy", size = 0.2 ) +
  scale_fill_gradient(low = "lightblue", high = "navy", na.value = "white",
                      name = "Excess return\nby state in \n2015 (%)"
  )











###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          10                              ####
####                                                                       ####
###############################################################################
###############################################################################


####################################################
##
##
## Map of net return excess percentage per state for all years 2012 --> 2024
##
##
#####################################################

### average market returns over the years (2012 to 2024)

average_market_return_over_time <- mean(SP_500_annual_returns$SP_500_return[85:98])



## group data by state in new data frame, sum abnormal return size over states for all years

library(tidyverse)
library(dplyr)

returns_by_state <- senators_full_data |>
  group_by(State) |>
  summarize(weighted_average_return_per_state = sum(Trade_Size_USD*excess_return, na.rm = T)/sum(Trade_Size_USD, na.rm = T ), .groups = "drop")


### new variable State net return percentage quantifies how much more (or less) did the congress members outperform (or under perform) compared to the market in %

returns_by_state <- returns_by_state |>
  mutate(net_return_magnitude = weighted_average_return_per_state + average_market_return_over_time, 
         
         state_net_return_percentage = ifelse(average_market_return_over_time > net_return_magnitude,
                                         -100*abs(average_market_return_over_time - net_return_magnitude)/abs(average_market_return_over_time),
                                         100*abs(net_return_magnitude - average_market_return_over_time)/abs(average_market_return_over_time)  ),
  )

### remove alaska and hawaii

returns_by_state <- returns_by_state |>
  filter(!(State %in% c("Alaska", "Hawaii")))

####### install shapefile pacakges to get US states Map


devtools::install_github("ropensci/rnaturalearthhires")

library(rnaturalearth)



## import shapefile object of US states map: 

us_states_map = ne_states(country ="united states of america", returnclass = "sf" )


################## create heatmap

# merge map and returns per state data

us_states_returns_map <- us_states_map |>
  inner_join(returns_by_state, by = c("name" = "State"))


#plot merged data

ggplot(us_states_returns_map, aes(fill = state_net_return_percentage)) +
  geom_sf( color = "black", size = 0.2 ) +
  
scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0,
                     name = "Average market\noutperformance per state\n(in%) from 2012 to 2024") +
  theme_minimal()
  









###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          11                              ####
####                                                                       ####
###############################################################################
###############################################################################


##### bar plot for senate average return by year per party


## gettotal  trade size for each year

Yearly_Tradesize<- senators_full_data |>
  group_by(Trade_year) |>
  summarise(Yearly_trade_size = sum(Trade_Size_USD, na.rm = T ), .groups = "drop")

## new datafraem including yearly trade size column

returns_per_year_per_party <- senators_full_data |>
  left_join(Yearly_Tradesize, by = "Trade_year") 

## calcualate each patrty's yearly weighted average return 

returns_per_year_per_party <- returns_per_year_per_party |>
  group_by(Trade_year, Party) |>
  summarize(weighted_retrun_per_party = sum(excess_return *Trade_Size_USD, na.rm=T),
            .groups = "drop")

## add yearly trade size to data frame

returns_per_year_per_party <- returns_per_year_per_party |> 
  left_join(Yearly_Tradesize, by ="Trade_year")

returns_per_year_per_party <- returns_per_year_per_party |>
  mutate(contribution_weighted_average_return = weighted_retrun_per_party/Yearly_trade_size)


## plot stacked bar chart

ggplot(returns_per_year_per_party, aes(x = as.factor(Trade_year),
                                       y = contribution_weighted_average_return, 
                                       fill = factor(Party))) +
  
  geom_col() +
  
  scale_fill_manual(
    values = c("D" = "blue", "R" = "red", "I" = "green"),  # Custom colors
    labels = c("Democrats", "Independents" , "Republicans" )    # Custom labels
  ) +
  
  labs(
    x = "Trade Year",
    y = "Weighted Average Return\n",
    title = "Parties contribution to total senate average return",
    fill = "Political Party"
  ) 


###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          12                              ####
####                                                                       ####
###############################################################################
###############################################################################


library(tidyverse)
library(dplyr)

## Get weighted avg excess retrun for each individual congress memeber in the data (Group by name)

senators_subpop_positive_excess_returns <- senators_full_data |>
  group_by(Name) |>
  summarize(weighted_avg_excess_return = sum(Trade_Size_USD*excess_return, na.rm = T)/sum(Trade_Size_USD, na.rm = T ), .groups = "drop" )

## add a Group column (categrorical variable) to group vongress members into positive and negative excess return groups
senators_subpop_positive_excess_returns <- senators_subpop_positive_excess_returns |>
  mutate(Group = ifelse (weighted_avg_excess_return > 0, "Higher than market ", "Lower than market"))

## Plot the number of congress memebers in each group
ggplot(senators_subpop_positive_excess_returns, aes(x = Group)
                                       ) +
  
  geom_bar(fill ="blue",
           width = 0.3,) +
  
  labs(x = "Congress members divided by excess return",
       y = "Number of members",
       title = "Subpopulation by return groups")



###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          13                              ####
####                                                                       ####
###############################################################################
###############################################################################


congresstrades <- read.csv("data/Copyofcongress-trading-all.csv")
Sp500 <- read.csv("data/sp-500-historical-annual-returns.csv")


library(ggplot2)
library(dplyr)
library(maps)

us_states <- map_data("state")
congresstrades$state_lower <- tolower(congresstrades$State)

state_trade_counts <- congresstrades %>%
  filter(!is.na(state_lower)) %>%
  group_by(state_lower) %>%
  summarise(num_trades = n())

map_data_joined <- left_join(us_states, state_trade_counts, by = c("region" = "state_lower"))

ggplot(map_data_joined, aes(x = long, y = lat, group = group, fill = num_trades)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey90") +
  labs(title = "Number of Trades by State", fill = "Trade Count") +
  theme_minimal()



###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          14                              ####
####                                                                       ####
###############################################################################
###############################################################################


congresstrades$PartyNumeric <- as.numeric(factor(congresstrades$Party))
levels(factor(congresstrades$Party))

party_labels <- c("1" = "D", "2" = "I", "3" = "R")


counts <- table(congresstrades$PartyNumeric)

barplot(counts,
        main = "Number of Trades by Party",
        xlab = "Party",
        ylab = "Count",
        ylim = c(0, 30000),
        col = "steelblue",
        names.arg = party_labels[names(counts)])
