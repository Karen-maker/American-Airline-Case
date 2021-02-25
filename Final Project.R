#I will be loading this data from my local computer
library(readxl)
library(scales)
library(tidyverse)
library(ggplot2)
library(plotly)
AirFrance_df <- read_excel("/Users/karenlarios/Desktop/AirFrance case.xls")
View(AirFrance_df)

# See if there are missing values
Missing_AirFrance_df <- any(is.na(AirFrance_df)) 
# Remove missing values and create a new data frame  
Final_AirFrance_df <- na.omit(AirFrance_df)
#Check if the missing values were removed
Missval <- any(is.na(Final_AirFrance_df))
# Summary of descriptive statistics
summary(Final_AirFrance_df)

#Deleting columns of Publisher ID, Keyword ID, Keyword type
Final_AirFrance_df$`Publisher ID` <- NULL
Final_AirFrance_df$`Keyword ID` <- NULL

#Adding 5 more columns, Net Revenue, ROA, Average Revenue per Booking, Probability of booking, Cost/booking
Final_AirFrance_df$Net_Rev <- Final_AirFrance_df$Amount - Final_AirFrance_df$`Total Cost`
Final_AirFrance_df$ROA <- Final_AirFrance_df$Net_Rev / Final_AirFrance_df$`Total Cost`
Final_AirFrance_df$Avg_Rev_Booking <- Final_AirFrance_df$Amount / Final_AirFrance_df$`Total Volume of Bookings`
Final_AirFrance_df$Prob_of_Booking <- (Final_AirFrance_df$`Trans. Conv. %` * Final_AirFrance_df$`Engine Click Thru %` /10000)*100
Final_AirFrance_df$Cost_Booking <- Final_AirFrance_df$`Total Cost` / Final_AirFrance_df$`Total Volume of Bookings`

# Convert certain variables to factors
Final_AirFrance_df$`Match Type` <- as.factor(Final_AirFrance_df$`Match Type`)
Final_AirFrance_df$`Bid Strategy` <- as.factor(Final_AirFrance_df$`Bid Strategy`)
Final_AirFrance_df$Status <-as.factor(Final_AirFrance_df$Status)
Final_AirFrance_df$`Publisher Name` <- as.factor(Final_AirFrance_df$`Publisher Name`)
Final_AirFrance_df$`Keyword Group` <- as.factor(Final_AirFrance_df$`Keyword Group`)
Final_AirFrance_df$Category <- as.factor(Final_AirFrance_df$Category)
Final_AirFrance_df$Campaign <- as.factor(Final_AirFrance_df$Campaign)

#Changing some variables to numeric
Final_AirFrance_df$publisher_numeric <- as.numeric(Final_AirFrance_df$`Publisher Name`)
Final_AirFrance_df$match_numeric  <- as.numeric(Final_AirFrance_df$`Match Type`)
Final_AirFrance_df$keyword_group_numeric <- as.numeric(Final_AirFrance_df$`Keyword Group`)
Final_AirFrance_df$campaign_numeric <- as.numeric(Final_AirFrance_df$Campaign)

#Changing data type for Probability of Booking to a binary.
#We want for this variable to be our business success (0 for not booking and 1 for booking)
for(i	in	1:nrow(Final_AirFrance_df)){
  if(Final_AirFrance_df$Prob_of_Booking[i]	=="0"){
    Final_AirFrance_df$Prob_of_Booking[i]	<- 0
  }else{
    Final_AirFrance_df$Prob_of_Booking[i]	<- 1
  }
} #closing the	loop

#Normalizing the data
my_normal <- function(x){
  my_min <- min(x, na.rm=TRUE)
  my_max <- max(x, na.rm=TRUE)
  normalized <- (x-my_min)/(my_max-my_min)
  return(normalized)
}

Final_AirFrance_df$ROA_norm <- my_normal(x=Final_AirFrance_df$ROA)
Final_AirFrance_df$Publisher_norm <- my_normal(x=Final_AirFrance_df$publisher_numeric)
Final_AirFrance_df$Campaign_norm <- my_normal(x=Final_AirFrance_df$campaign_numeric)
Final_AirFrance_df$Match_type_norm <- my_normal(x=Final_AirFrance_df$match_numeric)
Final_AirFrance_df$Impressions_norm <- my_normal(x=Final_AirFrance_df$Impressions)
Final_AirFrance_df$Engine_click_norm <- my_normal(x=Final_AirFrance_df$`Engine Click Thru %`)
Final_AirFrance_df$Conversion_norm <- my_normal(x=Final_AirFrance_df$`Trans. Conv. %`)
Final_AirFrance_df$Avg_booking_norm <- my_normal(x=Final_AirFrance_df$Avg_Rev_Booking)
Final_AirFrance_df$Cost_click_norm <- my_normal(x=Final_AirFrance_df$`Avg. Cost per Click`)
Final_AirFrance_df$Avg_pos_norm <- my_normal(x=Final_AirFrance_df$`Avg. Pos.`)
Final_AirFrance_df$click_norm <- my_normal(x=Final_AirFrance_df$Clicks)
Final_AirFrance_df$keyword_group_norm <- my_normal(x=Final_AirFrance_df$keyword_group_numeric)
Final_AirFrance_df$Total_Bookings_norm <- my_normal(x=Final_AirFrance_df$`Total Volume of Bookings`)
Final_AirFrance_df$Revenue_norm <- my_normal(x=Final_AirFrance_df$Net_Rev)
Final_AirFrance_df$Click_Charges_norm <- my_normal(x=Final_AirFrance_df$`Click Charges`)
Final_AirFrance_df$Cost_Booking_norm <- my_normal(x=Final_AirFrance_df$Cost_Booking)

#Creating a logistic regression with the normalized data
Airfrance_logit <- glm(Prob_of_Booking ~ Publisher_norm + Match_type_norm + Campaign_norm + 
                       Impressions_norm + Avg_pos_norm + keyword_group_norm, 
                       data=Final_AirFrance_df,family="binomial")
summary(Airfrance_logit)

#We delete the variable of publisher and run again
Airfrance_logit <- glm(Prob_of_Booking ~ Match_type_norm + Campaign_norm + 
                         Impressions_norm + Avg_pos_norm + keyword_group_norm, 
                       data=Final_AirFrance_df,family="binomial")
summary(Airfrance_logit)

#We delete the variable of keyword group and run again
Airfrance_logit <- glm(Prob_of_Booking ~ Match_type_norm + Campaign_norm + 
                         Impressions_norm + Avg_pos_norm, 
                       data=Final_AirFrance_df,family="binomial")
summary(Airfrance_logit)

#We delete the variable of match type and see a change in the final regression
Airfrance_logit <- glm(Prob_of_Booking ~ Campaign_norm + 
                         Impressions_norm + Avg_pos_norm, 
                       data=Final_AirFrance_df,family="binomial")
summary(Airfrance_logit)

#Conclusions of the normalized logistic regression
#There are three variables that affect the probability of booking and those are campaign, impressions and average position
exp(-0.9672)-1 #by every change in campaign made, the odds of probability of booking decreases by almost 62%
exp(179.4360)-1 #increasing the impressions by 1, the odds of probability of booking increases by 8.47%
exp(-2.4286)-1 #every change made in the average position, the odds of probability of booking decreases by 91.2%


#We create a prediction model to see if our model works in making the right predictions
library(caret)
library(e1071)

my_prediction <- predict(Airfrance_logit, Final_AirFrance_df, type="response")

confusionMatrix(data=as.factor(as.numeric(my_prediction>0.5)),
                reference=as.factor(as.numeric(Final_AirFrance_df$Prob_of_Booking)))

#We plot the prediction model
library(ROCR)

pred_val_logit <- prediction(my_prediction, Final_AirFrance_df$Prob_of_Booking)

#Runs the confusion matrices for different values of p(p=0-1). Sensitivity analysis
perf_logit <- performance(pred_val_logit, "tpr", "fpr")

plot(perf_logit, col="blue")

#We create a decision tree to analyze better the data
my_Airfrance_tree <- rpart(Prob_of_Booking ~ Campaign_norm + Impressions_norm + Avg_pos_norm,
                      data=Final_AirFrance_df,
                      method="class", cp=0.01)

rpart.plot(my_Airfrance_tree, type=1, extra=1,
           box.palette = c("red", "turquoise"),
           branch.lty=3, shadow.col = "gray")

plotcp(my_Airfrance_tree) #Best cp = 0.01

#Conclusions of tree: 
#Following the tree we can have a booking if
#1 we have a normalized impression that is not less than 0.0016, translated to the original numbers
#this would be not have an impression less than 7198.
#2 the normalized campaign is not greater or equal to 0.52, translated to the original
#this would be not having the campaign of Air France brand and French destinations.
#If it is that campaign then the  normalized impressions don't need to be less than 0.0081 or in translation
#they don't have to be less than 37068
#If it is then the normalized average position have to be equal or bigger than 0.068, translated to the original
#this would be an average position less than 1.87 
#and the normalized impression not be less than 0.0022 and not bigger or equal to 0.0025.
#if it is then you need another normalized impression not to be less than 0.0041 and not bigger or equal than 0.0055
#impression less than 24561

#It is a little bit confusing but a general conclusion is that they can't have 
#less than 7198 impressions in total for them to have an increase in bookings and it is better
#if they do not use the the campaign of Air France brand and French destinations.


#Creating another dataset to add selected variables by publisher
sum_data_Airfrance <- Final_AirFrance_df %>% group_by(`Publisher Name`) %>% summarise(Total_Bookings=sum(`Total Volume of Bookings`), Revenue=sum(Net_Rev),
                                                                                      Click_Charges=sum(`Click Charges`), roa=sum(ROA), Prob_Booking=sum(Prob_of_Booking),
                                                                                      Cost_Book=sum(Cost_Booking), Cost_click=sum(`Avg. Cost per Click`), Rev_Booking=sum(Avg_Rev_Booking),
                                                                                      Click_rate=sum(`Engine Click Thru %`), Conversion_rate=sum(`Trans. Conv. %`),na.rm=TRUE)


#Creating plots to analyze data
#Investment for each Publisher
plot1 <- plot_ly(type = "scatter",
                 sum_data_Airfrance, 
                 x = ~sum_data_Airfrance$Prob_Booking, 
                 y = ~sum_data_Airfrance$Cost_click,
                 text = ~paste("Publisher: ", rownames(sum_data_Airfrance)),
                 mode = "markers",
                 size = sum_data_Airfrance$Prob_Booking,
                 sizes = c(10,100), # Change to make bubbles bigger
                 marker = list(opacity = 1, sizemode = "diameter"),
                 color = ~sum_data_Airfrance$`Publisher Name`)

plot1 <- plot1 %>% layout(title = "Investment per Publisher",
                          xaxis = list(title = "Probability of Booking per Publisher",
                                       gridcolor = "light grey"),
                          yaxis = list(title = "Average Cost per Click per Publisher",
                                       gridcolor = "light grey"),
                          showlegend = TRUE,
                          paper_bgcolor = "white",
                          plot_bgcolor = "white")

plot1

#Conclusion of plot1: They spend more on Google US and that gives as well a higher probability of booking. 
#For US market it makes sense to stay using Google, as for a Global market they should consider using overture because 
#the investment is less than MSN and the probability of booking is almost the same.
#As for Yahoo they should stop using it completely.

#Efficiency of each publisher
plot2 <- plot_ly(type = "scatter",
                 sum_data_Airfrance, 
                 x = ~sum_data_Airfrance$Click_rate, 
                 y = ~sum_data_Airfrance$Conversion_rate,
                 text = ~paste("Publisher: ", rownames(sum_data_Airfrance)),
                 mode = "markers",
                 size = sum_data_Airfrance$Click_rate,
                 sizes = c(10,100), # Change to make bubbles bigger
                 marker = list(opacity = 1, sizemode = "diameter"),
                 color = ~sum_data_Airfrance$`Publisher Name`)

plot2 <- plot2 %>% layout(title = "Efficiency of each Publisher",
                          xaxis = list(title = "Click thru Rate",
                                       gridcolor = "light grey"),
                          yaxis = list(title = "Conversion Rate",
                                       gridcolor = "light grey"),
                          showlegend = TRUE,
                          paper_bgcolor = "white",
                          plot_bgcolor = "white")

plot2

#Conclusions of the plot efficiency of each publisher: Google US has the best efficiency,
#the click thru rate also has a big conversion rate. Which means that every click converts into a bookings.
#As for the global market the best publisher seems to be MSN because it has the highest click thru rate and conversion rate than the rest

### Correlation 
# Subsetting
Corr_AirFrance_df <- Final_AirFrance_df[,12:23]

# Correlation matrix for numeric variables
Corr_Matrix_AirFrance_df <- round(cor(Corr_AirFrance_df),2)
# Display correlation matrix
print(Corr_Matrix_AirFrance_df)

# Create correlation matrix heatmap
#install.packages("reshape2")
library(reshape2)
melted_cormat <- melt(Corr_Matrix_AirFrance_df)

get_upper_tri <- function(Corr_Matrix_AirFrance_df){
  Corr_Matrix_AirFrance_df[lower.tri(Corr_Matrix_AirFrance_df)]<- NA
  return(Corr_Matrix_AirFrance_df)
}

upper_tri <- get_upper_tri(Corr_Matrix_AirFrance_df)
upper_tri

melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

## Analysis of Match Type using Publisher and Trans. Conv. %
ggplot(Final_AirFrance_df, aes(x=`Publisher Name`, y=`Trans. Conv. %`, group=`Match Type`)) +
  geom_point(aes(color=`Match Type`, size=`Match Type`), alpha=0.4)

## Boxplot using log of Impression/Total Bookings
Final_AirFrance_df$`Impressions/Booking` <- round(log(Final_AirFrance_df$`Impressions` / Final_AirFrance_df$`Total Volume of Bookings`), digits = 2)

ggplot(Final_AirFrance_df,aes(x=Final_AirFrance_df$`Publisher Name`, y= Final_AirFrance_df$`Impressions/Booking`,fill=Final_AirFrance_df$`Publisher Name`)) + 
  geom_boxplot() 


#Importing the Kayak Dataset to make comparisons with the rest of the search engines

library(tidyverse)

Kayak_df <- read_excel("/Users/karenlarios/Desktop/Kayak.xls")
View(Kayak_df)

Kayak <- na.omit(Kayak_df)

names(Kayak) <- Kayak %>% slice(1) %>% unlist()
new_Kayak <- Kayak %>% slice(-1)

summary(new_Kayak)


#Kayak plots

Kayak_Airfrance_Revenue <- ggplot() +
  geom_point(data=sum_data_Airfrance, aes(x=`Publisher Name`, y=`Revenue`), color="blue") +
  geom_point(data= NULL, aes(x="Kayak", y=230126.86), color="orange") +
  geom_hline(yintercept = mean(sum_data_Airfrance$Revenue), color="red")

ggplotly(Kayak_Airfrance_Revenue)

#Conclusion of plot Kayak_Airfrance_Revenue: Kayaks Revenue is above the average and this revenue 
#is only from one week of observations.


Kayak_Airfrance_Bookings <- ggplot() +
  geom_point(data=sum_data_Airfrance, aes(x=`Publisher Name`, y=`Total_Bookings`), color="blue") +
  geom_point(data= NULL, aes(x="Kayak", y=208), color="orange") +
  geom_hline(yintercept = mean(sum_data_Airfrance$Total_Bookings), color="red")

ggplotly(Kayak_Airfrance_Bookings)

#Conclusion of plot the Kayak_Airfrance_Bookings: Kayak on average is having less bookings than 
#the other publishers but again this is only from one week of observations. 


