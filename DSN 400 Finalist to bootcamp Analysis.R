#setwd("C:/Users/HP/Desktop/R")
#DSN<-read.csv("dsn2021_bootcamp_finalist.CSV")
DSN <- read.csv("https://drive.google.com/uc?export=download&id=1WscVgo79N-frRkFAg8_ufRiWZ_FWhGAr")
print(DSN)

# print number of columns
print (ncol(DSN))

# print number of rows
print(nrow(DSN))
#EDA
summary(DSN)

# list the variables in mydata
names(DSN)
# print first 10 rows of mydata
head(DSN, n=10)
#Frequency

library("plyr")
count(DSN,'Gender')
#Visualization
library(ggplot2)
ggplot(DSN,title = "Gender of Finalist", aes(x=reorder(Gender, Gender, function(x) - length(x)))) +
  geom_bar( width=0.4, fill="steelblue")+
  labs(x='Gender',y="Frequency")

#AI-COMMUNITY EDA
head(DSN$AI..Community, n=10)


#View the top 10  Community that participated in the hackaton
sort(table(DSN$AI..Community),decreasing=TRUE)[1:10]
#changing the value "none" to "no commuinity"
DSN$AI..Community[DSN$AI..Community=="None"]<-"No Community"


#To visualize this using a Tree map..
#creating any new column in the dataset representing the frequency of community
DSN$AI..Community <- as.character(DSN$AI..Community)
DSN$countofcommunities <- as.numeric(ave(DSN$AI..Community, DSN$AI..Community, FUN = length))
head(DSN, n=10)

library(treemap)
#creating a new column that would serve as a labelvalue on the tree map
DSN$label <- paste(DSN$AI..Community, DSN$countofcommunities, sep = "\n")
#plotting the tree map
treemap(DSN, #Your data frame object
        index=c("label"),  #A list of your categorical variables
        vSize = "countofcommunities",  #This is your quantitative variable
        type="value", #Type sets the organization and color scheme of your treemap
        title = "Tree map to show the community with the most Finalists",#colors are determined by the index variables. Different branches in the hierarchical tree get different colors
        border.col ="white")

#Visualization on a bar

ggplot(DSN,title = "AI-COMMUNITIES", aes(x=reorder(AI..Community, AI..Community, function(x) - length(x)))) +
  geom_bar( width=0.4, fill="BROWN")+
  labs(x='AI..Community',y="Frequency")+theme(axis.text.x = element_text(angle = 90))

#Creating a new column to indicate type of community
library(stringr)

DSN$Type.of.community <- word(DSN$AI..Community, 1)

head(DSN$Type.of.community, n=10)


#Visualization with Pie-Chart

library(tidyverse)
DSN %>%
  count(Type.of.community) %>% 
  arrange(desc(Type.of.community)) %>%
  drop_na(Type.of.community) %>% 
  mutate(Type.of.community = factor(Type.of.community), percentage = n / sum(n)) %>% 
  ggplot(aes(x = "", percentage, fill = Type.of.community)) +
  geom_col(color="white") +
  coord_polar(theta = "y", start=0) +
  labs(title = "Finalist Community type") +
  geom_label(aes(label = scales::percent(percentage), x = 1.3),
             position = position_stack(vjust = 0.5),
             color = "black",
             fill="#69b3a2")

theme_void()



