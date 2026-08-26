df <- read.csv('data/vgsales.csv')



head(df)
colnames (df)
summary(df)



#target --> 
#1: IDENTIFY POPULAR GENRES BY AMOUNT OF SALES IN NA, EU, JP, OTHER, AND GLOBAL

#let's see which genres are recorded 
unique(df$Genre) -> #Sports, Platform, Racing, Role-Playing, Puzzle, Misc, Shooter, Simulation
#, Action, Fighting, Adventure, Strategy
table(df$Genre) #shows how much each genre is recorded in rows throughout the data set
#there are a lot of action games 
  
  

#--North America--


#first, find the sum of NA_Sales across each genre 

na_genre_sales <- tapply(df$NA_Sales, df$Genre, sum)
na_genre_sales

#barplot containing the number of sales for each video game genre in North America
barplot(na_genre_sales, main= "Top Selling Genres in North America", xlab = "Genre", ylab="Sales (Millions)", col ="darkred")

#Assumption: shooters would be the most popular 
#Reality: action is the most popular based on sales



#--Japan--

#find the sum of JP_Sales across each genre 
jp_genre_sales <- tapply(df$JP_Sales, df$Genre, sum)
jp_genre_sales

#barplot containing the number of sales for each video game genre in Japan
barplot(jp_genre_sales, main= "Top Selling Genres in Japan", xlab = "Genre", ylab="Sales (Millions)", col ="darkblue")

#Assumption: action would be popular since Japan is a popular video game market and in North America and globally action has the most sales, so in Japan
#it is probably similar
#Reality: RPGs are more popular based on sales 




#--Europe--

#find the sum of EU_Sales across each genre 
eu_genre_sales <- tapply(df$EU_Sales, df$Genre, sum)
eu_genre_sales

#barplot
barplot(eu_genre_sales, main= "Top Selling Genres in Europe", xlab = "Genre", ylab="Sales (Millions)", col ="forestgreen")

#action is also popular 


#--Other Countries--

#find the sum of Other_Sales across each genre 
other_genre_sales <- tapply(df$Other_Sales, df$Genre, sum)
other_genre_sales

#barplot
barplot(other_genre_sales, main= "Top Selling Genres in Other Countries", xlab = "Genre", ylab="Sales (Millions)", col ="purple")

#action is also popular 


#--Global--


#find the sum of Global_Sales across each genre 
global_genre_sales <- tapply(df$Global_Sales, df$Genre, sum)
global_genre_sales

#barplot
barplot(global_genre_sales, main= "Top Selling Genres Globally", xlab = "Genre", ylab="Sales (Millions)", col ="pink")

#action is most popular in terms of global sales




#2: IDENTIFY POPULAR PUBLISHERS BY AMOUNT OF SALES IN NA, EU, JP, OTHER, AND GLOBAL

#while I tried to identify the top 13 publishers in NA, EU, and JP, I noticed that they were mostly Nintendo and thought that Nintendo's ACTION games would
#be top selling; however, that was not the case



#--North America--

#found the top 13 games in North America using the subset function
top13_df_NA <- subset(df, NA_Sales > 10)
#display the subset 
top13_df_NA

#use tapply to find the total sum of every publisher's sales in North America FROM the top13_df_NA subset

NA_by_publishers <- tapply(top13_df_NA$NA_Sales, top13_df_NA$Publisher, sum)
NA_by_publishers #display

#create a  barplot of the publishers of top 13 games in NA by global sales by the tapply NA_by_publishers 
barplot(NA_by_publishers, main = "Publishers of Top 13 Games in NA by Global Sales", xlab= "Publisher", ylab ="Sales (Millions)", col = "darkred")

#I noticed from the barplot that Nintendo dominated the top 13 chart...but in the tapply I also noticed that 
#none of them were action games. I had assumed that Nintendo's action games would dominate in the top 13
#since action games are popular...so this was unexpected to me.

#display only Nintendo sales in top 13 using subset
top13_df_NA_Nintendo <- subset(top13_df_NA, Publisher == "Nintendo")
top13_df_NA_Nintendo

#lets visualize what I noticed in the tapply into the barplot
top13_df_NA_Nintendo_Genre <- tapply(top13_df_NA_Nintendo$NA_Sales, top13_df_NA_Nintendo$Genre, sum)
barplot(top13_df_NA_Nintendo_Genre, main = "Nintendo Sales by Genre (subset from Top 13 games in NA)", xlab= "Genre", ylab ="Sales (Millions)", col = "darkred")

#so Nintendo's best came from platform and sports. Even here I saw puzzle games, which is unexpected because the
#less popular genre made the top 13 while action never did.





#--Europe--

#found the top 13 games in Europe using the subset function
top13_df_EU <- subset(df, EU_Sales > 7)
#display the subset 
top13_df_EU

#use tapply to find the total sum of every publisher's sales in Europe FROM the top13_df_EU subset
EU_by_publishers <- tapply(top13_df_EU$EU_Sales, top13_df_EU$Publisher, sum)

#create a  barplot of the publishers of top 13 games in EU by global sales using tapply EU_by_publishers 
barplot(EU_by_publishers, main = "EU Sales by Publishers", xlab= "Publisher", ylab ="Count", col = "darkblue")

# Like North America, Nintendo heavily dominated the top 13 games sold in the EU


#display only Nintendo sales in top 13 using subset
top13_df_EU_Nintendo <- subset(top13_df_EU, Publisher == "Nintendo")
top13_df_EU_Nintendo


#lets visualize what I noticed in the tapply into the barplot
top13_df_EU_Nintendo_Genre <- tapply(top13_df_EU_Nintendo$EU_Sales, top13_df_EU_Nintendo$Genre, sum)
barplot(top13_df_EU_Nintendo_Genre, main = "Nintendo Sales by Genre from Top 13 games in EU", xlab= "Genre", ylab ="Sales (Millions)", col = "darkblue")

#Sports games by Nintendo in Europe made it to the top 13, while action never did. This is the same as North America, which is 
#unexpected. I would have thought that because action is the top-selling genre in Europe and Nintendo is a top publisher compared
#to the other publishers in this dataset, Nintendo's action games would make the top 13. However, instead, its sports and other games 
#such as racing, simulation, and puzzle make it instead. 






#--Japan--

#found the top 13 games in Japan using the subset function
top13_df_JP <- subset(df, JP_Sales > 4.34)
#display the subset 
top13_df_JP

#use tapply to find the total sum of every publisher's sales in Japan FROM the top13_df_JP subset
JP_by_publishers <- tapply(top13_df_JP$JP_Sales, top13_df_JP$Publisher, sum)

#create a  barplot of the publishers of top 13 games in Japan by global sales by the tapply JP_by_publishers 
barplot(JP_by_publishers, main = "JP Sales by Publishers in Top 13", xlab= "Publisher", ylab ="Sales (Millions)", col = "pink")
top13_df_JP_Nintendo <- subset(top13_df_JP, Publisher == "Nintendo")
#display
top13_df_JP_Nintendo

#lets visualize what I noticed in the tapply into the barplot
top13_df_JP_Nintendo_Genre <- tapply(top13_df_JP_Nintendo$JP_Sales, filtered_df_JP_Nintendo$Genre, sum)
barplot(top13_df_JP_Nintendo_Genre, main = "Nintendo Sales in Japan by Genre (From Top 13 Top Selling Games)", xlab= "Genre", ylab ="Sales (Millions)", col = "pink")

#this is interesting because I would've thought that the same genres for Nintendo (for North America Platform and Sports for Nintendo were popular) would be the same also in JP,
#but *maybe* the publisher doesn't really matter but rather the genre





#--Other--
#not used in article

top13_df_Other <- subset(df, Other_Sales > 2.30)
top13_df_Other

Other_by_publishers <- tapply(top13_df_Other$Other_Sales, top13_df_Other$Publisher, sum)
barplot(Other_by_publishers, main = "Other Sales by Publishers", xlab= "Publisher", ylab ="Sales (Millions)", col = "darkgreen")
top13_df_Other_Nintendo <- subset(top13_df_Other, Publisher == "Nintendo")
top13_df_Other_Nintendo

top13_df_Other_Nintendo_Genre <- tapply(top13_df_Other_Nintendo$Other_Sales, top13_df_Other_Nintendo$Genre, sum)
barplot(top13_df_Other_Nintendo_Genre, main = "Nintendo Sales in Other by Genre(From Top 13 Top Selling Games)", xlab= "Genre", ylab ="Sales (Millions)", col = "darkgreen")


#--Global--
#will focus on NA and JP instead



#for the article I decided to focus on the contrast between North America and Japan regarding Nintendo's top selling genre instead of EU_Sales, Other_Sales, and Global_Sales


#3: identify popular platforms by amount of sales in NA, EU, JP, Other, and Global

platforms_NA <- tapply(df$Global_Sales, df$Year, sum)
platforms_NA

#2008 is the most popular in terms of how many video games were sold

#subsetting to only games with publisher Nintendo
df_Nintendo <- subset(df, Publisher == "Nintendo")

#subsetting to only NA sales whose publisher is Nintendo and only action games 
df_Nintendo_NA_Sales_Action <- subset(df_Nintendo, Genre == "Action")
#display
df_Nintendo_NA_Sales_Action
#using tapply to find the sum of NA_Sales across all genres with the subset df_Nintendo_NA_Sales_Action
tapply(df_Nintendo_NA_Sales_Action$NA_Sales, df_Nintendo_NA_Sales_Action$Genre, sum)

df_Nintendo <- subset(df, Publisher == "Nintendo")

df_Nintendo_NA_Sales_Action <- subset(df_Nintendo, Genre == "Action")
df_Nintendo_NA_Sales_Action

df_NA <- subset(df, Genre == "Action")
df_NA

#show Nintendo's most popular platforms they launched games on
#does genre matter when choosing a platform? 


#4:TOTAL GLOBAL SALES BY YEAR (BAR PLOT)

yearly_sales <- tapply(df$Global_Sales, df$Year, sum)
yearly_sales
yearly_sales <- subset(yearly_sales, sales_by_year > 0)
yearly_sales

#barplot for total global sales by year 
barplot(sales_by_year, main="Global Sales by Year",xlab= "Year", ylab="Sales", col="darkred" )

#I noticed that, in particular, 2008 was high in global sales

#remove NA values 
years_games_clean <- subset(df, !is.na(Year)) #looked up how to use !is.na in ChatGPT as I noticed some years were NA and did not want to include that
years_games <- table(years_games_clean$Year)
years_games

#barplot for total games developed per year 

barplot(years_games, main="Total Games Created by Year",xlab= "Year", ylab="Sales", col="darkblue" )

#2008 and 2009 saw the most games developed


# Decided not to focus on year column because it was only from 1980-2016. However, it can be used as historical data that can give insight into 
#trends related to publishers and genres (my other interesting findings).

#5: PLATFORMS VS. GENRE? (MOSAIC PLOT)

unique(df$Platform)
colors1 <- c('red', 'blue', 'cyan', 'yellow', 'green', 'orange') #code taken from data101 active textbook for help 
mosaicplot(df$Platform~df$Genre, xlab='Platform', ylab ='Genre', main="Mosiac Plot of Platform vs Genre", col=colors1, border="black") #code taken from data101 active textbook for help


#6: POPULAR ACTION GAMES 

popular_action <- subset(df, Genre== "Action")
#displays ALL action games
popular_action

#I noticed over here that Nintendo's action game sales in North America, Global, and Europe are not as high as I thought. highest is 4.10 sales in North America so maybe there are more action games produced
#but that have very little sales, whereas in the top 13 for all these countries, the sports and platform games that made the top 13 were the ones that made up much of 
#the sales for sports games in general (basically outliers)

