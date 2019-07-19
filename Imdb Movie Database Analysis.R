#Welcome to the Imdb Movie Database Analysis

#loading readxl

library(readxl)

#loading the Imdb Movie Database Through readxl package

Imdb <- readxl::read_xlsx("D:\\R\\IMDd-Movie-Database.xlsx",sheet = 1)

#structure of Imdb file

str(Imdb)

#loading dplyr

library(dplyr)

#filtering on the basis of language and color and selecting the required columns of interest.

Imdb_subset <- Imdb %>% filter(Language=="English",`Color/B&W`=="Color") %>% select(`IMDb Score (1-10)`,`Total Reviews`,`Cast FB Likes`,`Gross Revenue`,Budget,Rating,Genre,Country)

#structure of the subset created
str(Imdb_subset)

#Checking the Correlation B/W the Gross Revenue and Budget

cor(Imdb_subset$Budget,Imdb_subset$`Gross Revenue`)

#We see There is a +ve association,let's sketch the scatterplot for better feel for it.

#Scatterplot to see any association between Movie budget and Gross Revenue.

plot(Imdb_subset$Budget,Imdb_subset$`Gross Revenue`,
     xlab="Budget of the Movie",ylab = "Gross Revenue"
     ,main = "Association B/W Budget \n and Gross Revenue", 
     pch = 20, col = "gray")


#Putting a regression line over the plot

abline(lm(Imdb_subset$`Gross Revenue`~Imdb_subset$Budget),col="red")

lines(lowess(Imdb_subset$Budget,Imdb_subset$`Gross Revenue`),col="Blue")

reg <- lm(Imdb_subset$`Gross Revenue`~ Imdb_subset$Budget)

summary(reg)

# We see that if movie budget is high the revenue earned is high upto some point,
#If we just keep on inreasing movie budget that does not mean it will earn more,
#even low budget movies have done great in the box office
#High budget movies have earned more gross revenue but
#Very high budget movies have earned avg or Below avg revenue


#Lets plot the correlation Matrix

#For this I'm gonna install the 'Psych' package

install.packages("psych")

library(psych)
names(Imdb_subset)

pairs.panels(Imdb_subset[c(4,5,3,6,1)], gap=0)


#the no. specified in the function pairs. panels are the order in which columns are placed in the data set
#the first No. 4 represents the Gross revenue, it is the prime Variable in which we are interested
#All the other variables will be used to find which affecting gross revenue the most 

#we see only Budget and Gross revenue have a significant Correlation Coefficient


#Hypothesis Testing

#Null Hypothesis: The Variable Color/BW Does Not affect the Gross Revenue
#Alternative Hypothesis: The Variable Color/Bw Does affect the Gross Revenue


t.test(Imdb$`Gross Revenue`~ Imdb$`Color/B&W`)

#We see that the p value is Signifacant showing there is a difference in Revenue earned Bwteen Colr and B/W Movies
#The obvious reason is that the B/W movies were Older so there gross revenue is Less

#Now lets see How Languages affect Gross Revenue by using anova

anova_Languages<- aov(Imdb$`Gross Revenue` ~ Imdb$Language)

summary(anova_Languages)

#the p value is very low, implying there is a statisticaly significant differnce between Different Languages and there Gross Revenue

#there are 2 movies for which there is no Language,we can safely assume that these are the movies for which sign language is used
