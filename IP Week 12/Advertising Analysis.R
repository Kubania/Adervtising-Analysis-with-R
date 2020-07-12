
# Load the dataset
advertising <- read.csv("~/Documents/R markdowns/advertising.csv")
head(advertising)

# The shape of the dataset
dim(advertising)
# (1000,10)

# The Data Types of 
str(advertising)

#----------------------------------- Tidying the Dataset-----------------------------------------

#Missing Data
colSums(is.na(advertising))
# No missing values

#Duplicated Values 
advert <- unique(advertising)
dim(advert)
# No duplicated values

#Outliers

boxplot(advert$Daily.Time.Spent.on.Site,xlab="Daily Time Spent on Site",main="Boxplot on Daily Time Spent on Site")
boxplot(advert$Age, xlab="Age",main="Boxplot on Age")
boxplot(advert$Area.Income,xlab="Area Income",main="Boxplot on Area Income")
boxplot(advert$Daily.Internet.Usage, xlab="Daily Internet Usage", main="Boxplot of Daily Internet Usage")
boxplot(advert$Male, xlab="Male", main="Boxplot of Male")
boxplot(advert$Clicked.on.Ad, xlab="Clicked on Ad", main="Boxplot of Clicked on Ad")

# The Area Income is the only column with outliers

# The number of outliers
boxplot.stats(advert$Area.Income)$out
sum(table(boxplot.stats(advert$Area.Income)$out))

#--------------------------------------------Analyis-----------------------------------------------

# Univariate Analysis

#Numeric Values
num = advert[,c(1,2,3,4,7,10)]
summary(num)

#Mean
mean(advert$Daily.Time.Spent.on.Site)
mean(advert$Age)
mean(advert$Area.Income)
mean(advert$Daily.Internet.Usage)
mean(advert$Male)
mean(advert$Clicked.on.Ad)

# Median
median(advert$Daily.Time.Spent.on.Site)
median(advert$Age)
median(advert$Area.Income)
median(advert$Daily.Internet.Usage)
median(advert$Male)
median(advert$Clicked.on.Ad)

# Mode
getMode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getMode(advert$Daily.Time.Spent.on.Site)
getMode(advert$Age)
getMode(advert$Area.Income)
getMode(advert$Daily.Internet.Usage)
getMode(advert$Male)
getMode(advert$Clicked.on.Ad)

#Variance
var(advert$Daily.Time.Spent.on.Site)
var(advert$Age)
var(advert$Area.Income)
var(advert$Daily.Internet.Usage)
var(advert$Male)
var(advert$Clicked.on.Ad)

# Standard Deviation
sd(advert$Daily.Time.Spent.on.Site)
sd(advert$Age)
sd(advert$Area.Income)
sd(advert$Daily.Internet.Usage)
sd(advert$Male)
sd(advert$Clicked.on.Ad)


# Quantiles
quantile(advert$Daily.Time.Spent.on.Site)
quantile(advert$Age)
quantile(advert$Area.Income)
quantile(advert$Daily.Internet.Usage)
quantile(advert$Male)
quantile(advert$Clicked.on.Ad)

#Histogram
hist(advert$Daily.Time.Spent.on.Site, breaks = 20,main ="Daily Time Spent on Site", col = "dodgerblue")
hist(advert$Age, breaks = 10,main = "Age",col = "dodgerblue")
hist(advert$Area.Income, breaks = 20, main = "Area Income",col = "dodgerblue")
hist(advert$Daily.Internet.Usage,breaks = 20,main = "Daily Internet Usage",col = "dodgerblue")
hist(advert$Male,breaks = 10,main = "Male",col = "dodgerblue")
hist(advert$Clicked.on.Ad,breaks = 10,main = "Clicked on Ad",col = "dodgerblue")

# the Male and the Clicked on Ad are categorical in nature
# the distribution of Age  is skewed to the left, the Area Income and Daily Time Spent on Site distribution is skewed to the right

# Bar Plot
male <- table(advert$Male)
barplot(male,main = "Male")

clicked <- table(advert$Clicked.on.Ad)
barplot(clicked,main = "Clicked on Ad")

par(las=2, cex.axis=0.7)
country <- table(advert$Country)
barplot(sort(country[1:40], decreasing = TRUE), main = "Country",)

par(las=2)
age <- table(advert$Age)
barplot(sort(age[1:20], decreasing = TRUE), main = "Age",)

library("dplyr")
library("ggplot2")
# group by gender/Male
by_time <- advert %>% 
  group_by(Male) %>% 
  summarise(Total.Time.Spent.on.Site = sum(Daily.Time.Spent.on.Site))
by_time
p <- ggplot(by_time, aes(x = factor(Male), y = Total.Time.Spent.on.Site, fill = factor(Male)))+geom_bar(stat="identity")
p + scale_fill_discrete(name = "Male", labels = c("Female","Male"))+ labs(title="Gender that spends more time on the Internet", x="Gender")

clicked_ad <- advert[advert$Clicked.on.Ad == 1,]
clicked_ad

# Group by country, Country with more clicks on Ad
by_country <- advert %>% group_by(Country) %>% summarise(clicked.ad =sum(Clicked.on.Ad[Clicked.on.Ad == 1]))
by_country

rows <- by_country[1:20,]
rows

c <- ggplot(rows, aes(x = reorder(Country,clicked.ad), y=clicked.ad)) + geom_col() +coord_flip() +  geom_bar(stat="identity")
c

# Females that click on ads
by_gender <- advert %>% group_by(Clicked.on.Ad) %>% summarise(gender = length(Male[Male == 0]))
by_gender

females <- ggplot(by_gender, aes(x = factor(Clicked.on.Ad), y = gender, fill=factor(Clicked.on.Ad))) + geom_bar(stat="identity")
females + scale_fill_discrete(name = "Ad Clicked", labels = c("Not Clicked","Clicked"))+ labs(title="Females that Clicked vs Not Clicked", x="Clicked on Ad", y="No. of Females")

# Males that clicked on ads

by_males <- advert %>% group_by(Clicked.on.Ad) %>% summarise(gender = length(Male[Male == 1]))
by_males

males <- ggplot(by_males, aes(x = factor(Clicked.on.Ad), y = gender, fill=factor(Clicked.on.Ad))) + geom_bar(stat="identity")
males + scale_fill_discrete(name = "Ad Clicked", labels = c("Not Clicked","Clicked"))+ labs(title="Males that Clicked vs Not Clicked", x="Clicked on Ad", y="No. of Males")

# Ad topic Line
value.count <- table(advert$Ad.Topic.Line)
value.count

# Only one occurrence of every topic line


# Bivariate Analysis
str(advert)

numeric_col <- advert[,c(1,2,3,4,7,10)]
head(numeric_col,4)

# Covariance
covariance_matrix = cov(numeric_col)
View(round(covariance_matrix,2))

# Correlation Matrix
correlation_matrix = cor(numeric_col)
View(round(correlation_matrix,2))

# Scatter Plot
area.income <- advert$Area.Income
internet.usage <- advert$Daily.Internet.Usage
time.spent <- advert$Daily.Time.Spent.on.Site
age <- advert$Age

time.stamp <- as.character(advert$Timestamp)
head(time.stamp)

advert$mydate <- strptime(x= time.stamp, format = "%Y-%m-%d %H:%M:%S")
advert$mydate

plot(area.income, internet.usage, xlab="Area Income",ylab = "Daily Internet Usage")
plot(area.income,time.spent,xlab = "Area Income",ylab = "Daily Time Spent on the Internet")
plot(time.spent,internet.usage, xlab="Daily Time spent", ylab="Daily Internet Usage")
plot(age,time.spent,, xlab="Age", ylab="Daily Internet Usage")
