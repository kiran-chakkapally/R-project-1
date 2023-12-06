# R Programming by Kiran Kumar Chakkapally

# Importing the data set in to the R Studio 
stats<- read.csv(file.choose())  # We have choosen the US_state_data manually by using choose function

stats  
nrow(stats)
ncol(stats)
head(stats)# Reading the first six rows
tail(stats)

#-----------------------------------------------
# here we observe that there are many empty columns in the data set so lets delete those columns 
is.data.frame(stats)
# Lets remove the columns that have null values 
stats$art<-NULL
stats$facebook<- NULL
stats$entrepreneurship<-NULL
stats$privacy<-NULL
stats$intsagram<- NULL
stats$twitter<- NULL
stats$gdpr<- NULL
stats$loans<-NULL
stats$university<-NULL
#-------------- We have removed all the empty columns
head(stats,n=10)


# Lets get the Arts
arts<-read.csv(file.choose())
arts
colnames(arts)
head(arts)
tail(arts)

 ## Manually selecting the file is a bit lenghty process lets get the data by method - 2
# Method 2 ( choosing the working directory)
# lets get the facebook data 

 getwd()
 #  "C:/Users/chkir/OneDrive - Central Michigan University/Documents" 
 # I have my files in the same location so i dont have to setup directory
 
 
facebook<-read.csv("Facebook.csv")
facebook
head(facebook)
tail(facebook)


# lets get the entrepreneurship data
entrepreneurship<-read.csv("Entrepreneurship.csv")
entrepreneurship
head(entrepreneurship)
tail(entrepreneurship)


# Lets get the Privacy
privacy<-read.csv("privacy.csv")
privacy
head(privacy)
tail(privacy)



# Lets get the instagram

instagram<-read.csv("instagram.csv")
instagram
head(instagram)
tail(instagram)


# Lets get the twitter
twitter<-read.csv("twitter.csv")
twitter
head(twitter)
tail(twitter)


# Lets get the GDPR
gdpr<-read.csv("gdpr.csv")
gdpr
head(gdpr)
tail(gdpr)


# Lets get the Loans
loans<-read.csv("loans.csv")
loans
head(loans)
tail(loans)

# Lets get the university

university<-read.csv("university.csv")
university
head(university)
tail(university)

 
# Lets merge the files now 
# merged= stats+arts+entrepreneurship+facebook+gdpr+instagram+loans+privacy+twitter+university


head(stats)
head(arts)

# But how do we do that , we do it using the merge function
 merged1<- merge(stats,arts,by.x = "State",by.y="State")
merged1

merged2<- merge(merged1,facebook,by.x = "State",by.y="State")
merged2


merged3<- merge(merged2,entrepreneurship,by.x = "State",by.y="State")
merged3



merged4<- merge(merged3,privacy,by.x = "State",by.y="State")
merged4


merged5<- merge(merged4,instagram,by.x = "State",by.y="State")
merged5


merged6<- merge(merged5,twitter,by.x = "State",by.y="State")
merged6


merged7<- merge(merged6,gdpr,by.x = "State",by.y="State")
merged7




merged8<- merge(merged7,loans,by.x = "State",by.y="State")
merged8


merged9<- merge(merged8,university,by.x = "State",by.y="State")
merged9
Merged_final<-merged9
Merged_final
head(Merged_final)

# we have successfully completed merging 
# Now its time to normalize the data between 0 to 1

ncol(Merged_final)
# We normalise the data by using the min&max function.
summary(Merged_final)

# Min Max normalization
min_max <- function(x) {
  res <- (x - min(x)) / (max(x) - min(x))
  return(res)
}

newdata<-as.data.frame(sapply(Merged_final[,4:17],min_max))
newdata
summary(newdata)
stats
newdata <- cbind(stats[, 1:3], newdata)
head(newdata)
  

##--------Visualization Part starts here -------------------------#

newdata
str(newdata)
summary(newdata)


##--------Installing Packages---------------#
install.packages("ggplot2")
install.packages("munsell")
library(ggplot2)
library(tidyr)


## Adding geometry
ggplot(data = newdata,aes(x=psychRegions,y=State))+
  geom_point()

# Adding colour
ggplot(data = newdata,aes(x=psychRegions,y=State,colour=extraversion))+
  geom_point()

# Adding size
ggplot(data = newdata,aes(x=psychRegions,y=State,colour=extraversion,
                          size=0.5))+
  geom_point()

##### How to plot with layers------------

p<-ggplot(data = newdata,aes(x=psychRegions,y=State,colour=extraversion,
                             size=extraversion))
p


# Lets set the geometry
#point
p + geom_point()

# Lines
p + geom_boxplot()

## multiples layers

p+geom_point() + geom_boxplot()
rm(p)
#-----------------------------------------------------#
# Scatter plot of State vs psychregions 


k<-ggplot(data = newdata, aes(x = psychRegions, y = State, color = extraversion, size = 0.5)) +
  geom_point() +
  labs(title = "Scatterplot of State vs. psychRegions",
       x = "psychRegions",
       y = "State",
       color = "Extraversion") +
  scale_color_gradient(low = "blue", high = "red") +  # Adjust color scale if needed
  theme_minimal()  # You can customize the theme as per your preference

k

#-----------------------------------#
# Calculate the correlation between 'extraversion' and 'agreeableness'
correlation_value <- cor(newdata$extraversion, newdata$agreeableness)

# Create a scatter plot to visualize the correlation
ggplot(newdata, aes(x = extraversion, y = agreeableness)) +
  geom_point() +
  labs(
    title = "Scatter Plot for Correlation",
    x = "Extraversion",
    y = "Agreeableness",
    subtitle = paste("Correlation:", round(correlation_value, 2))
  )




















