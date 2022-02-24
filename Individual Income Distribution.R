#Project - 05
# An individual's annual income results from various factors. Intuitively, it is influenced by the individual's education level, age, gender, occupation, and etc.
# Draw prediction of salary class of person based upon given info.

#import the data
adult_info = read.csv("adult.csv")
print(adult_info)
#View(adult_info) 
#print(summary(adult_info))

#Data Cleaning
## work class combining

table(adult_info$workclass)

#combining like factors of workclass
adult_info$workclass = as.character(adult_info$workclass)
adult_info$workclass[adult_info$workclass == "Without-pay"|
                       adult_info$workclass == "Never-worked"] = "Unemployed"
adult_info$workclass[adult_info$workclass == "State-gov" |
                       adult_info$workclass == "Local-gov"] = "SL-gov"
adult_info$workclass[adult_info$workclass == "Self-emp-inc" |
                       adult_info$workclass =="Self-emp-not-inc"] = "Self-employed"


##Marital Status Combining
table(adult_info$marital.status)

adult_info$marital.status[adult_info$marital.status =="Married-AF-spouse"|
                               adult_info$marital.status == "Married-civ-spouse" |
                               adult_info$marital.status == "Married-spouse-absent"] = "Married"
adult_info$marital.status[adult_info$marital.status == "Divorced"|
                               adult_info$marital.status == "Separated"|
                               adult_info$marital.status == "Widowed"] = "Not-Married"
table(adult_info$marital.status)

##Country Combining
adult_info$native.country = as.character(adult_info$native.country)
table(adult_info$native.country)

north.america = c("Canada",'Cuba'," Dominican-Republic","El-Salvador","Guatemala","Haiti","Mexico","Nicaragua","Dominican-Republic","Honduras","Jamaica",
                  "Outlying-US(Guam-USVI-etc)","Puerto-Rico","Trinadad&Tobago","United-States")
asia = c("Cambodia","China","Hong","India","Iran","Japan","Laos","Philippines",
         "Taiwan","Thailand","Vietnam")
south.america = c("Columbia","Ecuador","Peru")
europe = c("England","France","Germany","Greece","Holand-Netherlands","Hungary","Ireland","Italy","Poland","Portugal","Scotland","Yugoslavia")
other = c("South","?")

adult_info$native.country[adult_info$native.country %in% north.america] = "North America"
adult_info$native.country[adult_info$native.country %in% asia] = "Asia"
adult_info$native.country[adult_info$native.country %in% south.america] = "South America"
adult_info$native.country[adult_info$native.country %in% europe] = "Europe"
adult_info$native.country[adult_info$native.country %in% other] = "other"

table(adult_info$native.country)

##Dealing with missing data - ? is missing data
table(adult_info$workclass)
adult_info[adult_info == "?"] = NA
table(adult_info$workclass)

#Missmap - draw map of missingness in a dataset using image function
#install.packages("Amelia")
library(Amelia)
png("missmap.png")
missmap(adult_info,y.at = 1, y.labels = "",col = c("yellow","black"), legend = FALSE)
dev.off()

#So from graph it seems to that occupation and workclass have NA values
#so remove it
adult_info = na.omit(adult_info)
missmap(adult_info,y.at = 1, y.labels = "",col = c("yellow","black"), legend = FALSE)

#Exploratory data analysis
#install.packages("ggplot2")
library(ggplot2)
#plot histogram where ages colored by income
png("Distribution of age with income.png")
ggplot(adult_info,aes(age)) +geom_histogram(aes(fill = income),color = "black",binwidth = 1)
dev.off()

#histogram of hours worked per week
png("Histogram of hours worked per week.png")
ggplot(adult_info,aes(hours.per.week)) +geom_histogram()
dev.off()

#Income class by region
##need to change the country name to region
##install.packages("data.table")
library(data.table)
setnames(x = adult_info,old = "native.country",new = "region",skip_absent = TRUE)

##reorder factor levels by count
region.ordered = reorder(adult_info$region, adult_info$region,length)
region.ordered = factor(region.ordered, levels = rev(levels(region.ordered)))

png("income class by region.png")
ggplot(adult_info,aes(region.ordered)) +geom_bar(aes(fill = income), color = "black")
dev.off()

#Building model
##the purpose of model is to classify into two group for incoem below 50k and greater 50k

##split the train and test data
split = sample.split(adult_info, SplitRatio = 0.7)
train_info = subset(adult_info, split = TRUE)
test_info = subset(adult_info, split = FALSE)

##Training the model - logistic regression
log.model = glm(formula = income ~ .,data = train_info,family = binomial())

##Prediction
prediction = predict(log.model,test_info,type = "response")

###confusion matrix
table(test_info$income,prediction >=0.5)

