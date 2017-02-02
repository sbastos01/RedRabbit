

# Get IRIS dataset
install.packages("RCurl")
library(RCurl)
iris_url = "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris_txt = getURL(iris_url)
iris_data = read.csv(textConnection(iris_txt), header = FALSE)

#Rename the columns
library(plyr)
names(iris_data)
iris = rename(iris_data, c("V1"="Sepal_Length", "V2"="Sepal_Width","V3"="Petal_Length", "V4"="Petal_Width", "V5"="Species"))
names(iris)

#Fit the model
library(e1071)
fit <- naiveBayes(Species~., data=iris) 

#Summarize model accuracy
table(predictions, iris$Species)

#Do a prediction using the model and a row of data
predict <- predict(fit, c(5.1,3.5,1.4,0.2))
predict[4]


#Summarize model accuracy
predictSpecies <- function(sepalLength, sepalWidth, petalLength, petalWidth)
{
predictDF <- predict(fit, data.frame("Sepal_Length" = sepalLength, "Sepal_Width" = sepalWidth, "Petal_Length" = petalLength, "Petal_Width" = petalWidth), type="raw")
return(colnames(predictDF)[apply(predictDF, 1, which.max)])
}

predictSpecies(5.1,3.5,1.4,0.9)







