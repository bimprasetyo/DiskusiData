
library(ggplot2)
library(class)
library(caret)
library(e1071)


mush<- read.csv('C:/Users/Bukalapak/Documents/DD/Data/mushrooms.csv')
data=mush

str(data)

head(data)

attach(data)

for (i in 1:ncol(data))
{print(colnames(data)[i])
 a=table(data[,i])
cat(round(prop.table(a),2))
 cat('\n')
 cat('\n')
}

#LoadPackage for EDA
library(DataExplorer)

#Check if all the column is present
plot_missing(data, title="Missing Values on each of the features")

#Cramer's V to check the association between the response and the features.
library(vcd)
library(corrplot)

# Association Measurement
# remove column 17 (because it only contain 1 level) 
cate= data[,-17]
# Initialize empty matrix to store coefficients
empty_m <- matrix(ncol = length(cate),
                  nrow = length(cate),
                  dimnames = list(names(cate), 
                                  names(cate)))



# Function that accepts matrix for coefficients and data and returns a correlation matrix
calculate_cramer <- function(m, df) {
  for (r in seq(nrow(m))){
    for (c in seq(ncol(m))){
      m[[r, c]] <- assocstats(table(df[[r]], df[[c]]))$cramer
    }
  }
  return(m)
}

cor_matrix <- calculate_cramer(empty_m ,cate)
corrplot(cor_matrix,type = "upper", method = "color")

set.seed(1)
trainIndex<-createDataPartition(data$class, p=.8, list=FALSE)
Train<-data[trainIndex,]
Test<-data[-trainIndex,]


model_nv1<-naiveBayes(class~., data=Train, laplace=1)
pred<-predict(model_nv1, newdata=Test)
confusionMatrix(data=pred, reference=Test$class, positive="e")

data2=cate
trainIndex2<-createDataPartition(data2$class, p=.8, list=FALSE)
Train2<-data2[trainIndex2,]
Test2<-data2[-trainIndex2,]

model_nv2<-naiveBayes(class~., data=Train2, laplace=1)
pred2<-predict(model_nv2, newdata=Test2)
confusionMatrix(data=pred2, reference=Test2$class, positive="e")

data3=cate[,-7]
trainIndex3<-createDataPartition(data3$class, p=.8, list=FALSE)
Train3<-data3[trainIndex3,]
Test3<-data3[-trainIndex3,]

model_nv3<-naiveBayes(class~., data=Train3, laplace=1)
pred3<-predict(model_nv3, newdata=Test3)
confusionMatrix(data=pred3, reference=Test3$class, positive="e")
