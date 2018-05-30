library(readr)
Breast_Cancer_csv <- read_csv("C:/Users/ACER/Downloads/Breast_Cancer.csv.csv")
View(Breast_Cancer_csv)
wbcd<-Breast_Cancer_csv
str(wbcd)
wbcd <- wbcd[-1]
table(wbcd$diagnosis)
wbcd$diagnosis<- factor(wbcd$diagnosis, levels = c("B", "M"),
                        labels = c("Benign", "Malignant"))
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 2)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))}
normalize(c(1,2,3,4,5))
wbcd_n<-as.data.frame(lapply(wbcd[,c(2:31)],normalize))
set.seed(1000)
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(wbcd_train,wbcd_test,wbcd_train_labels, k = 21)
