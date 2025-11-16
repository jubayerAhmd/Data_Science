# ======= INSTALL PACKAGES =======
install.packages("dplyr")
install.packages("tidytext")
install.packages("tm")
install.packages("corrplot")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("dbscan")
install.packages("factoextra")
install.packages("qdapDictionaries")
install.packages("textclean")
install.packages("stringr")
install.packages("tokenizers")
install.packages("hunspell")
install.packages("stopwords")
install.packages("textstem")
install.packages("stringi")
install.packages("knitr")
install.packages("Matrix")
install.packages("irlba")
install.packages("e1071")

# ===== LIBRARIES =====
library(e1071)
library(corrplot)
library(dplyr)
library(tidytext)
library(tm)
library(tidyr)
library(ggplot2)
library(dbscan)
library(factoextra)
library(qdapDictionaries)
library(textclean)
library(stringr)
library(tokenizers)
library(hunspell)
library(stopwords)
library(textstem)
library(stringi)
library(parallel)
library(knitr)
library(Matrix)
library(irlba)

cat("All packages loaded successfully!\n")


#-------------------- Load Dataset -----------------------

mydata <- read.csv("F:/semester-12/INTRODUCTION TO DATA SCIENCE/Project/My_Project_Mid/pdataset.csv")

mydata <- mydata[1:1000, ]    


# ------------------- Display the first few rows of the Dataset --------------------
head(mydata, 20)

# ------------------- Show shape (rows × columns) --------------------

dim(mydata)

# ------------------- Display data types of each column --------------------

# str(mydata)
sapply(mydata, class)



# ------------------- Generate basic descriptive statistics --------------------


# summary(mydata)

get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


statistics <- data.frame(
  Mean   = sapply(mydata, mean, na.rm = TRUE),
  Median = sapply(mydata, median, na.rm = TRUE),
  Mode   = sapply(mydata, get_mode),
  SD     = sapply(mydata, sd, na.rm = TRUE),
  Min    = sapply(mydata, min, na.rm = TRUE),
  Max    = sapply(mydata, max, na.rm = TRUE),
  Count  = sapply(mydata, function(x) sum(!is.na(x)))
)

statistics

# ------------------- Identify categorical and numerical features --------------------

numeric_features <- names(mydata)[sapply(mydata, is.numeric)]
numeric_features

categorical_features <- names(mydata)[sapply(mydata, function(x) is.factor(x) || is.character(x))]
categorical_features


list(
  Numerical = numeric_features,
  Categorical = categorical_features
)




# ------------------- Data Exploration & Visualization --------------------
#--------------------------------------------------------------------------

# ------------------- Univariate Analysis --------------------
par(mar = c(4, 4, 2, 1))

for (col in numeric_features) {
  hist(mydata[[col]], 
       main=paste("Histogram of", col),
       xlab=col, 
       col="lightblue",
       border="black")
}


for (col in numeric_features) {
  boxplot(mydata[[col]],
          main=paste("Boxplot of", col),
          col="orange",
          vertical=TRUE)
}


for (col in categorical_features) {
  barplot(table(mydata[[col]]),
          main=paste("Bar Plot of", col),
          col="lightgreen", las=2)
}


# ------------------- Bivariate Analysis --------------------

cor_matrix <- cor(mydata[, numeric_features], use="complete.obs")
corrplot(cor_matrix, method="color", type="lower")


pairs(mydata[, numeric_features], main="Scatter Plot Matrix")



# ------------------- Identify patterns: skewness, and possible outliers --------------------


skew_values <- sapply(mydata[, numeric_features], skewness)
skew_values


outlier_summary <- data.frame()

for (col in numeric_features) {
  Q1 <- quantile(mydata[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(mydata[[col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  
  outliers <- sum(mydata[[col]] < lower_bound | mydata[[col]] > upper_bound)
  
  outlier_summary <- rbind(outlier_summary,
                           data.frame(Feature=col, Outliers=outliers))
}

outlier_summary



