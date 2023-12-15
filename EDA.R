# Load necessary libraries
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("readxl")
install.packages("writexl")
install.packages("vcd")
install.packages("reshape2")
install.packages("plotly")
install.packages("lattice")
install.packages("gplots")
install.packages("summarytools")
install.packages("e1071")

library(e1071)
library(summarytools)
library(gplots)
library(plotly)
library(lattice)
library(vcd)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(writexl)
library(reshape2)


# Load the data
path <- file.choose()
View(path)
data <- read_excel(path,col_names=TRUE)
View(data)

# 1. Explore the Data
head(data)
str(data)
summary(data)

#2.Data Cleaning (Checking for missing values, outliers)
#Check for missing values in all variables of the data set
missing_values <- sapply(data, function(x) sum(is.na(x)))

#Print the number of missing values for each variable
print(missing_values)

clean_data <- na.omit(data)
View(clean_data)

####################################################################################################
data$ANC_Trim <- ifelse(is.na(data$ANC_Trim), "No", data$ANC_Trim)
data$ANC_4 <- ifelse(is.na(data$ANC_4), "No", data$ANC_4)
data$Place_De <- ifelse(is.na(data$Place_De), "PUBLIC S", data$Place_De)
data$C_Sectio <- ifelse(is.na(data$C_Sectio), "No", data$C_Sectio)
data$Res_Heal <- ifelse(is.na(data$Res_Heal), "No", data$Res_Heal)
data$Postnata <- ifelse(is.na(data$Postnata), "No", data$Postnata)
data$Child_Ch <- ifelse(is.na(data$Child_Ch), "No", data$Child_Ch)
data$LBW1 <- ifelse(is.na(data$LBW1), "1", data$LBW1)
data$Comp_Ble <- ifelse(is.na(data$Comp_Ble), "No", data$Comp_Ble)
data$Comp_Con <- ifelse(is.na(data$Comp_Con), "No", data$Comp_Con)
data$Comp_Pro <- ifelse(is.na(data$Comp_Pro), "No", data$Comp_Pro)
data$Comp_Sev <- ifelse(is.na(data$Comp_Sev), "No", data$Comp_Sev)
data$Comp_HBP <- ifelse(is.na(data$Comp_HBP), "No", data$Comp_HBP)
data$Res_chec <- ifelse(is.na(data$Res_chec), "No", data$Res_chec)
data$comp_pre <- ifelse(is.na(data$comp_pre), "0", data$comp_pre)
View(data)

####################################################################################################

missing_values1 <- sapply(clean_data, function(x) sum(is.na(x)))
print(missing_values1)

data <- clean_data
View(data)
#VISUALIZATION
#Create a box plot for each numeric variable(AGE) in the data set
for (col in names(data)) {
  if (is.numeric(data[[col]])) {
    boxplot(data[[col]], main = col, ylab = col)
  }
}


#Variable - Residence
plot_ly(x = ~data$Residenc, type = "histogram", name = "Bar Chart")

#Variable - Education
barplot(table(data$Educatio), main = "Bar Plot", xlab = "Categories", ylab = "Frequency")

#Variable - Wealth 
pie(table(data$Wealth_I), main = "Pie Chart")

#Variable - Place of delivery 
pie(table(data$Place_De), main = "Pie Chart")

#Variable - Postnatal
ggplot(data, aes(x = Postnata)) +
  geom_bar() +
  labs(title = "Count Plot", x = "Categories", y = "Count")

#Variables - C-Section and Res_heal
mosaicplot(table(data$C_Sectio, data$Res_Heal), main = "Mosaic Plot")

#Variables - ANC_trim and ANC_4
tab <- table(data$ANC_Trim, data$ANC_4)
barplot(tab, col = c("blue", "red"), legend = rownames(tab), beside = TRUE)

#Variables - Media exposure and Postnatal
cont_table <- table(data$Media_Ex, data$Postnata)
heatmap.2(cont_table, dendrogram = "none", col = viridis::viridis(20))

#Variables - Education, House_hold and Under_5
mosaic(~Educatio + House_Ho + Under_5_, data = data)

#Variables - C_Section and Age_1
ggplot(data, aes(x = C_Sectio, y = Age_1)) +
  geom_boxplot()

# Calculate and plot correlation matrix for Age_1, LBW1 and Comp_pre
# Check column data types
str(data)

# Convert specific columns to numeric if needed
data$Age_1 <- as.numeric(data$Age_1)
data$LBW1 <- as.numeric(data$LBW1)
data$comp_pre <- as.numeric(data$comp_pre)

# Check if conversion was successful
str(data)

# Calculate correlation matrix
correlation_matrix <- cor(data[, c("Age_1", "LBW1", "comp_pre")], use = "complete.obs")

# Melt correlation matrix for visualization
melted_correlation <- melt(correlation_matrix)

ggplot(data = melt(correlation_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Correlation Matrix")

#SUMMARY
# Generate a detailed summary report
summary(data)
descr(data)

# Frequency tables for categorical variables
sapply(data, function(x) table(x)[1:min(length(table(x)), 5)] )

#DATA PREPROCESSING
# Example using ggplot2 for box plot
ggplot(data, aes(x = LBW1, y = Age_1)) +
  geom_boxplot()

# Trim values beyond certain quantiles
quantiles <- quantile(data$Age_1, probs = c(0.01, 0.99), na.rm = TRUE)
data$Age_1[data$Age_1 < quantiles[1]] <- quantiles[1]
data$Age_1[data$Age_1 > quantiles[2]] <- quantiles[2]

# Scale numeric variables
data$Age_1 <- scale(data$Age_1)

#PERFORMANCE SPECIFICATION
lm(LBW1 ~ Age_1, data = data)

data$Postnata <- ifelse(data$Postnata == "Yes", 0, 1)
glm(Postnata ~ Media_Ex, family = binomial(link = "logit"), data = data)

#SVM - 1
# Train a linear SVM
svm_model_linear <- svm(Age_1 ~ ., data = data, kernel = "linear")

# View model summary
summary(svm_model_linear)


