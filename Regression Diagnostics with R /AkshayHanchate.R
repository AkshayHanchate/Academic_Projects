#Akshay Hanchate

library(janitor)
library(dplyr)
library(ggplot2)
library(plotly)
library(zoo)
library(scales)

df_ameshousing = read.csv("AmesHousing-1.csv")

#starting with EDA
#lets get the summary statistics 
summary(df_ameshousing)

#lets get the structure of the data to check the imp variables and datatypes
str(df_ameshousing)

#missing check 
colSums(is.na(df_ameshousing))

#in R sometimes empty string is not considered in missing value, so we convert empty string to NA and check the missing count again        
df_ameshousing[df_ameshousing == ''] = NA
missing_values = colSums(is.na(df_ameshousing))

#calculating summary with percentage for better understanding 
missing_percentage = round((missing_values / nrow(df_ameshousing)) * 100,2)
View(missing_percentage)





#handling missing values 
#Apart from these two function rest all are categorical function, NA means that feature being not applicable
df_ameshousing$Lot.Frontage = na.aggregate(df_ameshousing$Lot.Frontage, FUN = mean)
df_ameshousing$Mas.Vnr.Area = na.aggregate(df_ameshousing$Mas.Vnr.Area, FUN = mean)

#to find out correlation lets create a subset containing only numeric values
df_numeric = df_ameshousing[,sapply(df_ameshousing,is.numeric)]
#to cross-check if we have any numerical missing values 
colSums(is.na(df_numeric))
#lets convert these misssing values to 0 because their independent categorical variable is 0
columns_to_replace = c('BsmtFin.SF.1','BsmtFin.SF.2','Bsmt.Unf.SF','Total.Bsmt.SF','Bsmt.Full.Bath','Bsmt.Half.Bath','Garage.Yr.Blt','Garage.Cars','Garage.Area')

for (col in columns_to_replace) {
  df_numeric[[col]] = replace(df_numeric[[col]], is.na(df_numeric[[col]]), 0)
}

corr_matrix = cor(df_numeric)
View(corr_martix)

# Reshape data for ggplot
install.packages('corrplot')
library(reshape2)
data_long <- melt(cor(df_numeric), na.rm = TRUE)

# Create ggplot
ggplot(data_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1))


# Scatter plot for the variable with the highest correlation
ggplot(df_numeric, aes(x = Gr.Liv.Area , y = SalePrice)) +
  geom_point() +
  labs(title = "Scatter Plot - Highest Correlation", x = "Gr Living Area", y = "SalePrice") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))

# Scatter plot for the variable with the lowest correlation
ggplot(df_numeric, aes(x = Enclosed.Porch, y = SalePrice)) +
  geom_point() +
  labs(title = "Scatter Plot - Lowest Correlation", x = "Enclosed Porch", y = "SalePrice") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))


# Scatter plot for the variable with correlation closest to 0.5
ggplot(df_numeric, aes(x = Mas.Vnr.Area, y = SalePrice)) +
  geom_point() +
  labs(title = "Scatter Plot - Closest to 0.5 Correlation", x = "Mas Vnr Area", y = "SalePrice") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))

cont_variables = c("Gr.Liv.Area","Garage.Area","Total.Bsmt.SF")

# Fit a linear regression model
model <- lm(SalePrice ~ ., data = df_numeric[, c("SalePrice",cont_variables)])

summary(model)


install.packages("car")
library(car)

vif_values <- car::vif(model)
print(vif_values)

outlierTest(model = model)
#Noww we know the outliers as 1499,2181 

#lets check if the models show any linearity or not 

plot(df_numeric$Gr.Liv.Area, df_numeric$SalePrice, 
     xlab = "Gr.Liv.Area", ylab = "SalePrice",
     main = "Scatter Plot - Gr.Liv.Area vs SalePrice")

# Scatter plot for Garage.Area
plot(df_numeric$Garage.Area, df_numeric$SalePrice, 
     xlab = "Garage.Area", ylab = "SalePrice",
     main = "Scatter Plot - Garage.Area vs SalePrice")

# Scatter plot for Total.Bsmt.SF
plot(df_numeric$Total.Bsmt.SF, df_numeric$SalePrice, 
     xlab = "Total.Bsmt.SF", ylab = "SalePrice",
     main = "Scatter Plot - Total.Bsmt.SF vs SalePrice")


#lets remve the outliers and check once again 
df_numeric <- subset(df_numeric, !(Order %in% c(1499, 2181)))



cont_variables2 = c("Gr.Liv.Area","Garage.Area","Total.Bsmt.SF","X1st.Flr.SF","Full.Bath")

#lets cehck the model fit again 
model3 <- lm(SalePrice ~ ., data = df_numeric[, c("SalePrice",cont_variables2)])

summary(model2)

#lets check the plots again by adding more customer
# Install and load the leaps package
library(MASS)

stepAIC(model2, direction = "backward")

stepAIC(model2, direction = "forward")

stepAIC(model2, direction = "both")


install.packages("leaps")
library(leaps)

# Specify the formula for the regression model
formula <- SalePrice ~ Gr.Liv.Area + Garage.Area + Total.Bsmt.SF + X1st.Flr.SF + Full.Bath

# Create a data frame with the variables of interest
subset_data <- df_numeric[, c("SalePrice", "Gr.Liv.Area", "Garage.Area", "Total.Bsmt.SF", "X1st.Flr.SF", "Full.Bath")]

# Use regsubsets to perform all subsets regression
subsets_model <- regsubsets(formula, data = subset_data, nvmax = 6)  # Adjust nvmax based on the number of predictors

plot(subsets_model, scale = "adjr2")

# Print summary of results
summary(subsets_model)



