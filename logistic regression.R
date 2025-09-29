# Load the data
nfl <- read.csv("NFL.csv")

# Drop unnecessary columns
nfl <- subset(nfl, select = -c(Player, School, Drafted..tm.rnd.yr., Player_Type, Position))

# checking NA observations
colSums(is.na(nfl))

# Replace NA with column medians for numeric predictors
for (col in names(nfl)) {
  if (is.numeric(nfl[[col]])) {
    nfl[[col]][is.na(nfl[[col]])] <- median(nfl[[col]], na.rm = TRUE)
  }
}
# confirm with no NA
colSums(is.na(nfl))

# Convert categorical variables to factors
cat_vars <- c("Position_Type")
nfl[cat_vars] <- lapply(nfl[cat_vars], as.factor)

# Convert Drafted to binary (1 = Yes, 0 = No)
nfl$Drafted <- ifelse(nfl$Drafted == "Yes", 1, 0)

# Fit logistic regression
model <- glm(Drafted ~ ., data = nfl, family = binomial)

# Summary of the model
summary(model)

nfl$predicted_prob <- predict(model, type = "response")
predicted_class <- ifelse(nfl$predicted_prob >= 0.5, 1, 0)
conf_mat <- table(Predicted = predicted_class, Actual = nfl$Drafted)
conf_mat

# Accuracy
accuracy <- (conf_mat[1,1] + conf_mat[2,2]) / sum(conf_mat)
accuracy

## ROC Curve
library(pROC)
nfl$predicted_prob <- predict(model, type = "response")
roc_obj <- roc(nfl$Drafted, nfl$predicted_prob)
plot(roc_obj, main = "ROC Curve", col = "blue")
auc(roc_obj)




