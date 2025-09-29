# Load Data
nfl <- read.csv("NFL.csv", header = FALSE)
colnames(nfl) = nfl[1,]
nfl = nfl[-1,]
nfl = nfl[,-13]

# Initial filtering
nfl_LR = nfl |>
  drop_na() |>
  dplyr::select(Height, Vertical_Jump, Broad_Jump, Player_Type, Year) |>
  filter(Year == "2017")

# Convert columns to numeric
nfl_LR$Height = as.numeric(nfl_LR$Height)
nfl_LR$Vertical_Jump = as.numeric(nfl_LR$Vertical_Jump)
nfl_LR$Broad_Jump = as.numeric(nfl_LR$Broad_Jump)

# Clean dataset with numeric conversions
nfl_LR <- nfl %>%
  dplyr::select(-Drafted..tm.rnd.yr.) %>%
  mutate(across(c(Broad_Jump, Vertical_Jump, Height), na_if, "")) %>%
  drop_na() %>% 
  mutate(
    Broad_Jump = as.numeric(Broad_Jump),
    Vertical_Jump = as.numeric(Vertical_Jump),
    Height = as.numeric(Height),
    Weight = as.numeric(Weight),
    Sprint_40yd = as.numeric(Sprint_40yd)
  )

# Subset for 2017
nfl_2017 = nfl_LR %>%
  dplyr::select(Height, Vertical_Jump, Broad_Jump, Player_Type, Year) %>%
  filter(Year == "2017")

# Plots
plot1 <- ggplot(nfl_2017, aes(x = Vertical_Jump, y = Broad_Jump)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~Player_Type) +
  labs(
    x = "Vertical Jump (CM)",
    y = "Broad Jump (CM)",
    title = "Broad Jump vs Vertical Jump by Player Type"
  ) +
  theme_minimal()

plot2 <- ggplot(nfl_2017, aes(x = Height, y = Broad_Jump)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~Player_Type) + 
  labs(
    x = "Height (M)",
    y = "Broad Jump (CM)",
    title = "Broad Jump vs Height by Player Type"
  ) +
  theme_minimal()

print(plot1)
print(plot2)
nrow(nfl_2017)

# Residuals vs. Fitted Values Plot
full.model = lm(Broad_Jump ~ Height + Player_Type + Vertical_Jump, data = nfl_LR)
plot(full.model$fitted.values, full.model$residuals,
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

# Removing Outliers
n = length(nfl_LR$Height)
p = length(full.model$coefficients)
t.cutoff = qt(1 - 0.05/2, n - p)
ei.s = full.model$residuals /
  sqrt(sum(full.model$residuals^2) /
         (length(full.model$residuals) - length(full.model$coefficients)))
outliers.idx = which(abs(ei.s) > t.cutoff)
outliers = nfl_LR[outliers.idx, ]
nfl_LR_cleaned = nfl_LR[-outliers.idx,]

# Normality
full.model = lm(Broad_Jump ~ Height + Player_Type + Vertical_Jump, data = nfl_LR_cleaned)
shapiro.test(full.model$residuals)

# Constant Variance
Group = rep("Lower", nrow(nfl_LR_cleaned))
Group[nfl_LR_cleaned$Broad_Jump > median(nfl_LR_cleaned$Broad_Jump)] = "Upper"
nfl_LR_cleaned$Group = as.factor(Group)
leveneTest(full.model$residuals ~ Group, data = nfl_LR_cleaned, center = median)

# Stepwise model selection
empty.model = lm(Broad_Jump ~ 1, data = nfl_LR_cleaned)
model = stepAIC(empty.model, scope = list(lower = empty.model, upper = full.model),
                k = 2, direction = "both", trace = TRUE)
model$coefficients
summary(full.model)

final.model = lm(Broad_Jump ~ Vertical_Jump, data = nfl_LR_cleaned)
summary(final.model)

# Multiplier functions
mult.fun = function(n, p, g, alpha){
  bon = qt(1 - alpha/(2*g), n - p)
  WH = sqrt(p * qf(1 - alpha, p, n - p))
  Sch = sqrt(g * qf(1 - alpha, g, n - p))
  all.mul = c(bon, WH, Sch)
  all.mul = round(all.mul, 3)
  names(all.mul) = c("Bon", "WH", "Sch")
  return(all.mul)
}

mult.CI = function(C.star, x.stars, the.model, alpha, the.type = "confidence"){
  all.preds = predict(the.model, x.stars)
  if(the.type == "confidence"){
    all.se = predict(the.model, x.stars, interval = the.type, se.fit = TRUE)$se.fit
  } else if(the.type == "prediction"){
    all.se = predict(the.model, x.stars, interval = the.type, se.fit = TRUE)$se.fit
    MSE = sum(the.model$residuals^2) /
      (length(the.model$residuals) - length(the.model$coefficients))
    all.se = sqrt(all.se^2 + MSE)
  }
  LB = all.preds - C.star * all.se
  UB = all.preds + C.star * all.se
  all.CIs = cbind(LB, UB)
  colnames(all.CIs) = paste((1 - alpha) * 100, "%", c(" Lower", " Upper"), sep = "")
  results = cbind(all.preds, all.CIs)
  colnames(results)[1] = "Estimate"
  return(results)
}

# Apply multipliers
all.multipliers = mult.fun(nrow(nfl_LR_cleaned), length(final.model$coefficients), 3, 0.05)
print(all.multipliers)

set.seed(123)
x = sample(1:102, 3)
points = nfl_LR_cleaned[x,]
mult.CI(all.multipliers[1], points, final.model, 0.05, "prediction")

# Predictions
predicted.y = final.model$coefficients[1] + final.model$coefficients[2] * points[,2]
y = points[,3]
ei = y - predicted.y

knitr::kable(cbind(predicted.y, y, ei),
             col.names = c("Predicted Y", "Actual Y", "Error"),
             caption = "Broad Jump Predictions with Final Model")
