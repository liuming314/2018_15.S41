#### FIRST SET OF EXERCISES: Data Wrangling
# 1. Working with dates
# The `mutate_at` command is very similar to `mutate_if` and
# `mutate_all` commands, but you can specify the variables that
# you want to change directly, for example, we can say:
# listingsOrig %>%
#   mutate_at(vars(city, market), as.factor)
# Using the `mutate_at` and `as.Date()` commands,
# convert the variables first_review and last_review to
# dates, and fill in the missing values with the median dates. ->

# this implementation is borked
# listings <- listings %>% mutate_at(vars(first_review, last_review), as.Date()) %>%
# mutate_all(function(x) coalesce(x, median(x, na.rm = TRUE)))

# more correctly
head(listingsOrig$first_review)
head(listingsOrig$last_review)
?as.date
listingsNew <- listingsOrig %>% 
  mutate_at(vars(first_review, last_review),
            function(x) as.Date(x, "%Y-%m-%d")) %>%
  mutate_at(vars(first_review, last_review),
            function(x) coalesce(x, median(x, na.rm = TRUE)))
summary(listingsNew$first_review)
summary(listingsNew$last_review)
?mutate_at

# how to function 101
f <- function(x) { coalesce(x, median(x, na.rm = TRUE))}
# how to function 102
f(listingsOrig$first_review)

# 2. Converting to NA's
# Sometimes the data set can be tricky, and we need to specify
# which values are NA's for ourselves.  For instance, look at
# the `host_response_rate` column.  Convert the appropriate 
# values to NA's using the `na_if()` command and impute the
# missing values with the median host_response_rate. ->

#### SECOND SET OF EXERCISES: Linear Regression
# 1. Building a simple model
# Regress price on review_scores_rating. Plot the regression
# line and the actual training points, and find the in-sample
# R^2. (Read below for more details if you need them.)

# DETAILS:
# -Use `lm()` to learn the linear relationship
# -In-sample R^2 is one of the outputs of `summary()`
# -Use `add_predictions()` and ggplot tools for the plotting ->

# 2. Adding more varibles
# Try to beat the out-of-sample performance of the
# price ~ accommodates model by adding other variables. You can use
# `names(listings)` to explore potential predictors.
# If you start getting  errors or unexpected behavior, make sure
# the predictors are in the format you think they are.
# You can check this using the `summary()` and `str()` functions
# on listings$<variable of interest>. ->

# 3. Median Regression
# # Since we're dealing with data on price,
# we expect that there will be high outliers. While least-squares
# regression is reliable in many settings, it has the property 
# that the estimates it generates depend quite a bit on the outliers.
# One alternative, median regression, minimizes *absolute* error
# rather than squared error. This has the effect of regressing
# on the median rather than the mean, and is more robust to outliers.
# In R, it can be implemented using the `quantreg` package.

# For this exercise, install the quantreg package, and compare
# the behavior of the median regression fit (using the `rq()`)
# function) to the least squares fit from `lm()` on the original
# listings data set given below which includes all the price outliers.
data <- listingsOrig %>%
mutate(price = as.numeric(gsub("\\$|,", "", price)))
# Hint: Enter ?rq for info on the rq function.

# DETAILS:
# -Split into training/testing set
# -Fit the median and linear regression models
# -Plot the two lines together using `gather_predictions`,
# which is very similar to the `add_predictions` function
# that we saw in class. 
# -Add "color = model" as a geom_line aesthetic
# to differentiate the two models in the plot. ->

#### THIRD SET OF EXERCISES: glmnet
# 1. The glmnet package is actually more versatile than just
# LASSO regression. It also does ridge regression (with the l2 norm),
# and any mixture of LASSO and ridge. The mixture is controlled
# by the parameter alpha: alpha=1 is the default and corresponds
# to LASSO, alpha=0 is ridge, and values in between are mixtures
# between the two (check out the formula using ?glmnet).
# One could use cross validation to choose this
# parameter as well. For now, try just a few different values of
# alpha on the model we built for LASSO using `cv.glmnet()`
# (which does not cross-validate for alpha automatically).
# How do the new models do on out-of-sample R^2? ->

#### Fourth Set of Exercises: Classification
# 1. Add more variables to Logistic Regression
# Try to beat the out-of-sample performance for logistic
# regression of elevators on price by adding new variables.
# Compute the out-of-sample AUC of the final model,
# and plot the ROC curve.  ->
logReg2 <- glm(amenity_Elevator_in_Building ~
                 price + neighbourhood_cleansed,
               family = "binomial", data = listingsGLMTrain)
summary(logReg2)

pred_test <- predict(logReg2, newdata = listingsGLMTest, type = "response")
pred_obj <- prediction(pred_test, listingsGLMTest$amenity_Elevator_in_Building)
perf <- performance(pred_obj, 'tpr', 'fpr')
performance(pred_obj, 'auc')@y.values
plot(perf, colorize = TRUE)

# 2. Tuning a CART model
# Let's try building a more complicated CART model and
# tuning the parameters. Using the below formula, build a
# CART model to predict `neighbourhood_cleansed` based on price
# and all of the amenities, tuning the cp parameter.
treeBig2 <- rpart(tree_formula,
                  data = listingsGLMTrain,
                  cp = 0.001)
plotcp(treeBig2)
treeFinal2 <- prune(treeBig2, 0.0025)
prp(treeFinal2, varlen =  0)
# Plot the final tree with the option "varlen = 0",
# and save the result as a pdf.  Upload your result to stellar. ->

