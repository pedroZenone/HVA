library('dplyr')
setwd("C:\\Users\\pedzenon\\Desktop\\Proyectos\\USA\\off-site\\Git")

###############################################################################
#########################        Preproc      #################################
###############################################################################

data = read.csv(".\\data\\daily_fb_bq_hva_updated_full.csv")

# Facebook
start_date = '2018-03-20'
end_date = '2018-07-29' 

data$date <- as.Date(data$date, format="%Y-%m-%d")
data = data[data$date >= as.Date(start_date), ]
data = data[data$date <= as.Date(end_date), ]

# filter
data = dplyr::select(data,likes,comments,shares,video_completes,form_fills)

# Impute missing data with cubic plynom
library("imputeTS")
for (i in 1:ncol(data))
{
  data[,i] = na.interpolation(data[,i],option = 'spline')  # interpolate missing data
}

#write.csv(x = data,file = "ffteame.csv") # para analizar en python

# To time series
library("zoo")
ts = zoo(data, seq(from = as.Date(start_date), to = as.Date(end_date), by = 1))

plot.ts(ts$form_fills)

# Analyze in frequency domain. See meaningful harmonics
library("GeneCycle")
f.data <- GeneCycle::periodogram(ts$form_fills - mean(ts$form_fills))
plot(f.data$freq, f.data$spec,type = "l")
# Puede verse que hay un pico claro en 0.01Hz -> 100 dias
acf(ts$form_fills)  # Also see seasonality here

###############################################################################
#########################        Model data      ##############################
###############################################################################


# Train several models and see wich has the lowest Accum error
library('bsts')

ss <- AddLocalLevel(list(), ts$form_fills)
model1 <- bsts(form_fills ~ .,
               state.specification = ss,
               niter = 1000,
               data = ts,
               ping = 0)

ss <- AddLocalLinearTrend(list(), ts$form_fills)
model2 <- bsts(form_fills ~ .,
               state.specification = ss,
               niter = 1000,
               data = ts,
               ping = 0)

ss <- AddSeasonal(list(), ts$form_fills, nseasons = 100)
model3 <- bsts(form_fills ~ .,
               state.specification = ss,
               niter = 1000,
               data = ts,
               ping = 0)


ss <- AddLocalLinearTrend(list(), ts$form_fills)
ss <- AddSeasonal(ss, ts$form_fills, nseasons = 100)
model4 <- bsts(form_fills ~ .,
               state.specification = ss,
               niter = 1000,
               data = ts,
               ping = 0)

ss <- AddLocalLinearTrend(list(), ts$form_fills)
ss <- AddSeasonal(ss, ts$form_fills, nseasons = 100)
model5 <- bsts(form_fills ~ .,
               state.specification = ss,
               niter = 1000,
               data = ts,
               expected.model.size = 1,
               ping = 0)

ss <- AddLocalLinearTrend(list(), ts$form_fills)
ss <- AddSeasonal(ss, ts$form_fills, nseasons = 100)
model6 <- bsts(form_fills ~ .,
               state.specification = ss,
               niter = 1000,
               data = ts,
               expected.model.size = 2,
               ping = 0)

ss <- AddLocalLinearTrend(list(), ts$form_fills)
ss <- AddSeasonal(ss, ts$form_fills, nseasons = 100)
model7 <- bsts(form_fills ~ .,
               state.specification = ss,
               niter = 1000,
               data = ts,
               expected.model.size = 3,
               ping = 0)

ss <- AddLocalLevel(list(), ts$form_fills)
model8 <- bsts(form_fills ~ .,
               state.specification = ss,
               niter = 1000,
               data = ts,
               expected.model.size = 3,
               ping = 0)

ss <- AddLocalLinearTrend(list(), ts$form_fills)
model9 <- bsts(form_fills ~ .,
               state.specification = ss,
               niter = 1000,
               data = ts,
               expected.model.size = 2,
               ping = 0)

###############################################################################
#######################        Test & Interpretation      #####################
###############################################################################


# Compare models: Model 1 is the winner :)
CompareBstsModels(model.list = list(local = model1,linear =model2,season = model3,local_3 = model8,local_2 = model9))

# Once choosen the best model, lets check coefficients:
plot(model1,"coef")

# Function to see coefficients!
GetInclusionProbabilities <- function(bsts.object) {
  # Pulls code from
  # - BoomSpikeSlab::PlotMarginalInclusionProbabilities
  # - bsts::PlotBstsCoefficients
  burn <- SuggestBurn(0.1, bsts.object)
  beta <- bsts.object$coefficients
  beta <- beta[-(1:burn), , drop = FALSE]
  inclusion.prob <- colMeans(beta != 0)
  index <- order(inclusion.prob)
  inclusion.prob <- inclusion.prob[index]
  # End from BoomSpikeSlab/bsts.
  return(data.frame(predictor = names(inclusion.prob),
                    inclusion.prob = inclusion.prob))
}

GetInclusionProbabilities(model1) 

# are them representative?
plot(model1,"comp")

# Test it with none seen values:
nrow = nrow(ts)
test= ts[(nrow - round(nrow*0.15)):nrow,]
train = ts[1:(nrow - round(nrow*0.15) - 1),]

ss <- AddLocalLevel(list(), train$form_fills)
model1 <- bsts(form_fills ~ .,
               state.specification = ss,
               niter = 1000,
               data = train,
               ping = 0)

pred = predict(model1,newdata = test)
plot(pred,plot.original = 30)

### Now Interpret this coef:

library('lubridate')
library('ggplot2')
library('reshape2')

# Analyze if theres something meaninfull in linear correlation between variables:
cor(data)  # Previous results are not equal to exposed previously...
plot(data)

# Get the number of burn-ins to discard
burn <- SuggestBurn(0.1, model1)

# Helper function to get the positive mean of a vector
PositiveMean <- function(b) {
  b <- b[abs(b) > 0]
  if (length(b) > 0) 
    return(mean(b))
  return(0)
}

# Plot the coeficients values:
coeff <- data.frame(melt(apply(model1$coefficients[-(1:burn),], 2, PositiveMean)))
coeff$Variable <- as.character(row.names(coeff))
ggplot(data=coeff, aes(x=Variable, y=value)) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  xlab("") + ylab("") + ggtitle("Average coefficients")

