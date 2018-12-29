library('dplyr')
setwd("C:\\Users\\pedzenon\\Desktop\\Proyectos\\USA\\off-site\\Git")

###############################################################################
#########################        Preproc      #################################
###############################################################################

data = read.csv(".\\data\\daily_fb_bq_hva_updated_full.csv")

# Facebook
start_date = '2018-01-01'
end_date = '2018-09-15' 

data$date <- as.Date(data$date, format="%Y-%m-%d")
data = data[data$date >= as.Date(start_date), ]
data = data[data$date <= as.Date(end_date), ]

# filter
data = dplyr::select(data,likes,comments,shares,video_completes,landing_page_view)

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

plot.ts(ts$landing_page_view)

# Analyze in frequency domain. See meaningful harmonics
library("GeneCycle")
f.data <- GeneCycle::periodogram(ts$landing_page_view - mean(ts$landing_page_view))
plot(f.data$freq, f.data$spec,type = "l")
# Puede verse que hay un pico claro en 0.01Hz -> 100 dias
acf(ts$landing_page_view)  # Also see seasonality here

###############################################################################
#########################        Model data      ##############################
###############################################################################


# Train several models and see wich has the lowest Accum error
library('bsts')

ss <- AddLocalLevel(list(), ts$landing_page_view)
model1 <- bsts(landing_page_view ~ .,
               state.specification = ss,
               niter = 1000,
               data = ts,
               ping = 0)

ss <- AddLocalLinearTrend(list(), ts$landing_page_view)
model2 <- bsts(landing_page_view ~ .,
               state.specification = ss,
               niter = 1000,
               data = ts,
               ping = 0)

ss <- AddSeasonal(list(), ts$landing_page_view, nseasons = 44)
model3 <- bsts(landing_page_view ~ .,
               state.specification = ss,
               niter = 1000,
               data = ts,
               ping = 0)


ss <- AddLocalLinearTrend(list(), ts$landing_page_view)
ss <- AddSeasonal(ss, ts$landing_page_view, nseasons = 44)
model4 <- bsts(landing_page_view ~ .,
               state.specification = ss,
               niter = 1000,
               data = ts,
               ping = 0)

ss <- AddLocalLinearTrend(list(), ts$landing_page_view)
ss <- AddSeasonal(ss, ts$landing_page_view, nseasons = 44)
model5 <- bsts(landing_page_view ~ .,
               state.specification = ss,
               niter = 1000,
               data = ts,
               expected.model.size = 1,
               ping = 0)

ss <- AddLocalLinearTrend(list(), ts$landing_page_view)
ss <- AddSeasonal(ss, ts$landing_page_view, nseasons = 44)
model6 <- bsts(landing_page_view ~ .,
               state.specification = ss,
               niter = 1000,
               data = ts,
               expected.model.size = 2,
               ping = 0)

ss <- AddLocalLinearTrend(list(), ts$landing_page_view)
ss <- AddSeasonal(ss, ts$landing_page_view, nseasons = 44)
model7 <- bsts(landing_page_view ~ .,
               state.specification = ss,
               niter = 1000,
               data = ts,
               expected.model.size = 3,
               ping = 0)

ss <- AddLocalLevel(list(), ts$landing_page_view)
model8 <- bsts(landing_page_view ~ .,
               state.specification = ss,
               niter = 1000,
               data = ts,
               expected.model.size = 3,
               ping = 0)

ss <- AddLocalLinearTrend(list(), ts$landing_page_view)
model9 <- bsts(landing_page_view ~ .,
               state.specification = ss,
               niter = 1000,
               data = ts,
               expected.model.size = 2,
               ping = 0)


ss <- AddLocalLevel(list(), ts$landing_page_view)
ss <- AddSeasonal(ss, ts$landing_page_view, nseasons = 44)
model10 <- bsts(landing_page_view ~ .,
               state.specification = ss,
               niter = 1000,
               data = ts,
               ping = 0)

ss <- AddLocalLevel(list(), ts$landing_page_view)
ss <- AddSeasonal(ss, ts$landing_page_view, nseasons = 44)
model11 <- bsts(landing_page_view ~ .,
                state.specification = ss,
                niter = 1000,
                data = ts,
                expected.model.size = 1,
                ping = 0)
ss <- AddLocalLevel(list(), ts$landing_page_view)
ss <- AddSeasonal(ss, ts$landing_page_view, nseasons = 44)
model12 <- bsts(landing_page_view ~ .,
                state.specification = ss,
                niter = 1000,
                data = ts,
                expected.model.size = 2,
                ping = 0)

ss <- AddLocalLevel(list(), ts$landing_page_view)
ss <- AddSeasonal(ss, ts$landing_page_view, nseasons = 44)
model13 <- bsts(landing_page_view ~ .,
                state.specification = ss,
                niter = 1000,
                data = ts,
                expected.model.size = 3,
                ping = 0)

#ss <- AddSeasonal(list(), ts$landing_page_view, nseasons = 7)
#model_1 <- bsts(landing_page_view ~ .,
#              state.specification = ss,
#               niter = 1000,
#              data = ts,
#              ping = 0)
#ss <- AddSeasonal(list(), ts$landing_page_view, nseasons = 14)
#model_2 <- bsts(landing_page_view ~ .,
#                state.specification = ss,
#                niter = 1000,
#                data = ts,
#                ping = 0)
#ss <- AddSeasonal(list(), ts$landing_page_view, nseasons = 21)
#model_3 <- bsts(landing_page_view ~ .,
#                state.specification = ss,
#                niter = 1000,
#                data = ts,
#                ping = 0)
#ss <- AddSeasonal(list(), ts$landing_page_view, nseasons = 30)
#model_4 <- bsts(landing_page_view ~ .,
#                state.specification = ss,
#                niter = 1000,
#                data = ts,
#                ping = 0)
#ss <- AddSeasonal(list(), ts$landing_page_view, nseasons = 40)
#model_5 <- bsts(landing_page_view ~ .,
#                state.specification = ss,
#                niter = 1000,
#                data = ts,
#                ping = 0)
#ss <- AddSeasonal(list(), ts$landing_page_view, nseasons = 52)
#model_6 <- bsts(landing_page_view ~ .,
#                state.specification = ss,
#                niter = 1000,
#                data = ts,
#                ping = 0)
#
#CompareBstsModels(model.list = list(m1 = model_1,m2 = model_2,m3 = model_3,m4 = model_4,m5 = model_5,m6 = model_6))

###############################################################################
#######################        Test & Interpretation      #####################
###############################################################################


# Compare models: Model 2 is the winner :)
CompareBstsModels(model.list = list(local = model1,linear =model2,season = model3,LTs = model4,LLs = model10))
CompareBstsModels(model.list = list(model5,model6,model7,model8,model9))
CompareBstsModels(model.list = list(model10,model11,model12,model13,model8))
CompareBstsModels(model.list = list(linear =model2,LocalLevel_3 = model8,LocalLevelSeason = model10))

# Once choosen the best model, lets check coefficients:
plot(model2,"coef")

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

GetInclusionProbabilities(model2) 

# are them representative?
plot(model2,"comp")

# Test it with none seen values:
ts_2 = ts
nrow = nrow(ts_2)
test_2= ts_2[(nrow - round(nrow*0.33)):nrow,]
train_2 = ts_2[1:(nrow - round(nrow*0.33) - 1),]

pre.period <- as.Date(c("2018-01-01","2018-06-21"))
post.period <- as.Date(c("2018-06-22","2018-09-15"))

post.period.response = fortify.zoo(test_2$landing_page_view)[,2]
ts_2[(nrow - round(nrow*0.33)):nrow,"landing_page_view"] <- NA

ss <- AddLocalLinearTrend(list(), ts_2$landing_page_view)
model4 <- bsts(landing_page_view ~ .,
               state.specification = ss,
               niter = 1000,
               data = ts_2,
               ping = 0)

impact <- CausalImpact(bsts.model = model4,
                       post.period.response = post.period.response)
plot(impact)

### Now Interpret this coef:

library('lubridate')
library('ggplot2')
library('reshape2')

# Analyze if theres something meaninfull in linear correlation between variables:
cor(data)  # Previous results are not equal to exposed previously...
plot(data)

# Get the number of burn-ins to discard
burn <- SuggestBurn(0.1, model2)

# Helper function to get the positive mean of a vector
PositiveMean <- function(b) {
  b <- b[abs(b) > 0]
  if (length(b) > 0) 
    return(mean(b))
  return(0)
}

# Plot the coeficients values:
coeff <- data.frame(melt(apply(model2$coefficients[-(1:burn),], 2, PositiveMean)))
coeff$Variable <- as.character(row.names(coeff))
ggplot(data=coeff, aes(x=Variable, y=value)) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  xlab("") + ylab("") + ggtitle("Average coefficients")

