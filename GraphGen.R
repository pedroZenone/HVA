
#https://github.com/dunkchawannut/bitcoin_bstm/blob/master/BitCoin_Bstm_a.R

library("igraph")
library('bsts')
library("imputeTS")
library("zoo")
library('bsts')
library("tseries")
library("ggplot2")
library("lubridate")
library("dplyr")
library("forecast")
library("reshape2")


#######################################################################################

data = read.csv(".\\data\\daily_fb_bq_hva_updated_full.csv")

############ PREPROC ############

start_date = '2018-01-01'
end_date = '2018-09-15' 
data = data[,3:ncol(data)]  # Skip Date and smooth


for (i in 1:ncol(data))
{
  data[,i] = na.interpolation(data[,i],option = 'spline')  # interpolate missing data
}

############ PREPROC ############



############CREATE SERIE ############

ts = zoo(data, seq(from = as.Date(start_date), to = as.Date(end_date), by = 1))

############CREA SERIE ############


############ CREATE RELATIONS BETWEEN VARIABLES (All vs All) ############

ss <- AddLocalLinearTrend(list() , ts$form_fills)
relation <- data.frame()

for(i in 1:ncol(ts)){
  # Run Bayesian Structural Model with 3000 chains
  
  model <- bsts(ts[,i] ~. ,
                state.specification = ss,
                niter = 3000 ,
                data = ts[,-i])
  
  burn_in <- bsts::SuggestBurn(0.3 , model)
  # Get 
  prop <- melt(colMeans(apply(model$coefficients[-(1:burn_in),], 2 , function(x){ ifelse(x > 0 , 1 , 0)})))
  #print(prop)
  #Format for relation table
  kk <- cbind(expand.grid(names(ts)[i] , row.names(prop)[-1]) , "Prob" = prop[-1,])
  relation <- rbind(relation , kk)
  #name <- names(ts)[i]
  #all_m[[name]] <- model
}

############ CREATE RELATIONS BETWEEN VARIABLES (All vs All) ############


############ CREATE GRAPH THRESHOLDING PROBABILITY ############

main <- melt(names(ts) ,value.name = "Variable")
main$Variable <- as.character(main$Variable)

relation_all <- relation
relation_filtada <- filter(relation , Prob >= 0.1)
g <- graph_from_data_frame(relation_filtada , directed = TRUE , vertices = main)

#Para elegir colores
#https://htmlcolorcodes.com/


dev.off()
plot.igraph(
  g ,
  edge.arrow.size= 0.6,
  vertex.size= 20,
  vertex.label.cex = 0.9, 
  label.cex = 0.1,
  layout=layout.fruchterman.reingold,
  vertex.color = "#CDF5F6", 
  edge.color = '#F2B829', 
  vertex.label.color="blue",
  edge.width=E(g)$Prob*4,
  edge.curved= FALSE,
  main = "Network of inclusion probability" 
)

############ CREATE GRAPH THRESHOLDING PROBABILITY ############

