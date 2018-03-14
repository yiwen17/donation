# This file is for data visualization on maps
require(googleVis)

# the following codes were written to construct suitable object s.t. googleVis could make maps based on it
count = summary(quant_learn$STATCODE)
statMean = aggregate(quant_learn[c('TARGDOL', 'avg', 'CONLARG', 'CNMON1')], list(quant_learn$STATCODE), mean)
statMean$state = paste0('US-', statMean$Group.1)

# making maps
GeoStates <- gvisGeoChart(statMean[c(2:12, 14:54), ], "state", "TARGDOL", options=list(region="US", displayMode="regions", resolution="provinces", width=600, height=400))                #'TARGDOL' is the variable of interest for this map
plot(GeoStates)

GeoStates2 <- gvisGeoChart(statMean[c(2:12, 14:54), ], "state", "avg", options=list(region="US", displayMode="regions", resolution="provinces", width=600, height=400))                #'avg' is the variable of interest here
plot(GeoStates2)
