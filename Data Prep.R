donate = read.csv('donation data.csv')  #master sheet of data
code = read.csv('dmef1code.csv')        #codebook for understanding solicitation & contribution codes
divide = read.csv('geo.csv')            #geographical region division
inc = read.csv('h08.csv')               #income level data for all states; larger is higher

##------------ DATA PREPARATION -------------------
require(plyr)
names(inc) = c('STATCODE', 'incZone')
donate = join(donate, inc, by='STATCODE')      #'divide' and 'code' can be joined similarly
solTime = rowSums(!is.na(donate[, c('sol1', 'sol2', 'sol3')]))
donate$solTime = as.factor(solTime)
donate[which(donate$SEX=='C'),]$SEX = 'U'      #category 'C' is unknown, hence combine w/ 'U'
donate$SEX = relevel(donate$SEX, ref = 'F')

###-----Training and Validation Split-----###
ind = 1:dim(donate)[1]
test_ind = ind %% 3 == 0                          #this is a vector of TRUE's and FALSE's
dona_test = donate[test_ind, ]                    #master test set
quant_test = dona_test[dona_test$TARGDOL>0, ]     #test set for multiple regression models
dona_test[which(is.na(dona_test$incZone)), ]$incZone = 6

train = donate[!test_ind, ]                                  #master training set
quant_learn = train[train$TARGDOL>0, ]                       #training set for multiple regression only
quant_learn$avg = quant_learn$CNTRLIF/quant_learn$CNTMLIF    #create new variable 'avg', for lifetime average contribution
quant_learn[which(is.na(quant_learn$incZone)), ]$incZone = 6
