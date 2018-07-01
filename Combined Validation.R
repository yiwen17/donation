test_once = dona_test[dona_test$CNTMLIF==1, ]
test_twice = dona_test[dona_test$CNTMLIF==2, ]
test_loyal = dona_test[dona_test$CNTMLIF>=3, ]
test_loyal = transform(test_loyal, AVG=CNTRLIF/CNTMLIF)
test_loyal = transform(test_loyal, vip = ifelse(CONLARG>100, 1, 0))

###----- On Group ONCE: those that donated only once -----
pred_once = predict(once_pois, newdata = test_once, type='response')
plot(pred_once)
plot(test1_predict)
expect_once = pred_once * test1_predict     # the expected dollar contribution amount
sse_once = sum((test_once$TARGDOL - expect_once)^2)
sqrt(sse_once/length(pred_once))            # RMSE for ONCE is $4.4927
plot(expect_once)
plot(test_once$TARGDOL)

###----- On Group TWICE: those that donated exactly twice -----
pred_twice = predict(twi_lin, newdata = test_twice)
expect_twice = pred_twice * test2_predict
notNA_twice = which(!is.na(expect_twice))
sse_twice = sum((test_twice$TARGDOL[notNA_twice] - expect_twice[notNA_twice])^2)
sqrt(sse_twice/length(notNA_twice))         # RMSE for TWICE is $4.8382


###----- On Group LOYAL: those that have donated at least 3 times -----
pred_loyal = predict(lin_int, newdata = test_loyal)
plot(pred_loyal)
expect_loyal = pred_loyal * test3_predict
plot(expect_loyal)
notNA_loyal = which(!is.na(expect_loyal))
sse_loyal = sum((test_loyal$TARGDOL[notNA_loyal] - expect_loyal[notNA_loyal])^2)
sqrt(sse_loyal/length(notNA_loyal))         # RMSE for LOYAL is $6.3861


## FULL TEST SET STATISTICS ---- RMSE = $5.200586
RMSE = sqrt((sse_once+sse_twice+sse_loyal)/(length(expect_once)+length(notNA_twice)+length(notNA_loyal)))

## 1000 Donors w/ Highest Potentials Consist of the Highest Expected 934 from Loyal, 65 from Twice, and 1 From Once
target_donor = c(which(expect_once>6.4378), which(expect_twice>6.4378), which(expect_loyal>6.4378))
target_donor = names(target_donor)
PAYOFF = sum(dona_test[target_donor, ]$TARGDOL)  # = $10,160.05; theoretical maximum = $23,760.98
sort(dona_test[target_donor, ]$TARGDOL)          # 498 out of 1000 didn't actually donate in the 2010 fall campaign, suggesting ample room for improvement on our modeling, especially in the classification stage. 
