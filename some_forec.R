library(xts)
library(forecast)
library(ggplot2)

# Data preparation

dd <- read.csv("data_coal.csv", sep = ";")
dd$DATE <- as.Date(paste(dd$DATE), format = "%d.%m.%Y")

ddts <- xts(dd[, 2:ncol(dd)], order.by = dd$DATE)

YNames <- colnames(ddts)[substr(colnames(ddts), 1, 2) == "P_"]
XNames <- colnames(ddts)[substr(colnames(ddts), 1, 2) != "P_"]

ddts = ddts[,colSums(is.na(ddts)) == 0]

# generating first lag

ddts[1:nrow(ddts), 2] = lag(ddts[1:nrow(ddts), 1])
ddts = ddts[2:nrow(ddts)]

# generating fitst_dif

ddts[1:nrow(ddts), 3] = diff(ddts[1:nrow(ddts), 1])
ddts = ddts[2:nrow(ddts)]

# generating relative diff

ddts[1:nrow(ddts), 4] = diff(ddts[1:nrow(ddts), 3]) / diff(ddts[1:nrow(ddts), 2])
ddts = ddts[2:nrow(ddts)]

plot(tail(ddts[2:nrow(ddts), 4], n=400))

ddts = ddts[1:nrow(ddts),1:9]
ddts[,9] = 1
names(ddts) = c("dependent", "first_lag", "first_diff", "rel_first_diff", "FX_USD_CNY","FX_USD_Index","CPI_US","CPI_Eur", "Intercept" )

# ddts = window(ddts,start = "2010-01-03")
autoplot(ddts$dependent)

# Recursive forecasting function
forec_func = function(params, indep_test,lag){
  forec = c()
  y_last = indep_test[1,lag]

  for (i in seq(1:nrow(indep_test))){
    indep_test[i, lag] = y_last
    one_forec = indep_test[i,] %*% params
    forec = c(forec, one_forec)
    y_last = one_forec
  }
  
  return(data.frame(time = tail(dd$DATE, n = nrow(indep_test)), forec = forec))
  
}



# Y_t chunk

indep = ddts[1:nrow(ddts),c(2,5:6,9)]
dep = ddts[1:nrow(ddts),1]




trainsize = 0.95
indep_train = indep[1: floor(nrow(indep)*trainsize),]
indep_test = indep[floor(nrow(indep)*trainsize + 1): nrow(indep),]

dep_train = dep[1: floor(nrow(dep)*trainsize),]
dep_test = dep[floor(nrow(dep)*trainsize + 1) : nrow(dep),]




ols_coefs = solve(t(indep_train) %*% indep_train) %*% t(indep_train) %*% dep_train



plot(dep)

results = data.frame(time = tail(dd$DATE, n = nrow(dep)), tr = dep)
names(results) = c('time', 'true')





forec_1 = data.frame(time = dd$DATE[floor(nrow(dep)*trainsize + 1) : nrow(dep)], forec = indep_test %*% ols_coefs)
names(forec_1) = c('time', 'forec')



#Recursive forecast


forec_2 = forec_func(ols_coefs, indep_test, lag = "first_lag")
 ggplot() + 
  geom_line(data=results[400:nrow(results),], aes(x=time, y = true)) + 
  # geom_line(data = forec_1, aes(x=time, y=forec),color = "#E7B800")+
  geom_line(data = forec_2, aes(x=time, y=forec), color = "#00AFBB")



#Dynamic model optimizer

data = indep_train
y_tr = dep_train

# start = t-600

err_sum = function(params){
  start = 1
  res = c(data$first_lag[start])
  for (i in seq(2,nrow(data))){
    data$first_lag[i] = data[i-1,] %*% params
    res = c(res, data$first_lag[i])
  }
  
  error = sum((y_tr - res)^2)
  return(error)
}




opt_result_1 <- optim(par = rep(1, ncol(indep_train)),          # начальная точка
                    fn = err_sum,       # минимизируемая функция
                    method = "BFGS",   # метод численной оптимизации
                    control = list("maxit" = 1000))        

forec_dyn_1 = forec_func(opt_result_1$par, indep_test, lag = "first_lag")


# start = t - 400

err_sum = function(params){
  start = 200
  res = c(data$first_lag[start])
  for (i in seq(2,nrow(data))){
    data$first_lag[i] = data[i-1,] %*% params
    res = c(res, data$first_lag[i])
  }
  
  error = sum((y_tr - res)^2)
  return(error)
}




opt_result_2 <- optim(par = rep(1, ncol(indep_train)),          # начальная точка
                      fn = err_sum,       # минимизируемая функция
                      method = "BFGS",   # метод численной оптимизации
                      control = list("maxit" = 1000))        

forec_dyn_2 = forec_func(opt_result_2$par, indep_test, lag = "first_lag")

# start = t - 200

err_sum = function(params){
  start = 300
  res = c(data$first_lag[start])
  for (i in seq(2,nrow(data))){
    data$first_lag[i] = data[i-1,] %*% params
    res = c(res, data$first_lag[i])
  }
  
  error = sum((y_tr - res)^2)
  return(error)
}


opt_result_3 <- optim(par = rep(1, ncol(indep_train)),          # начальная точка
                      fn = err_sum,       # минимизируемая функция
                      method = "BFGS",   # метод численной оптимизации
                      control = list("maxit" = 1000))        

forec_dyn_3 = forec_func(opt_result_3$par, indep_test, lag = "first_lag")






ggplot() + 
  geom_line(data=results[1:nrow(results),], aes(x=time, y = true)) + 
  # geom_line(data = forec_1, aes(x=time, y=forec),color = "#E7B800")+
  geom_line(data = forec_2, aes(x=time, y=forec), color = "orange") + 
  geom_line(data = forec_dyn_1, aes(x=time, y=forec), color = "red") +
  geom_line(data = forec_dyn_2, aes(x=time, y=forec), color = "green") +
  geom_line(data = forec_dyn_3, aes(x=time, y=forec), color = "blue")


# In/out of sample ols forecast

ols_insample = data.frame(forec = indep_train %*% ols_coefs)
ols_insample$time = head(results$time, n = nrow(ols_insample))
ggplot() + 
  geom_line(data=results[1:nrow(results),], aes(x=time, y = true)) + 
  # geom_line(data = forec_1, aes(x=time, y=forec),color = "#E7B800")+
  geom_line(data = forec_2, aes(x=time, y=forec), color = "orange") +
  geom_line(data = ols_insample, aes(x=time, y=dependent), color = "orange")


# In/out of sample dyn_1 forecast

dyn_1_insample = data.frame(forec = indep_train %*% opt_result_1$par)
dyn_1_insample$time = head(results$time, n = nrow(ols_insample))


ggplot() + 
  geom_line(data=results[1:nrow(results),], aes(x=time, y = true)) + 
  geom_line(data = forec_dyn_1, aes(x=time, y=forec), color = "red") +
  geom_line(data = dyn_1_insample, aes(x=time, y=forec), color = "red")

# In/out of sample dyn_2 forecast

dyn_2_insample = data.frame(forec = indep_train %*% opt_result_2$par)
dyn_2_insample$time = head(results$time, n = nrow(dyn_2_insample))


ggplot() + 
  geom_line(data=results[1:nrow(results),], aes(x=time, y = true)) + 
  geom_line(data = forec_dyn_2, aes(x=time, y=forec), color = "green") +
  geom_line(data = dyn_2_insample, aes(x=time, y=forec), color = "green")

# In/out of sample dyn_3 forecast

dyn_3_insample = data.frame(forec = indep_train %*% opt_result_3$par)
dyn_3_insample$time = head(results$time, n = nrow(dyn_3_insample))


ggplot() + 
  geom_line(data=results[1:nrow(results),], aes(x=time, y = true)) + 
  geom_line(data = forec_dyn_3, aes(x=time, y=forec), color = "red") +
  geom_line(data = dyn_3_insample, aes(x=time, y=forec), color = "red")

# firstdiff_chunk 

dep = ddts[1:nrow(ddts),3]
indep = ddts[1:nrow(ddts),c(2,5:6,9)]
indep$first_lag = lag(dep)
indep =  indep[2:nrow(indep)]
dep = dep[2:nrow(dep)]



trainsize = 0.95
indep_train = indep[1: floor(nrow(indep)*trainsize),]
indep_test = indep[floor(nrow(indep)*trainsize + 1): nrow(indep),]

dep_train = dep[1: floor(nrow(dep)*trainsize),]
dep_test = dep[floor(nrow(dep)*trainsize + 1) : nrow(dep),]



ols_coefs = solve(t(indep_train) %*% indep_train) %*% t(indep_train) %*% dep_train



plot(dep)

results = data.frame(time = tail(dd$DATE, n = nrow(dep)), tr = dep)
names(results) = c('time', 'true')





forec_1 = data.frame(time = dd$DATE[floor(nrow(dep)*trainsize + 1) : nrow(dep)], forec = indep_test %*% ols_coefs)
names(forec_1) = c('time', 'forec')



#Recursive forecast


forec_2 = forec_func(ols_coefs, indep_test, lag = "first_lag")
ggplot() + 
  geom_line(data=results[400:nrow(results),], aes(x=time, y = true)) + 
  # geom_line(data = forec_1, aes(x=time, y=forec),color = "#E7B800")+
  geom_line(data = forec_2, aes(x=time, y=forec), color = "#00AFBB")



#Dynamic model optimizer

data = indep_train
y_tr = dep_train

# start = t-600

err_sum = function(params){
  start = 1
  res = c(data$first_lag[start])
  for (i in seq(2,nrow(data))){
    data$first_lag[i] = data[i-1,] %*% params
    res = c(res, data$first_lag[i])
  }
  
  error = sum((y_tr - res)^2)
  return(error)
}




opt_result_1 <- optim(par = rep(1, ncol(indep_train)),          # начальная точка
                      fn = err_sum,       # минимизируемая функция
                      method = "BFGS",   # метод численной оптимизации
                      control = list("maxit" = 1000))        

forec_dyn_1 = forec_func(opt_result_1$par, indep_test, lag = "first_lag")


# start = t - 400

err_sum = function(params){
  start = 200
  res = c(data$first_lag[start])
  for (i in seq(2,nrow(data))){
    data$first_lag[i] = data[i-1,] %*% params
    res = c(res, data$first_lag[i])
  }
  
  error = sum((y_tr - res)^2)
  return(error)
}




opt_result_2 <- optim(par = rep(1, ncol(indep_train)),          # начальная точка
                      fn = err_sum,       # минимизируемая функция
                      method = "BFGS",   # метод численной оптимизации
                      control = list("maxit" = 1000))        

forec_dyn_2 = forec_func(opt_result_2$par, indep_test, lag = "first_lag")

# start = t - 200

err_sum = function(params){
  start = 300
  res = c(data$first_lag[start])
  for (i in seq(2,nrow(data))){
    data$first_lag[i] = data[i-1,] %*% params
    res = c(res, data$first_lag[i])
  }
  
  error = sum((y_tr - res)^2)
  return(error)
}


opt_result_3 <- optim(par = rep(1, ncol(indep_train)),          # начальная точка
                      fn = err_sum,       # минимизируемая функция
                      method = "BFGS",   # метод численной оптимизации
                      control = list("maxit" = 1000))        

forec_dyn_3 = forec_func(opt_result_3$par, indep_test, lag = "first_lag")






ggplot() + 
  geom_line(data=results[550:nrow(results),], aes(x=time, y = true)) + 
  # geom_line(data = forec_1, aes(x=time, y=forec),color = "#E7B800")+
  geom_line(data = forec_2, aes(x=time, y=forec), color = "orange") + 
  geom_line(data = forec_dyn_1, aes(x=time, y=forec), color = "red") +
  geom_line(data = forec_dyn_2, aes(x=time, y=forec), color = "green") +
  geom_line(data = forec_dyn_3, aes(x=time, y=forec), color = "blue")


# In/out of sample ols forecast

ols_insample = data.frame(forec = indep_train %*% ols_coefs)
ols_insample$time = head(results$time, n = nrow(ols_insample))
ggplot() + 
  geom_line(data=results[1:nrow(results),], aes(x=time, y = true)) + 
  # geom_line(data = forec_1, aes(x=time, y=forec),color = "#E7B800")+
  geom_line(data = forec_2, aes(x=time, y=forec), color = "orange") +
  geom_line(data = ols_insample, aes(x=time, y=dependent), color = "orange")


# In/out of sample dyn_1 forecast

dyn_1_insample = data.frame(forec = indep_train %*% opt_result_1$par)
dyn_1_insample$time = head(results$time, n = nrow(ols_insample))


ggplot() + 
  geom_line(data=results[1:nrow(results),], aes(x=time, y = true)) + 
  geom_line(data = forec_dyn_1, aes(x=time, y=forec), color = "red") +
  geom_line(data = dyn_1_insample, aes(x=time, y=forec), color = "red")

# In/out of sample dyn_2 forecast

dyn_2_insample = data.frame(forec = indep_train %*% opt_result_2$par)
dyn_2_insample$time = head(results$time, n = nrow(dyn_2_insample))


ggplot() + 
  geom_line(data=results[1:nrow(results),], aes(x=time, y = true)) + 
  geom_line(data = forec_dyn_2, aes(x=time, y=forec), color = "green") +
  geom_line(data = dyn_2_insample, aes(x=time, y=forec), color = "green")

# In/out of sample dyn_3 forecast

dyn_3_insample = data.frame(forec = indep_train %*% opt_result_3$par)
dyn_3_insample$time = head(results$time, n = nrow(dyn_3_insample))


ggplot() + 
  geom_line(data=results[1:nrow(results),], aes(x=time, y = true)) + 
  geom_line(data = forec_dyn_3, aes(x=time, y=forec), color = "red") +
  geom_line(data = dyn_3_insample, aes(x=time, y=forec), color = "red")





# ols_coefs = solve(t(indep_train) %*% indep_train) %*% t(indep_train) %*% dep_train
# 
# 
# 
# plot(dep)
# 
# results = data.frame(time = dd$DATE[5:length(dd$DAT)], tr = dep)
# names(results) = c('time', 'true')
# 
# 
# 
# 
# 
# forec_1 = data.frame(time = dd$DATE[floor(nrow(dep)*trainsize + 1) : nrow(dep)], forec = indep_test %*% ols_coefs)
# names(forec_1) = c('time', 'forec')
# 
# 
# 
# #Recursive forecast
# 
# 
# forec_2 = forec_func(ols_coefs, indep_test, lag = "first_lag")
# ggplot() + 
#   geom_line(data=results[400:nrow(results),], aes(x=time, y = true)) + 
#   # geom_line(data = forec_1, aes(x=time, y=forec),color = "#E7B800")+
#   geom_line(data = forec_2, aes(x=time, y=forec), color = "#00AFBB")
# 
# 
# 
# #Dynamic model optimizer
# 
# data = indep_train
# y_tr = dep_train
# 
# 
# # start = t - 400
# 
# err_sum = function(params){
#   start = 1
#   res = c(data$first_lag[start])
#   for (i in seq(2,nrow(data))){
#     data$first_lag[i] = data[i-1,] %*% params
#     res = c(res, data$first_lag[i])
#   }
#   
#   error = sum((y_tr - res)^2)
#   return(error)
# }
# 
# 
# 
# 
# opt_result_1 <- optim(par = rep(1, ncol(indep_train)),          # начальная точка
#                       fn = err_sum,       # минимизируемая функция
#                       method = "BFGS",   # метод численной оптимизации
#                       control = list("maxit" = 1000))        
# 
# forec_dyn_1 = forec_func(opt_result_1$par, indep_test, lag = "first_lag")
# 
# 
# # start = t - 400
# 
# err_sum = function(params){
#   start = 200
#   res = c(data$first_lag[start])
#   for (i in seq(2,nrow(data))){
#     data$first_lag[i] = data[i-1,] %*% params
#     res = c(res, data$first_lag[i])
#   }
#   
#   error = sum((y_tr - res)^2)
#   return(error)
# }
# 
# 
# 
# 
# opt_result_2 <- optim(par = rep(1, ncol(indep_train)),          # начальная точка
#                       fn = err_sum,       # минимизируемая функция
#                       method = "BFGS",   # метод численной оптимизации
#                       control = list("maxit" = 1000))        
# 
# forec_dyn_2 = forec_func(opt_result_2$par, indep_test, lag = "first_lag")
# 
# # start = t - 200
# 
# err_sum = function(params){
#   start = 550
#   res = c(data$first_lag[start])
#   for (i in seq(2,nrow(data))){
#     data$first_lag[i] = data[i-1,] %*% params
#     res = c(res, data$first_lag[i])
#   }
#   
#   error = sum((y_tr - res)^2)
#   return(error)
# }
# 
# 
# opt_result_3 <- optim(par = rep(1, ncol(indep_train)),          # начальная точка
#                       fn = err_sum,       # минимизируемая функция
#                       method = "BFGS",   # метод численной оптимизации
#                       control = list("maxit" = 1000))        
# 
# forec_dyn_3 = forec_func(opt_result_3$par, indep_test, lag = "first_lag")
# 
# 
# 
# 
# ggplot() + 
#   geom_line(data=results[550:nrow(results),], aes(x=time, y = true)) + 
#   # geom_line(data = forec_1, aes(x=time, y=forec),color = "#E7B800")+
#   geom_line(data = forec_2, aes(x=time, y=forec), color = "black") + 
#   geom_line(data = forec_dyn_1, aes(x=time, y=forec), color = "red") +
#   geom_line(data = forec_dyn_2, aes(x=time, y=forec), color = "green") +
#   geom_line(data = forec_dyn_3, aes(x=time, y=forec), color = "blue")
# 
# 
# 
# 
