library(xts)
library(forecast)
library(ggplot2)
library(readxl)
library(xlsx)
# Data preparation

types = c("date", rep("numeric", 72))
dd = read_excel("data_amm.xlsx",sheet = 2, col_types = types)
colnames(dd)[1] = "DATE"


dd$DATE <- as.Date(dd$DATE, format = "%d.%m.%Y")

# Выкинем слишком короткие ряды

drops =  c("P_AMM_FOB_SE_ASIA","P_AMM_CFR_TURKEY", "P_AMM_CFR_SE_ASIA")
dd = dd[, !(names(dd) %in% drops)]

ddts <- xts(dd[, 2:ncol(dd)], order.by = dd$DATE)

# Точка старта так, чтобы экзогенные переменные тоже стратовали без пропусков
ddts = window(ddts, start = "2004-07-04")

YNames <- colnames(ddts)[substr(colnames(ddts), 1, 2) == "P_"]
XNames <- colnames(ddts)[substr(colnames(ddts), 1, 2) != "P_"]

# Заполним пропуски предыдущими значениями там, 
# где не было торгов и первыми известными значениями там, где нет начала ряда
ddts = na.locf(ddts)
ddts[,XNames] = na.locf(ddts[,XNames], fromLast = TRUE)


dd = data.frame(DATE=index(ddts), coredata(ddts))



some_series = ggplot(data = dd) + geom_line(aes(x = DATE, y =P_AMM_FOB_CARIBBEAN, color = "Caribbean")) +
  geom_line(aes(x = DATE, y = P_AMM_CFR_NW_EUR, color = "North-West Europe")) + 
  scale_colour_manual("", values = c("Caribbean"="black", "North-West Europe"="blue")) +
  xlab("Date") + ylab("Spot price") + ggtitle("Ammonia spot prices")
plot(some_series)
plot.xts(ddts[,YNames[1:3]], screens = factor(1, 1), auto.legend = TRUE)
autoplot(ddts[,YNames[1:4]])
ddts = ddts[,colSums(is.na(ddts)) == 0]

XNames = c("BRENT")
# Выделим отдельно только сами ряды
data = ddts[,YNames]
xdata = ddts[, XNames]


# data - xts, test_start - граница раздела тренировочной и тестовой выборки, metric - метрика качества прогноза
# горизонт прогноза для модели на каждой итерации


# Получить предыдущую дату
get_prev = function(data, ind){
  return(index(data)[which(index(data) == ind) - 1])
}

model_names = c("auto.arima", "ARIMA(1, d, 0)", "sarima", "auto.arimax")


benchmark = function(data, xdata, expanding = TRUE, test_start, metric, horizon, model_names){
  n_models = length(model_names)
  indeces = index(data)
  
  final = array(0, dim = c(horizon,n_models, ncol(data)), dimnames = list((1:horizon), model_names,colnames(data)))
  for (h in (1:horizon)){

  indeces_test = indeces[indeces >= test_start] #Ищем все стартовые точки, которые позже начала тестовой выборки
  indeces_test = indeces_test[1: (length(indeces_test) - h + 1)]
  
  # Массив для результатов
  results = array(0, dim = c(length(indeces_test),n_models, ncol(data)), dimnames = list((1:length(indeces_test)), model_names,colnames(data)))

  # Пройдём по всем точкам разбиения
  for (i in (1 : length(indeces_test))){
    
  start = 1
  
  if (expanding == FALSE){
    start = i
  }
    
  train = window(data, start = indeces[start], end = get_prev(data, indeces_test[i]))
  test = window(data, start = indeces_test[i])
  
  train_x = window(xdata,start = indeces[start], end = get_prev(xdata, indeces_test[i]))
  test_x = window(xdata, start = indeces_test[i])
  
  
  # Пройдёмся по каждому ряду
  for (name in colnames(data)){
      print(2)
      arima = auto.arima(train[,name], max.P = 0, max.Q = 0, max.D = 0)
      results[i,"auto.arima", name] = accuracy(forecast(arima, h = h)$mean, head(test, h)[, name])[,metric]
      
      
      ar = auto.arima(train[,name], max.p = 1, max.q = 0,  max.P = 0, max.Q = 0, max.D = 0)
      results[i,"ARIMA(1, d, 0)", name] = accuracy(forecast(ar, h = h)$mean, head(test, h)[, name])[,metric]
      
      sarima = auto.arima(train[,name])
      results[i,"sarima", name] = accuracy(forecast(sarima, h = h)$mean, head(test, h)[, name])[,metric]

      arimax = auto.arima(train[,name], xreg = train_x)
      results[i,"auto.arimax", name] = accuracy(forecast(arimax, h = h, xreg = test_x)$mean, head(test, h)[, name])[,metric]
      
    }
  
  }
  final[h, , ] = colMeans(results)
  print(1)
  }
  return(final)
}

metrics = benchmark(data, xdata, expanding = FALSE, test_start = "2019-12-15", metric = "MAPE", horizon = 2, model_names = model_names)
autoplot(ts(metrics[,1,1]))


  
 
  