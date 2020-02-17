library(xts)
library(forecast)
library(ggplot2)
library(readxl)
library(car)

library(parallel)
library(MASS)
library(foreach)
library(doParallel)

options(digits=3)

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
ddts = window(ddts, start = "2006-07-30")

YNames <- colnames(ddts)[substr(colnames(ddts), 1, 2) == "P_"] # Asya: tidyselect
XNames <- colnames(ddts)[substr(colnames(ddts), 1, 2) != "P_"]

XNames = c("BRENT", "GAS_Henry", "Coal", "Urea")
ddts = ddts[, c(YNames, XNames)]


# Заполним пропуски предыдущими значениями там, 
# где не было торгов и первыми известными значениями там, где нет начала ряда
ddts = na.locf(ddts)

# All exogeneous vars are lagged
ddts[,XNames] = lag(ddts[, XNames])

ddts = na.omit(ddts)
# ddts[,XNames] = na.locf(ddts[,XNames], fromLast = TRUE)


dd = data.frame(DATE=index(ddts), coredata(ddts))



some_series = ggplot(data = dd) + geom_line(aes(x = DATE, y =P_AMM_FOB_CARIBBEAN, color = "Caribbean")) +
  geom_line(aes(x = DATE, y = P_AMM_CFR_NW_EUR, color = "North-West Europe")) + 
  scale_colour_manual("", values = c("Caribbean"="black", "North-West Europe"="blue")) +
  xlab("Date") + ylab("Spot price") + ggtitle("Ammonia spot prices")
plot(some_series)
plot.xts(ddts[,YNames[1:3]], screens = factor(1, 1), auto.legend = TRUE)
autoplot(ddts[,YNames[1:4]])
ddts = ddts[,colSums(is.na(ddts)) == 0]




# data - xts, test_start - граница раздела тренировочной и тестовой выборки, metric - метрика качества прогноза
# горизонт прогноза для модели на каждой итерации


# Получить предыдущую дату
get_prev = function(data, ind){
  return(index(data)[which(index(data) == ind) - 1])
}

reshape_metrics = function(frame, names, model_names){
  new_frame = array(0, dim = c(floor(nrow(frame) / length(names)), ncol(frame), length(names)), dimnames = list(1:floor(nrow(frame) / length(names)), model_names, names))
  for (i in 1:length(names)){
    new_frame[,,i] = as.matrix(frame[c(startsWith(rownames(frame), names[i])),])
  }
  return(new_frame)
}


model_names = c(
  "auto.arima", 
  "ARIMA(1, d, 0)",
  # "sarima", 
  "auto.arimax"
  # "lm"
)


benchmark = function(data, xdata, expanding = "exp",indiff = "diff", test_start, metric, horizon, model_names, parallel){
  # Сохраним датасет в исходных значениях
  data_level = cbind(data) #Глубокое копирование на всякий случай
  xdata_level = cbind(xdata)
  # Если надо, преобразуем до разностей
  if (indiff == "diff"){
    data = na.omit(diff(data))
    xdata = na.omit(diff(xdata))
  } else if (indiff == "diff+xlevel") {
    data = na.omit(diff(data))
    xdata = na.omit(cbind(diff(xdata), xdata_level))
  }  else if (indiff == "diff+ylevel") {
    data = na.omit(diff(data))
    xdata = na.omit(cbind(diff(xdata), lag(data_level)))
  } else if (indiff == "diff+xlevel+ylevel") {
    data = na.omit(diff(data))
    xdata = na.omit(cbind(diff(xdata), xdata_level, lag(data_level)))
  } 
  
  
  
  
  # Вычислим количество моделей и возьмём даты в качестве индексов
  n_models = length(model_names)
  indeces = index(data)
  
  numCores = 1
  if (parallel == "work"){
    numCores <- detectCores() - 1
  } else if (parallel == "full"){
    numCores <- detectCores()
  }
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  print(numCores)
  

   # Идём по всем горизонтам
  r = foreach(h = 1:horizon,
           .combine = "cbind",
           .export = c("get_prev"),
           .packages = c("xts", "forecast", "ggplot2"),
           .verbose = FALSE) %dopar% {
             

    
    indeces_test = indeces[indeces >= test_start] #Ищем все стартовые точки, которые позже начала тестовой выборки
    indeces_test = indeces_test[1: (length(indeces_test) - h + 1)] #Последняя стартовая точка 
    
    print(length(indeces_test))
    
    # Массив для результатов
    results = array(0, dim = c(length(indeces_test),n_models, ncol(data)), dimnames = list((1:length(indeces_test)), model_names,colnames(data)))
    
    # Пройдём по всем точкам разбиения
    for (i in (1 : length(indeces_test))){
      
      start = 1
      # Если окно скользящее, то старт тоже двигается
      if (expanding == "exp"){
        start = i
      }
      
      # Тренировочные данные в разностях и тестовые данных в уровнях. Экзогенные переменные для теста тоже в разностях 
      train = window(data, start = indeces[start], end = get_prev(data, indeces_test[i]))
      test = window(data, start = indeces_test[i])
      test_level = window(data_level, start = indeces_test[i])
      
      train_x = window(xdata,start = indeces[start], end = get_prev(xdata, indeces_test[i]))
      test_x = window(xdata, start = indeces_test[i])
      test_x_level =  window(xdata_level, start = indeces_test[i])
      
      # print(length(indeces_test))
      print(h)
      # Пройдёмся по каждому ряду и оценим все модели.
      for (name in colnames(data)){
        # print(2)
        # Тут чёрное шаманство. По-факту, считается кумулятивная сумма на дату последнего
        # тренировочного элемента в истинных значениях и прогноза в разностях. И от этого считается метрика.
        arima = auto.arima(train[,name], max.P = 0, max.Q = 0, max.D = 0)
        results[i,"auto.arima", name] = accuracy(tail(cumsum(c(as.vector(data_level[get_prev(data, indeces_test[i])]) ,forecast(arima, h = h)$mean)), -1), head(test_level, h)[, name])[,metric]
        
        
        ar = auto.arima(train[,name], max.p = 1, max.q = 0,  max.P = 0, max.Q = 0, max.D = 0)
        results[i,"ARIMA(1, d, 0)", name] = accuracy(tail(cumsum(c(as.vector(data_level[get_prev(data, indeces_test[i])]) ,forecast(ar, h = h)$mean)), -1), head(test_level, h)[, name])[,metric]
        
        # sarima = auto.arima(train[,name])
        # results[i,"sarima", name] = accuracy(forecast(sarima, h = h)$mean, head(test, h)[, name])[,metric]
        
        arimax = auto.arima(train[,name], xreg = train_x)
        # print(forecast(arimax, h = h, xreg = test_x)$mean)
        results[i,"auto.arimax", name] = accuracy(tail(cumsum(c(as.vector(data_level[get_prev(data, indeces_test[i])]) ,forecast(arimax, h = h, xreg = test_x)$mean)), -1), head(test_level, h)[, name])[,metric]
        
        # reg = lm("P_AMM_FOB_BLACK_SEA~BRENT", data = cbind(data,xdata))
        # results[i,"lm", name] = accuracy(tail(cumsum(c(as.vector(data_level[get_prev(data, indeces_test[i])]) ,predict(reg, head(test_x_level, h)))), -1),as.vector(head(test_level, h)))[,metric]
        # print(accuracy(predict(reg, head(test_x, h)), as.vector(head(test, h))))
      }
      
    }
    # Пишем результаты
    print(colMeans(results))
    return(colMeans(results))
    # final[h, , ] = colMeans(results)
    print(100500)
   }
  stopCluster(cl)

  return(reshape_metrics(t(r), names = colnames(data), model_names = model_names))
}

data = ddts[,YNames]
xdata = ddts[, XNames]


XN = list("BRENT", "GAS_Henry", "Coal", "Urea", c("BRENT", "GAS_Henry", "Coal", "Urea"))
indiff = c("diff", "diff+xlevel", "diff+ylevel", "diff+xlevel+ylevel")
expanding = c("exp", "noexp")

library(openxlsx)




i = 1

wb <- createWorkbook()

for (name in YNames){
  addWorksheet(wb, name)
}

for (xset in XN){
  for (ind in indiff){
    for (exp in expanding){
      metrics = benchmark(data[,1], 
                          xdata[,xset],
                          indiff = ind,
                          expanding = exp,
                          test_start = "2018-11-18",
                          metric = "MAPE",
                          horizon = 12,
                          model_names = model_names,
                          parallel = "full") 
      print(i/16)
      
      for (name in dimnames(metrics)[[3]]){
        writeData(wb, name, t(c(ind, exp, xset)), startCol = 1, startRow = i)
        writeData(wb, name, metrics[,,name], startCol = 1, startRow = i+2)
        i = i + 16
      }
      
      
    }
  }
}

saveWorkbook(wb, file = "benchmarks2.xlsx", overwrite = TRUE)

metrics
metrics_2 = benchmark(data[,1], xdata, indiff = TRUE, expanding = TRUE, test_start = "2018-06-03", metric = "MAPE", horizon = 12, model_names = model_names, parallel = TRUE)
autoplot(ts(metrics_2[,1:3,1]))

system.time(benchmark(data[,1:2], xdata, indiff = TRUE, expanding = TRUE, test_start = "2018-01-07", metric = "MAPE", horizon = 30, model_names = model_names, parallel = TRUE))
