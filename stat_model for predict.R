

library("DMwR")
library("Metrics")
library("tseries")
library("forecast")
library("TTR")
library("fUnitRoots")


cci = read.csv("E:\\Data\\CCI.CSV.csv", header = F)
attach(cci)

##时序分析
cc = ts(cci, frequency = 12, start = c(1990, 1))
cc = cc[, -1]
tsdisplay(cc, main = "CCI", xlab = "Time", ylab = "CCI")
decom = decompose(cc)
plot(decom)


##划分训练集和测试集
c = cci[0:204, ]
train = ts(c, frequency = 12, start = c(1990, 1))
train = train[, -1]
tsdisplay(train, main = "Train Set", xlab = "Time", ylab = "CCI")
test = cci[205:295, ]
test = test[, -1]
tsdisplay(test, main = "Test Set", xlab = "Time", ylab = "CCI")


##简单指数平滑
fore1 = HoltWinters(train, beta = FALSE, gamma = FALSE)
fore2 = forecast:::forecast.HoltWinters(fore1, h=91)
plot(fore2)

Box.test(fore2$residuals, lag=20, type="Ljung-Box")

rmse(fore2$mean, test)
regr.eval(fore2$mean, test)
regr.eval(test, fore2$mean)

plot(fore2, ylim = c(4000, 10000), main = "Forcast from First-Order ES Out-of-Sample", xlab = "Time", ylab = "CCI")
lines(cc) ##深蓝色为80%预测区间，浅蓝色为90%


##Holt指数平滑法
fore3 = HoltWinters(train, alpha = 0.999, beta = 0.020, gamma=FALSE, l.start=4680, b.start=5)
fore4 = forecast:::forecast.HoltWinters(fore3, h = 91)
plot(fore4)

Box.test(fore4$residuals, lag = 20, type = "Ljung-Box")

rmse(fore4$mean, test)
regr.eval(fore4$mean, test)
regr.eval(test, fore4$mean)

plot(fore4, ylim = c(4000, 10000), main = "Forcast from Holt ES Out-of-Sample", xlab = "Time", ylab = "CCI")
lines(cc)

plot(fore4, xlim = c(2007,2015), main = "Magnifying Forcast from Holt ES Out-of-Sample", xlab = "Time", ylab = "CCI")  ##放大后测试的图
lines(cc)


##Holt-Winters 指数平滑法
fore5 = HoltWinters(train, alpha = 0.88690, beta = 0.020, gamma = 0.999)
fore6 = forecast:::forecast.HoltWinters(fore5, h = 91)
plot(fore6)

Box.test(fore6$residuals, lag = 20, type="Ljung-Box")

rmse(fore6$mean, test)
regr.eval(test, fore6$mean)

plot(fore6, ylim = c(4000, 10000), main = "Forcast from Holt-Winters ES Out-of-Sample", xlab = "Time", ylab = "CCI")
lines(cc)

plot(fore6, xlim = c(2007,2015), main = "Magnifying Forcast from Holt-Winters ES Out-of-Sample", xlab = "Time", ylab = "CCI")  ##放大后测试的图
lines(cc)


##Holt指数平滑法  单步预测
a = 0
aa = 0
aaa = 0
d = 0
for(i in 1:91)
{
  ccc = cci[0:204 + i - 1, ]
  train1 = ts(ccc, frequency = 12, start = c(1990, 1))
  train1 = train1[, -1]
  fore7 = HoltWinters(train1, alpha = 0.999, beta = 0.020, gamma = FALSE)
  fore8 = forecast:::forecast.HoltWinters(fore7, h = 1)
  d[i] = fore8$mean
  test1 = cci[205 + i - 1, ]
  test1 = test1[, -1]
  a = a + rmse(fore8$mean, test1)
  aa = regr.eval(test1, fore8$mean)
  aaa = aaa + aa[4]
}
rmse = a / 91
rmse
mape = aaa /91
mape

d = ts(d, frequency = 12, start = c(2007,1))
plot(cc, main = "Forcast from Holt ES In-Sample", xlab = "Time", ylab = "CCI")
lines(d, col = 4, lwd = 1)

##放大后测试的图
plot(d, col = 4, main = "Magnifying Forcast from Holt ES In-Sample", xlab = "Time", ylab = "CCI")
lines(cc)



##Holt-Winters 指数平滑法  单步预测
a = 0
aa = 0
aaa = 0
e = 0
for(i in 1:91)
{
  cccc = cci[0:204 + i - 1, ]
  train2 = ts(cccc, frequency = 12, start = c(1990, 1))
  train2 = train2[, -1]
  fore9 = HoltWinters(train2, alpha = 0.88690, beta = 0.010, gamma = 0.999)
  fore10 = forecast:::forecast.HoltWinters(fore9, h = 1)
  e[i] = fore10$mean
  test2 = cci[205 + i - 1, ]
  test2 = test2[, -1]
  a = a + rmse(fore10$mean, test2)
  aa = regr.eval(test2, fore10$mean)
  aaa = aaa + aa[4]
}

rmse = a / 91
rmse
mape = aaa /91
mape

e = ts(d, frequency = 12, start = c(2007, 1))
plot(cc, main = "Forcast from Holt-Winters ES In-Sample", xlab = "Time", ylab = "CCI")
lines(e, col = 4, lwd = 1)

##放大后测试的图
plot(d, col = 4, main = "Magnifying Forcast from Holt-Winters ES In-Sample", xlab = "Time", ylab = "CCI")
lines(cc)

##差分检验
diff = diff(train, differences = 2)
plot.ts(diff, main = "Second-Order Difference")
acf(diff, lag.max = 20, main = "ACF Test")
acf(diff, lag.max = 20, plot = FALSE)
pacf(diff, lag.max = 20, main = "PACF Test")
pacf(diff, lag.max = 20, plot = FALSE)
auto.arima(diff)

##ARIMA模型
fore11 = arima(train, order = c(2, 2, 2))
fore12 = forecast:::forecast.Arima(fore11, h = 91)
plot(fore12)
acf(fore12$residuals, lag.max = 20, main = "ARIMA ACF Test")
Box.test(fore12$residuals, lag = 20, type="Ljung-Box")

rmse(fore12$mean, test)
regr.eval(test, fore12$mean)

plot(fore12, ylim = c(4000,10000), main = "Forcast from ARIMA(2,2,2) Out-of-Sample", xlab = "Time", ylab = "CCI")
lines(cc)

plot(fore12, xlim = c(2007,2015), main = "Magnifying Forcast from ARIMA(2,2,2) Out-of-Sample", xlab = "Time", ylab = "CCI")  ##放大后测试的图
lines(cc)


##ARIMA模型     单步预测
a = 0
aa = 0
aaa = 0
f = 0
for(i in 1:91)
{
  ccccc = cci[0:204 + i - 1, ]
  train3 = ts(ccccc, frequency = 12, start = c(1990, 1))
  train3 = train3[, -1]
  fore13 = arima(train3, order = c(2, 2, 2))
  fore14 = forecast:::forecast.Arima(fore13, h = 1)
  f[i] = fore14$mean
  test3 = cci[205 + i - 1, ]
  test3 = test3[, -1]
  a = a + rmse(fore14$mean, test3)
  aa = regr.eval(test3, fore14$mean)
  aaa = aaa + aa[4]
}
rmse = a / 91
rmse
mape = aaa /91
mape

f = ts(f, frequency = 12, start = c(2007, 1))
plot(cc, main = "Forcast from ARIMA(2,2,2) In-Sample", xlab = "Time", ylab = "CCI")
lines(f, col = 4, lwd = 1)

plot(f, col = 4, main = "Magnifying Forcast from ARIMA(2,2,2) In-Sample", xlab = "Time", ylab = "CCI")  ##放大后测试的图
lines(cc)

##季节性ARIMA模型
auto.arima(train, trace = T)

fore15 = arima(train, order = c(2, 2, 2), seasonal = c(1, 0, 1))
fore16 = forecast:::forecast.Arima(fore15, h = 91)
acf(fore16$residuals, lag.max = 20, main = "Seasonal ARIMA ACF Test")
Box.test(fore16$residuals, lag = 20, type="Ljung-Box")
plot(fore16)

rmse(fore16$mean, test)
regr.eval(test, fore16$mean)

plot(fore16, ylim = c(4000, 10000), main = "Forcast from Seasonal ARIMA(2,2,2)(1,0,1) Out-of-Sample", xlab = "Time", ylab = "CCI")
lines(cc)

plot(fore16, xlim = c(2007,2015), main = "Magnifying Forcast from Seasonal ARIMA(2,2,2)(1,0,1) Out-of-Sample", xlab = "Time", ylab = "CCI")  ##放大后测试的图
lines(cc)


##季节性ARIMA模型    单步预测
a = 0
aa = 0
aaa = 0
g = 0
for(i in 1:91)
{
  cccccc = cci[0:204 + i - 1, ]
  train4 = ts(cccccc, frequency = 12, start = c(1990, 1))
  train4 = train4[, -1]
  fore17 = arima(train4, order = c(2, 2, 2), seasonal = c(1, 0, 1))
  fore18 = forecast:::forecast.Arima(fore17, h = 1)
  g[i] = fore18$mean
  test4 = cci[205 + i - 1, ]
  test4 = test4[, -1]
  a = a + rmse(fore18$mean, test4)
  aa = regr.eval(test4, fore18$mean)
  aaa = aaa + aa[4]
}
rmse = a / 91
rmse
mape = aaa /91
mape

g = ts(g, frequency = 12, start = c(2007, 1))
plot(cc, main = "Forcast from Seasonal ARIMA(2,2,2)(1,0,1) In-Sample", xlab = "Time", ylab = "CCI")
lines(g,col = 4, lwd = 1)
plot(g, col = 4, main = "Magnifying Forcast from Seasonal ARIMA(2,2,2)(1,0,1) In-Sample", xlab = "Time", ylab = "CCI")  ##放大后测试的图
lines(cc)














