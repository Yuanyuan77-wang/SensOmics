install.packages("data.table")
install.packages("lubridate")
install.packages("midasml")
install.packages("alfred")
library(data.table)
library(lubridate)
library(midasml)
library(alfred)
##windows some mistakes in utf-8
Sys.setlocale(category = 'LC_ALL', locale = 'English_United States.1252')
##read data
gdp = readRDS('C:/Users/admin/Desktop/gdp_quarter.rds')
gdp = as.data.table(gdp)
month_data = readRDS('C:/Users/admin/Desktop/month_pingwen.rds')
month_data = as.data.table(month_data)
daily_data = readRDS('C:/Users/admin/Desktop/daily_pingwen.rds')
daily_data = as.data.table(daily_data)
#define estimation period
est.start = as.Date("2016-03-30")
est.end = as.Date("2019-12-31")
# month and daily parameters
y_data = gdp[, .(GDP)]
x_month_date = month_data$date
x_daily_date = daily_data$date
x_month_data = month_data[, .(x1, x2, x3, x5, x7, x9, x10, x12, x13, x15, x16, x18, x19, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x36, x37, x38, x39, x40, x41)]
x_daily_data = daily_data[, .(SH_volatility, HS300_volatility, SZ_volatility, SZ_turnover, Shibor, R001, baidu_zhaopin, baidu_zhaogong,	baidu_shiyejin,	IBOC, HS300_pe, SH_turnover, HS300_turnover)]
x_month_data = as.data.frame(x_month_data)
x_daily_data = as.data.frame(x_daily_data)
dim_month = dim(month_data)[2]-1
dim_daily = dim(daily_data)[2]-1
#other parameters
horizon = 3L
month_delay = 0
est_end = est.end
est_start = est.start
y_lag = 4
x_month_lag = 12
x_daily_lag = 3
x_quarter_lag = 4
legendre_degree = 3L
disp_flag = FALSE
x.quarterly_group = NULL
group_ar_lags = FALSE
real_time_predictions = FALSE
# storage
x_strm_out = x_strm = NULL
x_strd_out = x_strd = NULL
x_unstrm_out = x_unstrm = NULL
x_unstrd_out = x_unstrd = NULL
x_averagem_out = x_averagem = NULL
x_averaged_out = x_averaged = NULL
group_indexm = group_indexm_un = group_indexm_av = 0
group_indexd = group_indexd_un = group_indexd_av = 0
ref_in = ref_out = NULL

#month covarites
data.refdate = gdp$date
#data.refdate = data.refdate %m-% months(month_delay)
est.start_m = est_start
est.end_m = est_end
#est.start_m = est.start_m %m-% months(month_delay)
#est.end_m = est.end_m %m-% months(month_delay)
#data.refdate = data.refdate  %m-% months(horizon)
#est.end_m = est.end_m  %m-% months(horizon)
# MIDAS 结构
#month data
for (j_month in seq(dim_month)){
  j_data = scale(x_month_data[,j_month], center = TRUE, scale = TRUE)
  #j_data = x_month_data[,j_month]
  # get MIDAS structure:
  tmpm = mixed_freq_data(data.y = y_data, data.ydate = data.refdate, data.x = j_data, data.xdate = x_month_date,
                                x.lag = x_month_lag, y.lag = y_lag, horizon = 0, est_start, est.end, disp.flag = disp_flag)
  #tmpm1 = mixed_freq_data_single(data.ydate = data.refdate, data.x = j_data, data.xdate = x_month_date,
  #                       x.lag = x_month_lag, y.lag = y_lag, horizon = 0, est_start, est.end, disp.flag = disp_flag)
  # if(j_month==1){
  #   refm_in = tmpm$est.refdate
  #   refm_out = tmpm$out.refdate
  #   refm_in = refm_in %m+% months(month_delay) %m+% months(horizon)
  #   if(!is.null(refm_out))
  #     refm_out = refm_out %m+% months(month_delay) %m+% months(horizon)
  # }
  # get Legendre weights:
  tmpm_w = lb(legendre_degree,a=0,b=1,jmax=x_month_lag)
  # store aggregated case:
  x_strm = cbind(x_strm, tmpm$est.x%*%tmpm_w)
  x_strm_out = cbind(x_strm_out, tmpm$out.x%*%tmpm_w)
  # store unrestricted case:
  x_unstrm = cbind(x_unstrm, tmpm$est.x)
  x_unstrm_out = cbind(x_unstrm_out, tmpm$out.x)
  # store averages:
  x_averagem = cbind(x_averagem, rowMeans(tmpm$est.x))
  x_averagem_out = cbind(x_averagem_out, rowMeans(tmpm$out.x))
  # store group indices:
  group_indexm = c(group_indexm, rep(max(group_indexm)+1,times=legendre_degree+1))
  group_indexm_un = c(group_indexm_un, rep(max(group_indexm_un)+1,times=x_month_lag))
  group_indexm_av = c(group_indexm_av, max(group_indexm_av)+1)
}
#daily data
for (j_daily in seq(dim_daily)){
  j_data = scale(x_daily_data[,j_daily], center = TRUE, scale = TRUE)
  #j_data = x_daily_data[,j_daily]
  # get MIDAS structure:
  tmpd = mixed_freq_data(data.y = y_data, data.ydate = data.refdate, data.x = j_data, data.xdate = x_daily_date,
                         x.lag = x_daily_lag, y.lag = y_lag, horizon = 3, est_start, est.end_m, disp.flag = disp_flag)
  #tmpd1 = mixed_freq_data_single(data.ydate = data.refdate, data.x = j_data, data.xdate = x_daily_date,
  #                              x.lag = x_daily_lag, y.lag = y_lag, horizon = 3, est_start, est.end_m, disp.flag = disp_flag)
  # if(j_daily==1){
  #       refd_in = tmpd$est.refdate
  #       refd_out = tmpd$out.refdate
  #       lubridate::month(refd_in) = lubridate::month(refd_in)+horizon
  #       if(!is.null(refd_out))
  #         lubridate::month(refd_out) = lubridate::month(refd_out)+horizon
  # }
  # get Legendre weights:
  tmpd_w = lb(legendre_degree,a=0,b=1,jmax=x_daily_lag)
  # store aggregated case:
  x_strd = cbind(x_strd, tmpd$est.x%*%tmpd_w)
  x_strd_out = cbind(x_strd_out, tmpd$out.x%*%tmpd_w)
  # store unrestricted case:
  x_unstrd = cbind(x_unstrd, tmpd$est.x)
  x_unstrd_out = cbind(x_unstrd_out, tmpd$out.x)
  # store averages:
  x_averaged = cbind(x_averaged, rowMeans(tmpd$est.x))
  x_averaged_out = cbind(x_averaged_out, rowMeans(tmpd$out.x))
  # store group indices:
  group_indexd = c(group_indexd, rep(max(group_indexd)+1,times=legendre_degree+1))
  group_indexd_un = c(group_indexd_un, rep(max(group_indexd_un)+1,times=x_daily_lag))
  group_indexd_av = c(group_indexd_av, max(group_indexd_av)+1)
}
#聚合month and daily
x_in = cbind(x_strd, x_strm)
x_in_un = cbind(x_unstrd, x_unstrm)
x_strd_out = x_strd_out[1:10, ]
x_unstrd_out = x_unstrd_out[1:10, ]
x_out = cbind(x_strd_out, x_strm_out)
x_out_un = cbind(x_unstrd_out, x_unstrm_out)
y_in = tmpm$est.y
#y_in = scale(y_in, center = TRUE, scale = TRUE)
y_out = tmpm$out.y
#y_out = scale(y_out, center = TRUE, scale = TRUE)
gindex = sort(rep(1:44,times=4))
#模型拟合
#MIDAS#只有月度数据
fit0 = sglfit(x_in, y_in, gamma = 1, gindex = gindex)
predict_result0 = predict(fit0, x_strm_out)
result0 = cbind(predict_result0, y_out)

#sg-lasso
fit = sglfit(x_in, y_in,gamma = 0.5, gindex = gindex)
fit1 = cv.sglfit(x_in, y_in, lambda = NULL, gamma = 0.5, gindex = gindex, nfolds = 10)
predict_result1 = predict(fit1, x_out)
result1 = cbind(predict_result1, y_out)
#group lasso
fit2 = cv.sglfit(x_in, y_in, lambda = NULL, gamma = 0, gindex = gindex, nfolds = 10)
predict_result2 = predict(fit2, x_out)
result2 = cbind(predict_result2, y_out)
#lasso
fit3 = cv.sglfit(x_in, y_in, lambda = NULL, gamma = 1, gindex = gindex, nfolds = 10)
predict_result3 = predict(fit3, x_out)
result3 = cbind(predict_result3, y_out)
#U-MIDAS#
#sg-lasso
fit4 = cv.sglfit(x_in_un, y_in, lambda = NULL, gamma = 0.5, gindex = gindex, nfolds = 10)
predict_result4 = predict(fit4, x_out_un)
result4 = cbind(predict_result4, y_out)
#lasso
fit5 = cv.sglfit(x_in_un, y_in, lambda = NULL, gamma = 1, gindex = gindex, nfolds = 10)
predict_result5 = predict(fit5, x_out_un)
result5 = cbind(predict_result5, y_out)
#bridge
fit6 = cv.sglfit(x_in_un, y_in, lambda = NULL, gamma = 0, gindex = gindex, nfolds = 10)
predict_result6 = predict(fit6, x_out_un)
result6 = cbind(predict_result6, y_out)
#AR models : lag = 0 #

#adl-midas#
fit7 = midas.ardl(y_, x, z = NULL, loss_choice = c("mse","logit"), poly_choice = c("legendre","expalmon","beta"), poly_spec = 0, legendre_degree = 3, nbtrials = 500)









fit2 = tscv.sglfit(x = x_in, y = y_in, gindex = gindex, gamma = 0.5, standardize = FALSE, intercept = FALSE)
fit3 = predict(fit1, x_out)
fit4 = reg.sgl(x = x_in, y = y_in, gamma = 0.5, gindex = gindex, method_choice = "cv")
result = cbind(fit3, y_out)
fit5 = sglfit(x = x_in, y = y_in, gindex = gindex, gamma = 0.5, standardize = FALSE, intercept = FALSE)
fit6 = predict(fit4, x_out)
data(rgdp_vintages)
rgdp_vintages$date
rgdp_vintages$time_series
rgdp_vintages$realtime_period # real time dates