# 1. Naive Tahmin
naive_fit <- c(NA_real_, head(value, n - 1))
naive_next <- tail(value, 1)
naive_acc <- calc_accuracy(value, naive_fit, "Naive Tahmin", naive_next)
plot_forecast(dates, value, naive_fit, next_date, naive_next, "Naive", "naive_forecast_plot.png")

# 2. Hareketli Ortalama (12 Aylık)
k_ma <- 12
ma_fit <- rep(NA_real_, n)
for (i in (k_ma + 1):n) ma_fit[i] <- mean(value[(i - k_ma):(i - 1)])
ma_next <- mean(tail(value, k_ma))
ma_acc <- calc_accuracy(value, ma_fit, "Hareketli Ortalama (12)", ma_next)
plot_forecast(dates, value, ma_fit, next_date, ma_next, "HO(12)", "moving_average_plot.png")

# 3. Ağırlıklı Hareketli Ortalama (6 Aylık)
k_wma <- 6
w_raw <- seq_len(k_wma)
w <- w_raw / sum(w_raw)
wma_fit <- rep(NA_real_, n)
for (i in (k_wma + 1):n) wma_fit[i] <- sum(w * value[(i - k_wma):(i - 1)])
wma_next <- sum(w * tail(value, k_wma))
wma_acc <- calc_accuracy(value, wma_fit, "Agirlikli HO (6)", wma_next)
plot_forecast(dates, value, wma_fit, next_date, wma_next, "AHO(6)", "weighted_moving_average_plot.png")

# 4. Üstel Düzeltme (SES)
es_model <- ses(ts_data, h = 1)
es_fit <- as.numeric(es_model$fitted)
es_next <- as.numeric(es_model$mean)
es_acc <- calc_accuracy(value, es_fit, "Ust. Duzeltme (SES)", es_next)
plot_forecast(dates, value, es_fit, next_date, es_next, "SES", "exponential_smoothing_plot.png")

# 5. Trend Düzeltmeli (Holt)
holt_model <- holt(ts_data, h = 1)
holt_fit <- as.numeric(holt_model$fitted)
holt_next <- as.numeric(holt_model$mean)
holt_acc <- calc_accuracy(value, holt_fit, "Trend Duzeltmeli (Holt)", holt_next)
plot_forecast(dates, value, holt_fit, next_date, holt_next, "Holt", "trend_adjusted_smoothing_plot.png")

# 6. Doğrusal Trend
trend_lm <- lm(value ~ t_idx)
trend_fit <- as.numeric(fitted(trend_lm))
trend_next <- predict(trend_lm, newdata = data.frame(t_idx = n + 1))
trend_acc <- calc_accuracy(value, trend_fit, "Dogrusal Trend", trend_next)
plot_forecast(dates, value, trend_fit, next_date, trend_next, "Dogrusal Trend", "trend_projection_plot.png")

# 7. Mevsimsel İndeksler
decomp_mult <- decompose(ts_data, type = "multiplicative")
si_trend <- as.numeric(decomp_mult$trend)
si_ratios <- value / si_trend
SI_raw <- tapply(si_ratios, m_factor, mean, na.rm = TRUE)
SI <- SI_raw * (12 / sum(SI_raw))
deseas <- value / SI[m_factor]
si_lm <- lm(deseas ~ t_idx)
si_fit <- as.numeric(predict(si_lm)) * SI[m_factor]
si_next <- predict(si_lm, newdata = data.frame(t_idx = n + 1)) * SI[next_cycle]
si_acc <- calc_accuracy(value, si_fit, "Mevsimsel Indeksler", si_next)
plot_forecast(dates, value, si_fit, next_date, si_next, "Mevs. Indeksler", "seasonal_indices_plot.png")

# 8. Toplamsal Ayrıştırma
decomp_add <- decompose(ts_data, type = "additive")
add_trend <- as.numeric(decomp_add$trend)
valid_add <- !is.na(add_trend)
add_lm <- lm(add_trend[valid_add] ~ t_idx[valid_add])
add_tfc <- predict(add_lm, newdata = data.frame(t_idx = t_idx))
add_fit <- add_tfc + as.numeric(decomp_add$seasonal)
add_next_t <- predict(add_lm, newdata = data.frame(t_idx = n + 1))
add_next <- as.numeric(add_next_t) + as.numeric(decomp_add$seasonal)[next_cycle]
add_acc <- calc_accuracy(value, add_fit, "Toplamsal Ayristirma", add_next)
plot_forecast(dates, value, add_fit, next_date, add_next, "Toplamsal", "additive_decomposition_plot.png")

# 9. Çarpımsal Ayrıştırma
mult_trend <- as.numeric(decomp_mult$trend)
valid_mult <- !is.na(mult_trend)
mult_lm <- lm(mult_trend[valid_mult] ~ t_idx[valid_mult])
mult_tfc <- predict(mult_lm, newdata = data.frame(t_idx = t_idx))
mult_fit <- mult_tfc * as.numeric(decomp_mult$seasonal)
mult_next_t <- predict(mult_lm, newdata = data.frame(t_idx = n + 1))
mult_next <- as.numeric(mult_next_t) * as.numeric(decomp_mult$seasonal)[next_cycle]
mult_acc <- calc_accuracy(value, mult_fit, "Carpimsal Ayristirma", mult_next)
plot_forecast(dates, value, mult_fit, next_date, mult_next, "Carpimsal", "multiplicative_decomposition_plot.png")

# 10. Regresyon (Mevsimsel Kukla Değişkenli)
month_f <- factor(m_factor, levels = 1:12)
reg_lm <- lm(value ~ t_idx + month_f)
reg_fit <- as.numeric(fitted(reg_lm))
reg_next <- predict(reg_lm, newdata = data.frame(t_idx = n + 1, month_f = factor(next_cycle, levels = 1:12)))
reg_acc <- calc_accuracy(value, reg_fit, "Regresyon (Mevsimsel)", reg_next)
plot_forecast(dates, value, reg_fit, next_date, reg_next, "Reg+Mevsim", "regression_seasonal_dummy_plot.png")