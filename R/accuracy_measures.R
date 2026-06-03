
calc_accuracy <- function(actual, fitted_vals, method_name, next_fc) {
  errors <- actual - fitted_vals
  valid  <- !is.na(errors) & !is.na(actual)
  if (sum(valid) == 0) return(data.frame(Method=method_name,Bias=NA,MAD=NA,MSE=NA,MAPE_pct=NA,RSFE=NA,Tracking_Signal=NA,Next_Period_Forecast=round(as.numeric(next_fc),0),stringsAsFactors=FALSE))
  e <- errors[valid]; a <- actual[valid]; n_e <- length(e)
  bias <- mean(e); mad <- mean(abs(e)); mse <- mean(e^2); mape <- mean(abs(e/a)*100)
  rsfe_vec <- cumsum(e); mad_run <- cumsum(abs(e))/seq_len(n_e); ts_vec <- rsfe_vec/mad_run
  data.frame(Method=method_name,Bias=round(bias,0),MAD=round(mad,0),MSE=round(mse,0),MAPE_pct=round(mape,2),RSFE=round(tail(rsfe_vec,1),0),Tracking_Signal=round(tail(ts_vec,1),2),Next_Period_Forecast=round(as.numeric(next_fc),0),stringsAsFactors=FALSE)
}

