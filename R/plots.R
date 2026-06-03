
my_theme <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.title=ggplot2::element_text(face="bold",size=12),
                   plot.subtitle=ggplot2::element_text(color="gray40",size=10),
                   legend.position="bottom", panel.grid.minor=ggplot2::element_blank())
}
plot_forecast <- function(dates, actual, fitted, next_date, next_fc, method_lbl, file_name, series_lbl="Linyit Toplam Sat<U+0131>labilir <U+00DC>retim (Ton) | T<U+00DC><U+0130>K") {
  df_main <- data.frame(Date=dates, Actual=actual, Fitted=fitted)
  df_fc   <- data.frame(Date=next_date, Fitted=as.numeric(next_fc))
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data=df_main, ggplot2::aes(x=Date,y=Actual,colour="Ger<U+00E7>ek De<U+011F>er"), linewidth=0.75) +
    ggplot2::geom_line(data=df_main[!is.na(df_main$Fitted),], ggplot2::aes(x=Date,y=Fitted,colour=method_lbl), linewidth=0.75, linetype="dashed", alpha=0.9) +
    ggplot2::geom_point(data=df_fc, ggplot2::aes(x=Date,y=Fitted,colour="Tahmin"), size=4, shape=17) +
    ggplot2::scale_colour_manual(values=c("Ger<U+00E7>ek De<U+011F>er"="steelblue","Tahmin"="red3",setNames("darkorange",method_lbl))) +
    ggplot2::labs(title=paste0(method_lbl," <U+2014> Ger<U+00E7>ek De<U+011F>er ve Model Tahmini"), subtitle=series_lbl, x=NULL, y="Ton", colour=NULL) +
    my_theme()
  ggplot2::ggsave(file.path("outputs","figures",file_name), p, width=10, height=5, dpi=150)
  invisible(p)
}

