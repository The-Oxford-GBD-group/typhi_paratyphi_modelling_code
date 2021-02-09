draws.df <- draws.df[!is.na(draws.df$draw1),]


draws.df <-  data.table(draws.df)

#get the mean of the draws
draws.df$mean_draws <- rowMeans(draws.df[,30:128])

#calcaulte the weighted mean of each draw

country <- draws.df[,. (estimate = weighted.mean(mean_draws, weight),
                        input = weighted.mean(rate, weight)),
                    by = c('nid', 'row_id', "super_region", "region", "country", 'year', 'fold')]

country$fold[country$fold != 0] <- 'OOS'
country$fold[country$fold == 0] <- 'IS'

country <- dcast(country, nid+row_id+super_region+region+country+year+input~fold, value.var = 'estimate') 

png(paste0(out_dir,  '/oos_correlation.png'),
    height = 12, width = 12, res = 350, unit = 'in')
print(
  ggplot(country)+
    geom_point(aes(x = input, y=OOS))+
    xlim(0, 1)+
    ylim(0, 1)+
    geom_abline(slope = 1, intercept = 0, colour = 'red')+
    theme_bw() +
    theme(strip.background = element_rect(fill = "white")) +
    labs(
      x = "Data Estimate",
      y = "Mean Prediction",
      size = "Weight",
      title = ("OOS Validation Plot for MBG"),
      subtitle = paste0("RMSE = ", round(rmse(country$input, country$OOS),2), ";    R2 = ", round(cor(country$input, country$OOS)^2,2)))
) 
dev.off()


png(paste0(out_dir,  '/IS_correlation.png'),
    height = 12, width = 12, res = 350, unit = 'in')
print(
  ggplot(country)+
    geom_point(aes(x = input, y=IS))+
    xlim(0, 1)+
    ylim(0, 1)+
    geom_abline(slope = 1, intercept = 0, colour = 'red')+
    theme_bw() +
    theme(strip.background = element_rect(fill = "white")) +
    labs(
      x = "Data Estimate",
      y = "Mean Prediction",
      size = "Weight",
      title = ("IS Validation Plot for MBG"),
      subtitle = paste0("RMSE = ", round(rmse(country$input, country$IS),2), ";    R2 = ", round(cor(country$input, country$IS)^2,2)))
) 
dev.off()


#plot by super region

metrics <- country[,.(RMSE = round(rmse(input, OOS),2),
                      Rsq = round(cor(input, OOS)^2,2)),
                  by = 'super_region']

write.csv(metrics, paste0(out_dir, '/oos_metrics_by_spr_reg.csv'), row.names = F)

#plot out the predicted vs estimated
png(paste0(out_dir, '/oos_correlation_by_spr_reg.png'),
    height = 12, width = 12, res = 350, unit = 'in')
print(
  ggplot(country)+
    geom_point(aes(x = input, y=OOS))+
    xlim(0, 1)+
    ylim(0, 1)+
    geom_abline(slope = 1, intercept = 0, colour = 'red')+
    theme_bw() +
    theme(strip.background = element_rect(fill = "white")) +
    labs(
      x = "Data Estimate",
      y = "Mean Prediction",
      size = "Weight")+
    facet_wrap(~super_region)
) 
dev.off()
