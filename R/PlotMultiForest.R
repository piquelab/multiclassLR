##' @title PlotMultiForest
##'

PlotMultiForest <-
function(ids, parms, SE){  
  ## requires a data.frame with ids, parameters, and SE
  system('mkdir -p multinomial')
  plot_dat <- data.frame(id=ids, mid=parms, lb=parms-1.96*se, ub=parms+1.96*se, stringsAsFactors=FALSE)
  require('ggplot2')
  pd <- position_dodge(width=0.5, height=NULL)
  p <- ggplot(plot_dat, aes(x=id, y=mid))
  pdf('./multinomial/multinomial_parms_sig.pdf')
  out_plot <- p + geom_abline(intercept=0, slope=0, colour='red',linetype=4) +
                layer(geom="point", position=pd, geom_params=list(size=3.2)) +
                geom_errorbar(aes(ymin=lb , ymax=ub), width=0.5, size=1, position=pd) +
                theme_bw() +
                coord_flip() +
                #scale_color_manual(values=c("black")) +
                theme(legend.position="bottom",
                      legend.direction="horizontal",
                      legend.key=element_blank(),
                      plot.title=element_text(angle=0,size=16, face="bold"),
                      axis.title.x=element_text(size=14, face="bold"),
                      axis.text.x=element_text(size=12),
                      axis.text.y=element_text(size=12)) +
                labs(title="Multinomial Estimates", y=expression(hat(beta)), x="")
  print(out_plot)
  dev.off()
}
