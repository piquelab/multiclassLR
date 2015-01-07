##' @title PlotMultiForest
##'

PlotMultiForest <- function(plot_dat, title, dir){
  if(sum(colnames(plot_dat) == c("feature", "config", "parms", "se")) != 4){
    stop("plot data must be formatted as: feature, config, parms, se")
  }
  if(require('ggplot2') != TRUE){
    stop("unable to load ggplot2")
  }
  
  system(paste0('mkdir -p ', dir))

  plot_dat <- data.frame(plot_dat, lb=plot_dat$parms-1.96*plot_dat$se, ub=plot_dat$parms+1.96*plot_dat$se, stringsAsFactors=FALSE)
  row.names(plot_dat) <- NULL
  plot_dat <- plot_dat[which(plot_dat$se<10), ]
  
  p <- ggplot(plot_dat, aes(x=feature, y=parms))
  pdf(paste0(dir, '/', title))
  out_plot <- p + geom_abline(intercept=0, slope=0, colour='red',linetype=4) +
                layer(geom="point", geom_params=list(size=3.2)) +
                geom_errorbar(aes(ymin=lb , ymax=ub), width=0.5, size=1) +
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
                labs(title="Multinomial Estimates", y=expression(hat(beta)), x="") +
                facet_grid(. ~ config)
  print(out_plot)
  dev.off()
}
