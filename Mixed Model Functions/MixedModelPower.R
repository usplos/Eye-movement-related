MixedModelPower = function(Model, FixEffect = NULL, Methods = 't', Nsim = 100,
                         Along = NULL, maxNumber = NULL, Breaks = NULL){
  library(simr)
  PowerList = list()
  PowerRaw = powerSim(fit = Model, test = fixed(FixEffect,Methods),
                            nsim = Nsim);PowerList[[1]] = PowerRaw
  if(!is.null(Along) & !is.null(maxNumber)){
    ModelExt = extend(Model, along = Along, n = maxNumber)
    if(is.null(Breaks)){
      PCurve = powerCurve(fit = ModelExt, fixed(FixEffect, Methods), 
                          along = Along, nsim = Nsim)
      PowerList[[2]] = PCurve
      DF = summary(PCurve)
      PowerPlot = ggplot(DF, aes(x = nlevels, y = mean))+
        geom_point(color = gray(0))+
        geom_errorbar(aes(ymax = upper, ymin = lower, x = nlevels), color = gray(0.6),width = 1)+
        geom_line(color = gray(0.3))+
        geom_hline(yintercept = 0.8, linetype = 3)+
        theme_classic()+
        theme(panel.grid = element_blank())+
        labs(x = paste0('Number of ', PCurve$along), y = 'Power value')+
        scale_x_continuous(breaks = seq(min(DF$nlevels)%/%10*10, (max(DF$nlevels)%/%10+1)*10, 5))
      PowerList[[3]] = PowerPlot
      print(PowerPlot)
    }else{
      PCurve = powerCurve(fit = ModelExt, fixed(FixEffect, Methods), 
                          along = Along, nsim = Nsim,breaks = Breaks)
      PowerList[[2]] = PCurve
      DF = summary(PCurve)
      PowerPlot = ggplot(DF, aes(x = nlevels, y = mean))+
        geom_point(color = gray(0))+
        geom_errorbar(aes(ymax = upper, ymin = lower, x = nlevels), color = gray(0.6),width = 1)+
        geom_line(color = gray(0.3))+
        geom_hline(yintercept = 0.8, linetype = 3)+
        theme_classic()+
        theme(panel.grid = element_blank())+
        labs(x = paste0('Number of ', PCurve$along), y = 'Power value')+
        scale_x_continuous(breaks = seq(min(DF$nlevels)%/%10*10, (max(DF$nlevels)%/%10+1)*10, 5))
      PowerList[[3]] = PowerPlot
      print(PowerPlot)
    }
  }
  return(PowerList)
}
