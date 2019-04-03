BarplotUniV = function(df, IndeName, DeName,Fill = 'grey', Dot = F, alpha = 0.2, Color = 'black'){
  if(!require(tidyverse)){install.packages('tidyverse')}
  library(tidyverse)
  dfTidy = df %>% group_by(eval(parse(text = IndeName))) %>% summarise(M = mean(eval(parse(text = DeName))),
                                                                       SE = sd(eval(parse(text = DeName)))/sqrt(length(eval(parse(text = DeName)))))
  names(dfTidy)[[1]] = IndeName
  p = ggplot(data = dfTidy,aes(x = eval(parse(text = IndeName)), y = M))+
    geom_bar(stat = 'identity', width = 0.25, fill = Fill)+
    geom_errorbar(aes(ymin = M-SE, ymax = M+SE), width = 0.05)+
    labs(x = IndeName, y = DeName)
  if (isTRUE(Dot)) {
    Level = levels(eval(parse(text = paste('df$', IndeName, sep = ''))))
    p = p + geom_point(data = df ,aes(x = eval(parse(text = IndeName)), y = eval(parse(text = DeName))),
                       alpha = 0.2,
                       color = Color)
  }
  print(p);return(p)
}

BarplotBiV = function(df, IndeName, DeName,Fill = c('red','blue'), Dot = F, Alpha = 0.2, Color = 'black', Dodge = 0.5){
  if(!require(tidyverse)){install.packages('tidyverse')}
  library(tidyverse)
  IndeName1 = IndeName[[1]];IndeName2 = IndeName[[2]]
  dfTidy = df %>% group_by(eval(parse(text = IndeName1)),
                           eval(parse(text = IndeName2))) %>% summarise(M = mean(eval(parse(text = DeName))),
                                                                       SE = sd(eval(parse(text = DeName)))/sqrt(length(eval(parse(text = DeName)))))
  names(dfTidy)[1:2] = IndeName
  p = ggplot(data = dfTidy,
             aes(x = eval(parse(text = IndeName1)),
                 y = M,
                 fill = eval(parse(text = IndeName2))))+
    geom_bar(position = position_dodge(Dodge),
             stat = 'identity',
             width = 0.5)+
    geom_errorbar(aes(ymin = M-SE,
                      ymax = M+SE),
                  width = 0.05,
                  position = position_dodge(Dodge))+
    labs(y = DeName, x = IndeName1, fill = IndeName2)+
    scale_fill_manual(values = Fill)
  
  if(isTRUE(Dot)){
    p = p + geom_point(data = df %>% 
                         filter(eval(parse(text = IndeName2)) %in% levels(eval(parse(text = IndeName2)))[[1]]) %>%
                         mutate(xp = ifelse(eval(parse(text = IndeName1)) %in% levels(eval(parse(text = IndeName1)))[[1]], 0.875, 1.875)), 
                       aes(x = xp, y = eval(parse(text = DeName))),
                       alpha = Alpha,
                       color = Color,
                       show.legend = F)+
      geom_point(data = df %>% 
                   filter(eval(parse(text = IndeName2)) %in% levels(eval(parse(text = IndeName2)))[[2]]) %>%
                   mutate(xp = ifelse(eval(parse(text = IndeName1)) %in% levels(eval(parse(text = IndeName1)))[[1]], 1.125, 2.125)), 
                 aes(x = xp, y = eval(parse(text = DeName))),
                 alpha = Alpha,
                 color = Color,
                 show.legend = F)
  }
  
  print(p);return(p)
}

InteractionplotUniV = function(df, IndeName, DeName,Dot = F, alpha = 0.2, Color = c('red','blue'),Dodge = 0.5, PColor = 'black'){
  if(!require(tidyverse)){install.packages('tidyverse')}
  library(tidyverse)
  IndeName1 = IndeName[[1]];IndeName2 = IndeName[[2]]
  dfTidy = df %>% group_by(eval(parse(text = IndeName1)),
                           eval(parse(text = IndeName2))) %>% summarise(M = mean(eval(parse(text = DeName))),
                                                                        SE = sd(eval(parse(text = DeName)))/sqrt(length(eval(parse(text = DeName)))))
  names(dfTidy)[1:2] = IndeName
  p = ggplot(data = dfTidy,
             aes(x = eval(parse(text = IndeName1)),
                 y = M,
                 color = eval(parse(text = IndeName2))))+
    geom_point(position = position_dodge(Dodge))+
    geom_errorbar(aes(ymin = M-SE,
                      ymax = M+SE),
                  width = 0.05,
                  position = position_dodge(Dodge))+
    geom_line(aes(group = eval(parse(text = IndeName2))),
              position = position_dodge(Dodge))+
    labs(x = IndeName1, y = DeName, color = IndeName2)+
    scale_colour_manual(values = Color)
  
  if(isTRUE(Dot)){
    p = p + geom_point(data = df %>% 
                         filter(eval(parse(text = IndeName2)) %in% levels(eval(parse(text = IndeName2)))[[1]]) %>%
                         mutate(xp = ifelse(eval(parse(text = IndeName1)) %in% levels(eval(parse(text = IndeName1)))[[1]], 0.875, 1.875)), 
                       aes(x = xp, y = eval(parse(text = DeName))),
                       alpha = Alpha,
                       color = PColor,
                       show.legend = F)+
      geom_point(data = df %>% 
                   filter(eval(parse(text = IndeName2)) %in% levels(eval(parse(text = IndeName2)))[[2]]) %>%
                   mutate(xp = ifelse(eval(parse(text = IndeName1)) %in% levels(eval(parse(text = IndeName1)))[[1]], 1.125, 2.125)), 
                 aes(x = xp, y = eval(parse(text = DeName))),
                 alpha = Alpha,
                 color = PColor,
                 show.legend = F)
  }
  
  print(p);return(p)
}
