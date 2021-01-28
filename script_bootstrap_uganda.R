###############################################
# Bootstrap                                   #
# Contacts per person per day                 #         
# R 4.0.3                                     #
###############################################

load(file = "auxd.rda") 

boot.mean = function(x,B,binwidth=NULL) {
  n = length(x)
  boot.samples = matrix( sample(x,size=n*B,replace=TRUE), B, n)
  boot.statistics = apply(boot.samples,1,mean)
  se = sd(boot.statistics)
  require(ggplot2)
  if ( is.null(binwidth) )
    binwidth = diff(range(boot.statistics))/30
  p = ggplot(data.frame(x=boot.statistics),aes(x=x)) +
    geom_histogram(aes(y=..density..),binwidth=binwidth) + geom_density(color="red") + theme_bw()
  #plot(p)
  interval = mean(x) + c(-1,1)*2*se
  #print( interval )
  return( list(boot.statistics = boot.statistics, interval=interval, se=se, plot=p) )
}

# Bootstrap for each individual from contacts aggregated by age class

recebe <- data.frame(age_class = c(), all_contacts= c(), mean_contacts= c(), Bootstrap1 = c(), Bootstrap2 = c())

for(i in unique(auxd$unique) ){
  aux <- data.frame(age_class = i, 
                    all_contacts = sum(auxd[which(auxd$unique == i),"value"]) ,
                    mean_contacts = mean(auxd[which(auxd$unique == i),"value"]),
                    Bootstrap1 = boot.mean(auxd[which(auxd$unique == i),"value"], B = 1000)$interval[1],
                    Bootstrap2 = boot.mean(auxd[which(auxd$unique == i),"value"], B = 1000)$interval[2])
  
  recebe <- rbind(recebe, aux )
}

recebe

library(xlsx)
#write.xlsx(recebe, "touch_contact_bootstrap.xlsx")