#' Yanplot
#'
#' @param output
#'the output you want to get
#' @return
#'the boxplots of out put against the DRG code
#' @export
#'None
#'#' @examples
#'NOne
library(tidyverse)
Yanplot<-function(outcome=c("Average.Medicare.Payments","Average.Total.Payments"))
{
  data<-read.csv("DRG_data.csv")
  data1<-data%>%separate(DRG.Definition,c("DRG.Code","DRG.Definition"),sep=" - ")%>%
    select(DRG.Code,outcome)
  colnames(data1)[2]<-"Variable"
  data1%>%ggplot()+
    geom_boxplot(aes(x=DRG.Code,y=log(Variable)),fill="pink",outlier.shape=NA)+
                   theme(axis.text.x = element_text(size=5,angle=90))+
    labs(title = paste("Boxplot of",outcome,"by DRG code"),x="DRG code",y=paste("Logged",outcome))
}

Yanplot("Average.Medicare.Payments")
