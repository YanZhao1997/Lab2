#' Yanmms
#'
#' @param output
#'the output you want to get
#' @return
#'the output value for each DRGcode
#' @export
#'None
#'#' @examples
#'NOne
Yanmms<-function(output)
{
data<-read.csv("DRG_data.csv")
data1<-data%>%select(DRG.Definition,Provider.Id,Provider.State, Average.Medicare.Payments)%>%spread(DRG.Definition,Average.Medicare.Payments)
outputmean<-vector(mode = "double",length = 100)
outputsd<-vector(mode = "double",length = 100)
outputmedian<-vector(mode = "double",length = 100)
for (i in 1:100){
  outputmean[i] <- mean(data1[,i+2],na.rm=TRUE)
  outputmedian[i] <-median(data1[,i+2],na.rm=TRUE)
  outputsd[i] <-sd(data1[,i+2],na.rm=TRUE)
}
code <- colnames(data1)[-1][-1]
data2 <- data.frame(code = code, mean = outputmean,median=outputmedian, sd = outputsd)
data2%>%select(code,output)
}
Yanmms("mean")
