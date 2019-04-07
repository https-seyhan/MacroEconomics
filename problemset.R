require(xlsx)
library(gdata)


rm(list=ls())

setwd("/media/saul/UUI/Macroeconomics Analysis 1/Assignment")


#pwtdata <-read.csv("pwt81.csv", sep=",", header=TRUE)
#pwtdata <-read.csv("Germany.csv", sep=",", header=TRUE)
#pwtdata <-read.csv("Canada.csv", sep=",", header=TRUE)
pwtdata <-read.csv("Norway.csv", sep=",", header=TRUE)
summary(pwtdata)

#Select Country

#pwtdata <- pwtdata[which(pwtdata$countrycode=="CAN"),]


pwtdata$RealGDP<- pwtdata$rgdpo #pwtdata$rgdpo 
pwtdata$POP  <- pwtdata$pop 
pwtdata$RealGDPPOP <- pwtdata$RealGDP/pwtdata$pop
pwtdata$RealGDPperWorker   <- pwtdata$RealGDP/pwtdata$emp 
pwtdata$LPOP <- pwtdata$emp/pwtdata$pop 
pwtdata$KY   <- pwtdata$ck/pwtdata$RealGDP 
pwtdata$CapitalperWorker   <- pwtdata$ck/pwtdata$emp 
pwtdata$hours<- pwtdata$avh 
pwtdata$shareofWorkerCompensation <- pwtdata$labsh


pwtdata$TFP2  <- pwtdata$rgdpo/(pwtdata$emp^(2/3)*pwtdata$ck^(1/3)) 

pwtdata$TFP  <- pwtdata$RealGDPperWorker/pwtdata$CapitalperWorker^(1/3) 

pwtdata$TFP3  <- pwtdata$rgdpe/(pwtdata$emp^(1 - pwtdata$labsh)*pwtdata$ck^(pwtdata$labsh)) 

#Growth RATES


#growth Rate of Real GDP  per Worker
pwtdata$GrowthRateOfRelGDPperWorker <- c(NA, head(tail(log(pwtdata$RealGDPperWorker) - log(pwtdata$RealGDPperWorker), -1) , -1))*100

#growth Rate of Capital per Worker
pwtdata$GrowthRateOfCapitalperWorker <- c(NA, head(log(pwtdata$CapitalperWorker), -1) - tail(log(pwtdata$CapitalperWorker), -1))*100



#growth Rate of Solow Residuals
pwtdata$GrowthRate <- c(NA, head(log(pwtdata$TFP3), -1) - tail(log(pwtdata$TFP3), -1))




# variables to keep for spreadsheet 
variables <- c("country", "countrycode", "year", "labsh", "RealGDPperWorker", "CapitalperWorker",
               "TFP","TFP2", "TFP3")

relatedVars  <- pwtdata[variables]
summary(relatedVars)

write.table(relatedVars , file="Norway_withCalcs2.csv",
            quote = FALSE,
            sep=",",
            row.names = FALSE,
            col.names=TRUE )

boxplot(pwtdata$GrowthRate)

save(relatedVars, file="pwt_global81.RData")




#Plot Real GDP per worker overtime
rm(list=ls())
load("pwt_global81.RData")

# plots 
plot_YL <- function(data, country) {
  # select country 
  data <- data[data$countrycode == country,]
  head(data) 
  # plot
  plot(data$year, data$RealGDPperWorker/1000,  # convert to thousands  
       , type="l",cex=1.5, lwd=2, col="red",
       main="", xlab="", ylab="GDP per worker (000s of 2005 USD)", 
       #     ylim=c(10, 90),
       mar=c(2,4,2,2),   # better than default in most cases 
       mgp=c(2.5,1,0)  
  )
  mtext(country, side=3, adj=0, line=1.0, cex=1.25)
  
  dev.print(device=pdf, file=paste(country,"_YL.pdf",sep=""),width=8,height=6)
}
# test
plot_YL(relatedVars,c("USA"))
plot_YL(relatedVars,c("TUR"))
plot_YL(relatedVars,c("CAN"))

plot_CapitalperWorker <- function(data, country) {
  # select country 
  data <- data[data$countrycode == country,]
  head(data) 
  # plot
  plot(data$year, data$CapitalperWorker/1000,  # convert to thousands  
       , type="l",cex=1.5, lwd=2, col="red",
       main="", xlab="", ylab="Capital per worker (000s of 2005 USD)", 
       #     ylim=c(10, 90),
       mar=c(2,4,2,2),   # better than default in most cases 
       mgp=c(2.5,1,0)  
  )
  mtext(country, side=3, adj=0, line=1.0, cex=1.25)
  
  dev.print(device=pdf, file=paste(country,"_CapitalperWorker.pdf",sep=""),width=8,height=6)
}
# test
plot_CapitalperWorker(relatedVars,c("USA"))
plot_CapitalperWorker(relatedVars,c("TUR"))
plot_CapitalperWorker(relatedVars,c("SYR"))



plot_TFP <- function(data, country) {
  # select country 
  data <- data[data$countrycode == country,]
  head(data) 
  # plot
  plot(data$year, data$TFP3/1000,  # convert to thousands  
       , type="l",cex=1.5, lwd=2, col="red",
       main="", xlab="", ylab="TFP (000s of 2005 USD)", 
       #     ylim=c(10, 90),
       mar=c(2,4,2,2),   # better than default in most cases 
       mgp=c(2.5,1,0)  
  )
  mtext(country, side=3, adj=0, line=1.0, cex=1.25)
  
  dev.print(device=pdf, file=paste(country,"_TFP.pdf",sep=""),width=8,height=6)
}
# test
plot_TFP(relatedVars,c("USA"))
plot_TFP(relatedVars,c("TUR"))
plot_TFP(relatedVars,c("SYR"))


#Plot Groth Rates

plot_SlowResiduals <- function(data, country) {
  # select country 
  data <- data[data$countrycode == country,]
  head(data) 
  # plot
  plot(data$year, data$GrowthRate,  # convert to thousands  
       , type="l",cex=1.5, lwd=2, col="red",
       main="", xlab="", ylab="Solow Residuals", 
       #     ylim=c(10, 90),
       mar=c(2,4,2,2),   # better than default in most cases 
       mgp=c(2.5,1,0)  
  )
  mtext(country, side=3, adj=0, line=1.0, cex=1.25)
  
  dev.print(device=pdf, file=paste(country,"_SolowResidual.pdf",sep=""),width=8,height=6)
}
# test
plot_SlowResiduals(relatedVars,c("USA"))
plot_SlowResiduals(relatedVars,c("TUR"))
plot_SlowResiduals(relatedVars,c("SYR"))
plot_SlowResiduals(relatedVars,c("CAN"))






# growth accounting 
growth_acc <- function(data, code, years) {
  # subset data
  data <- subset(data, countrycode %in% code & year %in% years)
  head(data)
  
  # compute growth rates 
  gYL <- (log(data$RealGDPperWorker[2])-log(data$RealGDPperWorker[1]))/(data$year[2]-data$year[1])
  gKL <- (log(data$CapitalperWorker[2])-log(data$CapitalperWorker[1]))/(data$year[2]-data$year[1])
  gA  <- (log(data$TFP3[2])-log(data$TFP3[1]))/(data$year[2]-data$year[1])
  # collect in matrix and return 
  date1 <- c(data$YL[1], data$KL[1], data$TFP[1])
  date2 <- c(data$YL[2], data$KL[2], data$TFP[2])
  growth <- c(gYL, gKL, gA)
  contri <- c(gYL, gKL/3, gA)
  #  print(growth)
  #  print(contri)
  return(rbind(date1, date2, growth, contri, gA))
}
# test
growth_acc(relatedVars, c("USA"), c("1950", "2011"))
growth_acc(relatedVars, c("CAN"), c("1965", "1990"))



relatedVars$newcol <- apply(relatedVars,1,insertSolowResudials)

summary(relatedVars)

#Get Canada Data
data <- subset(relatedVars, countrycode %in% 'CAN' & year %in% c("1965", "1990"))

