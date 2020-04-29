require(fitdistrplus)
require(reliaR)

  x <- c(5500, 4380 ,2370, 3220, 8050, 4560, 2100, 6840, 5640, 3500, 1940,
          7060, 7500, 5370, 13100, 4920, 6500, 4790, 6050, 4560, 3210, 6450,
          5870, 2900, 5490, 3490, 9030, 3100, 4600, 3410, 3690, 6420, 10300,7240, 9130)
  
  dgumbel <- function(x, a, b) 1/b*exp((a-x)/b)*exp(-exp((a-x)/b))
  pgumbel <- function(q, a, b) exp(-exp((a-q)/b))
  qgumbel <- function(p, a, b) a-b*log(-log(p))
  g <- fitdist(x,'gumbel',method = 'mle', start = list(a = 4000, b = 3000))
  summary(g)$estimate
  mu = summary(g)$estimate[1]
  sigma = summary(g)$estimate[2]
  

  qq.gumbel(x,mu,sigma)

  
  
  
  arrivals <- read.table("data.txt")
  arrivals <- unname(unlist(arrivals))
  hist(arrivals, breaks=30, probability = T)
  
  gammaparameters <- fitdist(arrivals, "gamma", method = "mme")
  GammaShapeMME <- summary(gammaparameters)$estimate[1]
  GammaRateMME <- summary(gammaparameters)$estimate[2]
  MME <- c(GammaShapeMME, GammaRateMME)
  print(MME)
  
  
  gammaparameters <- fitdist(arrivals, "gamma", method = "mle")
  MLE[1] <- summary(gammaparameters)$estimate[1]
  MLE[2] <- summary(gammaparameters)$estimate[2]
  MLE <- c(MLE[1], MLE[2])
  print(MLE)
  
  
  hist(arrivals, breaks=30,probability = T, ylim = c(0,0.012),
       main="Data and Gamma estimations")
  x <- seq(0,600,by=0.1)
  curve(dgamma(x, shape=MME[1], rate=MME[2]),
        add=TRUE, col='blue')
  curve(dgamma(x, shape=MLE[1], rate=MLE[2]),
        add=TRUE, col='red')
  
  require(matrixStats)
  require(RBesT)
  require(resample)
  MOMsample <- matrix(rgamma(length(arrivals)*1000, shape=MME[1],
                             rate = MME[2]), nrow=length(arrivals), ncol=1000)
  MOMshapes <- colMeans(MOMsample)^2/colVars(MOMsample)
  MOMrates <- colMeans(MOMsample)/colVars(MOMsample)
  
  MLEsample <- matrix(rgamma(length(arrivals)*1000, 
                             shape=MLE[1], rate = MLE[2]), nrow=length(arrivals),
                      ncol=1000)
  sampleMLEparameters <- numeric(1000)
  for (i in 1:1000)
    sampleMLEparameters[i] <- fitdist(MLEsample[i,], "gamma", method = "mle")
  MLEshapes <- numeric(1000)
  MLErates <- numeric(1000)
  for (i in 1:1000){
    MLEshapes[i] <- sampleMLEparameters[[i]][1]
    MLErates[i] <- sampleMLEparameters[[i]][2]}
  head(MLEshapes)
  head(MLErates)
  
    # shape MOM
  MOM950s <- MME[1] - (sort(MOMshapes, decreasing = T)[950] - MME[1])
  # rate MOM
  MOM950r <- MME[2] - (sort(MOMrates, decreasing = T)[950] - MME[2])
  # shape MLE
  MLE950s <- MLE[1] - (sort(MLEshapes,decreasing = T)[950] - MLE[1])
  # rate MLE
  MLE950r <- MLE[2] - (sort(MLErates, decreasing = T)[950] - MLE[2])
c(MOM950s,MLE950s)
c(MOM950r,MLE950r)