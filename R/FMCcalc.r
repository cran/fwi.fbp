  .FMCcalc<-function(LAT,LONG,ELV,DJ,D0){                                                                                               # if D0, date of min FMC, is not known then D0 = NULL.
     FMC  <- rep(-1,length(LAT))
     LATN <- rep(0,length(LAT))
     LATN <- ifelse(D0<=0,ifelse(ELV<=0,46+23.4*exp(-0.0360*(150-LONG)),43+33.7*exp(-0.0351*(150-LONG))),LATN)
     D0   <- ifelse(D0<=0,ifelse(ELV<=0,151*(LAT/LATN),142.1*(LAT/LATN)+0.0172*ELV),D0)
     ND   <- abs(DJ-D0)
     FMC  <- ifelse(ND<30,85+0.0189*ND^2,ifelse(ND>=30&ND<50,32.9+3.17*ND-0.0288*ND^2,120))
     FMC}
