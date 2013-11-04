\name{fwi.fbp-package}
\alias{fwi.fbp-package}
\alias{fwi.fbp}
\docType{package}
\title{
Fire Weather Index and Fire Behavior Prediction Systems
}
\description{
The fwi.fbp package allows R users to calculate the outputs of the two main components of the Canadian Forest Fire Danger Rating System (CFFDRS; \url{http://cwfis.cfs.nrcan.gc.ca/en_CA/background/summary/fdr}): the Fire Weather Index (FWI) System (\url{http://cwfis.cfs.nrcan.gc.ca/en_CA/background/summary/fwi}) and the Fire Behaviour Prediction (FBP) System (\url{http://cwfis.cfs.nrcan.gc.ca/en_CA/background/summary/fbp}). These systems are widely used internationally to assess fire danger (FWI System) and quantify fire behavior (FBP System). 

The FWI System (Van Wagner, 1987) is based on the moisture content and the effect of wind of three classes of forest fuels on fire behavior. It consists of six components: three fuel moisture codes (Fire Fuel Moisture Code, Duff Moisture Code, Drought Code), and three fire behavior indexes representing rate of spread (Initial Spread Index), fuel consumption (Buildup Index), and fire intensity (Fire Weather Index). The FWI System outputs are determined from daily noon weather observations: temperature, relative humidity, wind speed, and 24-hour rainfall. 

The FBP System (Forestry Canada Fire Danger Group. 1992; Hirsch 1996) provides a set of primary and secondary measures of fire behavior. The primary outputs consist of estimates of fire spread rate, fuel consumption, fire intensity, and fire description (i.e., surface, intermittent, or crown fire). The secondary outputs, which are not used nearly as often, gives estimates of fire area, perimeter, perimeter growth rate, and flank and back fire behavior based on a simple elliptical fire growth model. Unlike the FWI System, which is weather based, the FBP System also requires information on vegetation (hereafter, fuel types) and slope (if any) to calculate its outputs. Sixteen fuel types are included in the FBP System, covering mainly major vegetations types in Canada. 

}
\details{
\tabular{ll}{
Package: \tab fwi.fbp\cr
Type: \tab Package\cr
Version: \tab 1.3\cr
Date: \tab 2013-11-04\cr
License: \tab GPL-2\cr
}
This package includes three functions. Two functions, fwi and fwiBAT, are used for FWI System calculation, whereas one function, fbp, is used for FBP System calculation. These functions are not fully independent: their inputs overlap greatly and the user will have to provide FWI System outputs to calculate FBP System outputs. The fwi function is a low level function that is used to calculate the outputs of the FWI System for one day based on noon local standard time (LST) weather observations of temperature, relative humidity, wind speed, and 24-hour rainfall, as well as the previous day's weather conditions. The fwiBAT function is similar to fwi, but is at a higher level in that it allows the user to directly calculate FWI System outputs from a list of consecutive daily weather observations. The fbp function calculates the outputs of the FBP System based on given set of fire weather conditions (weather observations and their associated FWI System components), fuel type, and slope (optional).
}
\author{
Xianli Wang, Alan Cantin, Marc-Andre Parisien, Mike Wotton, Kerry Anderson, and Mike Flannigan

Maintainer: Alan Cantin \email{Alan.Cantin@RNCan-NRCan.gc.ca}
}
\references{
1. Van Wagner, C.E. and T.L. Pickett. 1985. Equations and FORTRAN program for the Canadian Forest Fire Weather Index System. Can. For. Serv., Ottawa, Ont. For. Tech. Rep. 33. 18 p.

2. Van Wagner, C.E. 1987. Development and structure of the Canadian forest fire weather index system. Forest Technology Report 35. (Canadian Forestry Service: Ottawa).

3. Lawson, B.D. and O.B. Armitage. 2008. Weather guide for the Canadian Forest Fire Danger Rating System. Nat. Resour. Can., Can. For. Serv., North. For. Cent., Edmonton, AB.

4. Hirsch K.G. 1996. Canadian Forest Fire Behavior Prediction (FBP) System: user's guide. Nat. Resour. Can., Can. For. Serv., Northwest Reg., North. For. Cent., Edmonton, Alberta. Spec. Rep. 7. 122p.

5. Forestry Canada Fire Danger Group. 1992. Development and structure of the Canadian Forest Fire Behavior Prediction System. Forestry Canada, Ottawa, Ontario Information Report ST-X-3. 63 p. \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/10068.pdf}

6. Wotton, B.M., Alexander, M.E., Taylor, S.W. 2009. Updates and revisions to the 1992 Canadian forest fire behavior prediction system. Nat. Resour. Can., Can. For. Serv., Great Lakes For. Cent., Sault Ste. Marie, Ontario, Canada. Information Report GLC-X-10, 45p. \url{http://publications.gc.ca/collections/collection_2010/nrcan/Fo123-2-10-2009-eng.pdf}

7. Tymstra, C., Bryce, R.W., Wotton, B.M., Armitage, O.B. 2009. Development and structure of Prometheus: the Canadian wildland fire growth simulation Model. Nat. Resour. Can., Can. For. Serv., North. For. Cent., Edmonton, AB. Inf. Rep. NOR-X-417.
}

\keyword{ package }
\seealso{
   \code{\link{fwi}},
   \code{\link{fwiBAT}},
   \code{\link{fbp}}
}
\examples{
library(fwi.fbp)

######## fwi function examples ########
# The test data is a standard test
# dataset (Van Wagner and Pickett 1985). 

data("test_fwi")
head(test_fwi)
# Using the default initial values
fwi.out1<-fwi(test_fwi)     		

# Using a different set of initial values
fwi.out2<-fwi(test_fwi,init=c(80,10,16,50)) 

# fwi system components calculated based on previous day's 
# fwi outputs 
fwi.out3<-fwi(test_fwi,fwi.out1)                 

# Using a suite of initials, assuming variables from fwi.out1
# are the initial values for different records
init_suite<-fwi.out1[,c("ffmc","dmc","dc","lat")]
fwi.out4<-fwi(test_fwi,init=init_suite)

# Using only the required input variables:
fwi.out5<-fwi(test_fwi[,7:10])  

# Daylength adjustment:
# Change latitude values where the monthly daylength adjustments
# are different from the standard ones
test_fwi$lat<-22
# With daylength adjustment
fwi(test_fwi)[1:3,]
# Without daylength adjustment
fwi(test_fwi,lat.adjust=FALSE)[1:3,]



######## fwiBAT function examples ########
# The test data is a standard test
# dataset for FWI system(Van Wagner and Pickett 1985). 

data("test_fwi")
head(test_fwi)

# using the default initial values
fwi.out<-fwiBAT(test_fwi)     		

# using a different set of initials 
fwi.out<-fwiBAT(test_fwi,init=c(80,10,16,50)) 

# using only the required input variables:
fwi.out<-fwiBAT(test_fwi[,7:10])



######## fbp function examples ########
# The dataset is the standard test data
# provided by Wotton et al (2009).

data("test_fbp")
head(test_fbp)
#Primary output (default)
fbp(test_fbp)
#or
fbp(test_fbp,output="Primary") 
#or 
fbp(test_fbp,"P")

#Secondary output          
fbp(test_fbp,"Secondary")
#or
fbp(test_fbp,"S")

#All output          
fbp(test_fbp,"All")
#or
fbp(test_fbp,"A")

#For a single record:
fbp(test_fbp[7,])    
#For a section of the records:
fbp(test_fbp[8:13,])	

#fbp function produces the default values if no data is fed to
#the function:
fbp()
}
