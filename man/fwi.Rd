\name{fwi}
\alias{fwi}
\title{
Fire Weather Index function
}
\description{
\code{\link{fwi}} is used to calculate the outputs of the Fire Weather Index (FWI) System for one day based on noon local standard time (LST) weather observations of temperature, relative humidity, wind speed, and 24-hour rainfall, as well as the previous day's weather conditions. This function could be used for either one weather station or for multiple weather stations or a gridded surface. This is a lower-level function that allows for maximum flexibility in FWI System component calculations. This package also contains a similar high-level function, \code{\link{fwiBAT}},to calculate FWI System outputs for an entire fire season at one weather station. 
}
\usage{
fwi(input,yda.fwi=NULL,init=c(ffmc_yda=85,dmc_yda=6,dc_yda=15, lat=55),
    out="all",lat.adjust="TRUE")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
    A dataframe containing input variables of daily weather observations taken at noon LST. Variable names have to be the same as in the following list, but they are case insensitive. The order in which the input variables are entered is not important. 

    \tabular{lll}{
        \var{id}   \tab (optional) \tab Unique identifier of a weather station or spatial point (no restriction on\cr\tab\tab
                                        data type)\cr
        \var{lat}  \tab (recommended) \tab Latitude (decimal degree, default=55)\cr
        \var{long} \tab (optional) \tab Longitude (decimal degree)\cr
        \var{yr}   \tab (optional) \tab Year of observation\cr
        \var{mon}  \tab (recommended) \tab Month of the year (integer 1-12, default=7)\cr
        \var{day}  \tab (optional) \tab Day of the month (integer)\cr
        \var{temp} \tab (required) \tab Temperature (centigrade)\cr    
        \var{rh}   \tab (required) \tab Relative humidity (\%)\cr
        \var{ws}   \tab (required) \tab 10-m height wind speed (km/h)\cr
        \var{prec} \tab (required) \tab 24-hour rainfall (mm)\cr
    }
  }
  \item{yda.fwi}{
    The FWI values calculated for the previous day that will be used for the current day's calculation. This input should be a dataframe that contains fwi outputs from the previous day. When yda.fwi is fed to the function, the initial (i.e., "startup") values in the function would be ignored.
  }
  \item{init}{
    In some situations, such as the first day of the fire season, there are no previous-day values to calculate the current day's FWI System codes. In such a case, initial ("startup") values have to be provided. If neither initial values nor previous day's values are specified, the function will use default values (see below). The init argument can also accept a dataframe with the same number of rows as that of the input data if the initial values are to be applied to more than one point (e.g. a grid), station, or year. 

    \tabular{ll}{
        \var{ffmc_yda} \tab Fine Fuel Moisture Code (FFMC; unitless) of the previous day. Default value is 85.\cr
        \var{dmc_yda} \tab Duff Moisture Code (DMC; unitless) of the previous day. Default value is 6.\cr
        \var{dc_yda} \tab Drought Code (DC; unitless) of the previous day. Default value is 15.\cr
        \var{lat} \tab Latitude of the weather station (optional, default=55). Latitude values are used to make\cr\tab
                      day length adjustments in the function.\cr
    }
  }
  \item{out}{
     The function offers two output options, out="all" will produce an output include both the input and the FWI System outputs; out="fwi" will generate only the FWI system components.
  }
  \item{lat.adjust}{
    The function offers options for whether day length adjustments should be applied to the calculations.  The default value is "TRUE".
  }
}
\details{
The Canadian Forest Fire Weather Index (FWI) System is a major subsystem of the Canadian Forest Fire Danger Rating System, which also includes Canadian Forest Fire Behavior Prediction (FBP) System. The modern FWI System was first issued in 1970 and is the result of work by numerous researchers from across Canada. It evolved from field research which began in the 1930's and regional fire hazard and fire danger tables developed from that early research. 

The modern System (Van Wagner 1987) provides six output indices which represent fuel moisture and potential fire behavior in a standard pine forest fuel type. Inputs are a daily noon observation of fire weather, which consists of screen-level air temperature and relative humidity, 10 meter open wind speed and 24 accumulated precipitation. 

The first three outputs of the system (the Fire Fuel Moisture Code, the Duff Moisture Code, and the Drought Code) track moisture in different layers of the fuel making up the forest floor. Their calculation relies on the daily fire weather observation and also, importantly, the code value from the previous day as they are in essence bookkeeping systems tracking the amount of moisture (water) in to and out of the layer. It is therefore important that when calculating FWI System outputs over an entire fire season, an uninterrupted daily weather stream is provided; one day is the assumed time step in the models and thus missing data must be filled in.  

The next three outputs of the System are relative (unitless) indicators of aspects of fire behavior potential: spread rate (the Initial Spread Index), fuel consumption (the Build-up Index) and fire intensity per unit length of fire front (the Fire Weather Index). This final index, the fwi, is the component of the System used to establish the daily fire danger level for a region and communicated to the public. This final index can be transformed to the Daily Severity Rating (dsr) to provide a more reasonably-scaled estimate of fire control difficulty.  

Both the Duff Moisture Code (dmc) and Drought Code (dc) are influenced by day length (see Van Wagner, 1987). Day length adjustments for different ranges in latitude can be used (as described in Lawson and Armitage 2008 (\url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/29152.pdf})) and are included in this R function; latitude must be positive in the northern hemisphere and negative in the southern hemisphere.

At the start of a fire season, or simply the start of a daily weather stream, the FWI System calculation requires an estimate of yesterday's moisture conditions. The default initial (i.e., "start-up") fuel moisture code values (FFMC=85, DMC=6, DC=15) provide a reasonable set of conditions for post-snowmelt springtime conditions in eastern/central Canada, the Northern U.S., and Alaska; physically these spring start-up values represent about 3 days of drying from complete moisture saturation of the fuel layer. In areas or years with particularly dry winters (or parts of the world without significant snow cover) these start-up values for FFMC and DMC may still be appropriate as these two elements respond relatively quickly to changes in the weather. The DC component however, because of its very long response time, can take considerable time to adjust to unrealistic initial values and some effort to estimate over-winter value of the DC may be necessary. Users can look again to Lawson and Armitage (2008) for a more detailed description of code calculation startup issues and the over-winter adjustment process.

}
\value{
fwi returns a dataframe which includes both the input and the FWI System variables as described below: 
  \item{Input Variables }{Including temp, rh, ws, and prec with id, long, lat, yr, mon, or day as optional.}
  \item{ffmc }{Fine Fuel Moisture Code}
  \item{dmc }{Duff Moisture Code}
  \item{dc }{Drought Code}
  \item{isi }{Initial Spread Index}
  \item{bui }{Buildup Index}
  \item{fwi }{Fire Weather Index}
  \item{dsr }{Daily Severity Rating}

}

\author{
Xianli Wang, Alan Cantin, Marc-Andre Parisien, Mike Wotton, Kerry Anderson, and Mike Flannigan
}

\references{
1. Van Wagner, C.E. and T.L. Pickett. 1985. Equations and FORTRAN program for the Canadian Forest Fire Weather Index System. Can. For. Serv., Ottawa, Ont. For. Tech. Rep. 33. 18 p.

2. Van Wagner, C.E. 1987. Development and structure of the Canadian forest fire weather index system. Forest Technology Report 35. (Canadian Forestry Service: Ottawa).

3.  Lawson, B.D. and O.B. Armitage. 2008. Weather guide for the Canadian Forest Fire Danger Rating System. Nat. Resour. Can., Can. For. Serv., North. For. Cent., Edmonton, AB.

}

\seealso{
   \code{\link{fwiBAT}},
   \code{\link{fbp}}
}
\examples{
library(fwi.fbp)
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

}
\keyword{ methods }
