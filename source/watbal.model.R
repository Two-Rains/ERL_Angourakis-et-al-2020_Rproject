################################################################################
# "Working with dynamic models for agriculture"
# R script for practical work
# Daniel Wallach (INRA), David Makowski (INRA), James W. Jones (U.of Florida),
# Francois Brun (ACTA), Sylvain Toulet (INRA, internship 2012)
# version : 2019-10-30 by Andreas Angourakis (using version from 2012-04-23)
# Model described in the book, Appendix. Models used as illustrative examples: description and R code
################################ FUNCTIONS #####################################
################################################################################
#' @title WaterBalance model - calculate change in soil water for one day
#' @description WaterBalance model - calculate change in soil water for one day
#' @param modelVars : A dataframe containing the daily series for a year of Rainfall of day (mm) or RAIN, Evapotranspiration of day (mm) or ETr, water at the beginning of the day (mm) or WAT, and the ARID drought index.
#' @param lastDay : the day from which variables will be calculated (there must be a slot in modelVars for lastDay + 1).
#' @param param : a vector of parameters
#' @param FC : Water content at field capacity (cm^3.cm^-3)
#' @param WP : Water content at wilting Point (cm^3.cm^-3)
#' @param typeOfParValue : type of parameter value: (nominal, binf, bsup)
#' @export
watbal.update = function(modelVars, lastDay, param, WP, FC, typeOfParValue){
  WHC =(param[typeOfParValue, "WHC"])
  MUF = (param[typeOfParValue, "MUF"])
  DC = (param[typeOfParValue, "DC"])
  z = (param[typeOfParValue, "z"])
  CN = (param[typeOfParValue, "CN"])
  # Maximum abstraction (for run off)
  S = 25400 / CN - 254
  # Initial Abstraction (for run off)
  IA = 0.2 * S
  # WATfc : Maximum Water content at field capacity (mm)
  WATfc = FC * z
  # WATwp : Water content at wilting Point (mm)
  WATwp = WP * z
  
  # get lastDay's values
  RAIN = modelVars$RAIN[lastDay]
  ETr = modelVars$ETr[lastDay]
  WAT = modelVars$WAT[lastDay]

  # Change in Water Before Drainage (Precipitation - Runoff)
  if (RAIN > IA){RO = (RAIN - 0.2 * S)^2 / (RAIN + 0.8 * S)} else {RO = 0}
  # Calculating the amount of deep drainage
  if (WAT + RAIN - RO > WATfc){DR = DC * (WAT + RAIN - RO - WATfc)} else {DR = 0}
  
  # Calculate rate of change of state variable WAT
  # Compute maximum water uptake by plant roots on a day, RWUM
  RWUM = MUF * (WAT + RAIN - RO - DR - WATwp)
  # Calculate the amount of water lost by transpiration (TR)
  TR = min(RWUM, ETr)
  
  dWAT = RAIN - RO -DR -TR
  modelVars$WAT[lastDay + 1] = WAT + dWAT

  # compute the ARID index. Note that it is an auxilliary variable, not a "state variable" as is WAT[day]
  if (TR < ETr) {modelVars$ARID[lastDay + 1] = 1 - TR / ETr}
  else {modelVars$ARID[lastDay + 1] = 0.0}
  
  return(modelVars)
}

################################################################################
#' @title WaterBalance model - calculate soil water over designated time period
#' @description WaterBalance model - calculate soil water over designated time period
#' @param param : a vector of parameters
#' @param weather : weather data.frame for one single year
#' @param WP : Water content at wilting Point (cm^3.cm^-3)
#' @param FC : Water content at field capacity (cm^3.cm^-3)
#' @param WAT0 : Initial Water content (mm). If NA WAT0=z*FC
#' @return data.frame with daily RAIN, ETR, Water at the beginning of the day (absolute : WAT, mm and relative value : WATp, -), and ARID coefficient
#' @export
watbal.model = function(param, weather, WP, FC, WAT0=NA, typeOfParValue = "nominal")
{
  z = (param[typeOfParValue, "z"])
  # input variable describing the soil
  # WP : Water content at wilting Point (cm^3.cm^-3)
  # FC : Water content at field capacity (cm^3.cm^-3)
  # WAT0 : Initial Water content (mm)
  if (is.na(WAT0)) {WAT0 = z*FC}
  
  # Initialize variable
  modelVars <- data.frame(
    year = weather$year,
    day = weather$day, 
    RAIN = weather$RAIN, 
    ETr = weather$ETr, 
    # WAT : Water at the beginning of the day (mm) : State variable
    WAT = rep(NA, nrow(weather)), 
    WATp = rep(NA, nrow(weather)), 
    # supplementary variable ARID drought index. 
    # computed as the ratio of transpiration to potential transpiration. (See Woli, 2010)        
    # A value of ARID = 0 means that there is no water stress in the crop; a value of ARID=1 means a maximum stress with no growth
    ARID = rep(NA, nrow(weather)))
  
  # initialisation use Amount of water at the beginning
  modelVars$WAT[1] = WAT0
  modelVars$ARID[1] = NA

  # integration loops
  for (day in 1:(nrow(modelVars) - 1))
  {
    modelVars <- watbal.update(modelVars, day, param, WP, FC, typeOfParValue)
  }
  
  # Volumetric Soil Water content (fraction : mm.mm-1)
  modelVars$WATp = modelVars$WAT / z
  return(modelVars);
}

################################################################################
#' @title Define values of the parameters for the WaterBalance model
#' @description Define values of the parameters for the WaterBalance model
#' @return matrix with parameter values (nominal, binf, bsup)
#' @export
watbal.define.param = function()
{
  # nominal, binf, bsup
  # WHC  : Water Holding Capacity of the soil (cm^3 cm^-3)
  WHC = c(0.13, 0.05, 0.18);
  # MUF : Water Uptake coefficient (mm^3 mm^-3)
  MUF = c(0.096, 0.06, 0.11);
  # DC :  Drainage coefficient (mm^3 mm^-3)
  DC = c(0.55, 0.25, 0.75);
  # z : root zone depth (mm)
  z = c(400, 300, 600);
  # CN : Runoff curve number
  CN = c(65, 15, 90); # nominal = 58 in the description ??
  
  param = data.frame(WHC, MUF, DC, z, CN);
  row.names(param) = c("nominal","binf","bsup");
  param = as.matrix(param)
  attributes(param)$description=t(t(c("WHC"="Water Holding Capacity of the soil (cm3.cm-3)",
                                      "MUF" = "Water Uptake coefficient (mm^3 mm^-3)", "DC" = "Drainage coefficient (mm3.mm-3)",
                                      "z" = "root zone depth (mm)",  "CN" = "Runoff curve number")))
  return(param)
}

################################################################################
#' @title Read weather data for the WaterBalance model (West of France Weather)
#' @description Read weather data for the WaterBalance model (West of France Weather)
#' @param working.year : year for the subset of weather data (default=NA : all the year)
#' @param working.site : site for the subset of weather data (default=NA : all the site)
#' @return data.frame with daily weather data for one or several site(s) and for one or several year(s)
#' @export
# Reading Weather data function
watbal.weather = function(working.year=NULL, working.site=NULL)
{
  #day month year R Tmax Tmin rain ETP
  # R : solar radiation (MJ)
  # Tmax : maximum temperature (degC)
  # Tmin : minimum temperature (degC)
  weather=ZeBook::weather_FranceWest
  names(weather)[names(weather)=="WEDAY"]= "day"
  names(weather)[names(weather)=="WEYR"]= "year"
  names(weather)[names(weather)=="SRAD"]= "I"
  names(weather)[names(weather)=="TMAX"]= "Tmax"
  names(weather)[names(weather)=="TMIN"]= "Tmin"
  names(weather)[names(weather)=="RAIN"]= "RAIN"
  names(weather)[names(weather)=="ETr"]= "ETr"
  # if argument working.year/working.site is specified, work on one particular year/site
  if (!is.null(working.year)&!is.null(working.site)) {weather=weather[(weather$year==working.year)&(weather$idsite==working.site),] }
  else{
    if (!is.null(working.year)) {weather<-weather[(weather$year==working.year),]}
    if (!is.null(working.site)) {weather<-weather[(weather$idsite==working.site),]}}
  return (weather)
}

################################################################################
#' @title Read weather data for the WaterBalance model (NASA POWER csv download)
#' @description Read weather data for the WaterBalance model (NASA POWER csv download)
#' @param fileName : name of the file containing the weather data dowloaded from NASA power (power.larc.nasa.gov)
#' @param year : selected year (NULL = all)
#' @return data.frame with daily weather data extracted from file, plus estimated ETr
#' @export
# Reading Weather data function
watbal.weather.file = function(fileName, year = NULL)
{
  #day month year R Tmax Tmin rain ETP
  # R : solar radiation (MJ)
  # Tmax : maximum temperature (degC)
  # Tmin : minimum temperature (degC)
  # assuming file downloaded in NASA POWER (power.larc.nasa.gov/data-access-viewer/)
  weather <- read.csv(fileName, skip = 17) 
  
  
  names(weather)[names(weather)=="DOY"]= "day"
  names(weather)[names(weather)=="YEAR"]= "year"
  names(weather)[names(weather)=="ALLSKY_SFC_SW_DWN"]= "I"
  names(weather)[names(weather)=="T2M_MAX"]= "Tmax"
  names(weather)[names(weather)=="T2M_MIN"]= "Tmin"
  names(weather)[names(weather)=="PRECTOT"]= "RAIN"
  
  # if argument working.year/working.site is specified, work on one particular year/site
  if (!is.null(year)) {
    weather=weather[weather$year %in% year,] }
  return (weather)
}

################################################################################
#' @title WaterBalance model - Variant with another order of calculation and ARID index
#' @description WaterBalance model - Variant with another order of calculation and ARID index
#' @param WHC : Water Holding Capacity of the soil (cm^3 cm^-3)
#' @param MUF : Water Uptake coefficient (mm^3 mm^-3)
#' @param DC : Drainage coefficient (mm^3 mm^-3)
#' @param z : root zone depth (mm)
#' @param CN : Runoff curve number
#' @param weather : weather data.frame for one single year
#' @param WP : Water content at wilting Point (cm^3.cm^-3)
#' @param FC : Water content at field capacity (cm^3.cm^-3)
#' @param WAT0 : Initial Water content (mm). If NA WAT0=z*FC
#' @return data.frame with daily RAIN, ETR, Water at the beginning of the day (absolute : WAT, mm and relative value : WATp, -)
#' @export
watbal.model.arid = function(WHC, MUF, DC, z, CN, weather, WP, FC, WAT0=NA)
{
  #WHC :Water Holding Capacity of the soil (cm3.cm-3)
  #MUF :Water Uptake coefficient (mm^3 mm^-3)        
  #DC :Drainage coefficient (mm3.mm-3)              
  #z :root zone depth (mm)                         
  #CN :Runoff curve number 
  
  # Maximum abstraction (for run off)
  S = 25400/CN-254
  # Initial Abstraction (for run off)
  IA = 0.2*S
  # WATfc : Maximum Water content at field capacity (mm)
  WATfc = FC*z
  # WATwp : Water content at wilting Point (mm)
  WATwp = WP*z
  
  # input variable describing the soil
  # WP : Water content at wilting Point (cm^3.cm^-3)
  # FC : Water content at field capacity (cm^3.cm^-3)
  # WAT0 : Initial Water content (mm)
  if (is.na(WAT0)) {WAT0 = z*FC}
  # Initialize variable
  # WAT : Water at the beginning of the day (mm) : State variable
  WAT = rep(NA, nrow(weather))
  
  # supplementary variable ARID drought index. 
  # computed as the ratio of transpiration to potential transpiration. (See Woli, 2010)        
  # A value of ARID = 0 means that there is no water stress in the crop; a value of ARID=1 means a maximum stress with no growth
  ARID = rep(NA, nrow(weather))
  
  # initialisation-use Amount of water at the beginning
  WAT[1]=WAT0
  ARID[1] = NA
  # integration loops
  for (day in 1:(nrow(weather)-1))
  {
    # Calculate rate of change of state variable WAT
    # Compute maximum water uptake by plant roots on a day, RWUM
    RWUM = MUF*(WAT[day]-WATwp)
    # Calculate the amount of water lost by transpiration (TR)-prior to RAIN, RO, and DR 
    TR = min(RWUM, weather$ETr[day])       
    
    # Compute Surface Runoff (RO)
    if (weather$RAIN[day]>IA){RO = (weather$RAIN[day]-0.2*S)^2/(weather$RAIN[day]+0.8*S)}else{RO = 0}
    # Calculate the amount of deep drainage (DR)
    if (WAT[day]+weather$RAIN[day]-RO > WATfc){DR = DC*(WAT[day]+weather$RAIN[day]-RO-WATfc)}else{DR = 0}
    
    # Update state variables 
    dWAT = weather$RAIN[day] - RO -DR -TR
    WAT[day+1] = WAT[day] + dWAT
    
    # compute the ARID index. Note that it is an auxilliary variable, not a "state variable" as is WAT[day]
    if (TR < weather$ETr[day])   {ARID[day+1] = 1 - TR/weather$ETr[day]}      else    {ARID[day+1] = 0.0}
    
  }
  
  # Volumetric Soil Water content (fraction : mm.mm-1)
  WATp=WAT/z
  return(data.frame(day = weather$day, RAIN = weather$RAIN, ETr = weather$ETr, WAT = WAT, WATp=WATp, ARID=ARID));
}

################################################################################
#' @title Estimate daily reference evapotranspiration (ET0)
#' @description Estimates daily reference evapotranspiration using either Penman-Monteith equation (default) or Priestley-Taylor equation.
#' @param R_s : solar radiation or insolation (MJ m2 day-1)
#' @param Temp : daily average temperature at 2m (ºC)
#' @param maxTemp : daily maximum temperature at 2m (ºC)
#' @param minTemp : daily minimum temperature at 2m (ºC)
#' @param dewTemp : daily dew point temperature at 2m (ºC)
#' @param windSpeed : mean daily wind speed at 2m (m s-1)
#' @param method : "PM" to use Penman-Monteith equation; "PT" to use Priestley-Taylor equation
#' @param z : elevation above sea level (m)
#' @return vector with daily ET0 (mm day-1)
#' @export
estimateET0 <- function(R_s, Temp,  maxTemp, minTemp, dewTemp = NULL, windSpeed = NULL, method = "PM", z = 200)
{
  ET0 <- NULL
  
  # estimate the net solar radiation, 
  # assuming canopy reflection or albedo of hypothetical grass reference crop (0.23)
  R_n <- (1 - 0.23) * R_s
  
  if (is.null(windSpeed)) 
  {
    windSpeed = 2 
    # as recommended by 
    # http://www.fao.org/3/X0490E/x0490e07.htm#estimating%20missing%20climatic%20data
  }
  
  # estimation of saturated vapour pressure (e_s) and actual vapour pressure (e_a)
  # according to 
  # Allen, R. G., Pereira, L. A., Raes, D., and Smith, M. 1998. 
  # “Crop evapotranspiration.”FAO irrigation and  drainage paper 56, FAO, Rome.
  # also: http://www.fao.org/3/X0490E/x0490e07.htm
  
  e_o <- function(temperature) {
    return(0.6108 * exp(17.27 * temperature / (temperature + 237.3)))
  }
  e_s = (e_o(maxTemp) + e_o(minTemp)) / 2
  
  if (is.null(dewTemp))
  {
    dewTemp <- minTemp
    # as recommended by 
    # http://www.fao.org/3/X0490E/x0490e07.htm#estimating%20missing%20climatic%20data
    # however, possibly minTemp > dewTemp under arid conditions
  }
  e_a = e_o(dewTemp)
  
  
  # slope of  the  vapor  pressure-temperature  curve (kPa ºC−1)
  DELTA = 4098 * e_o(Temp) / (Temp + 237.3) ^ 2
  
  # useful references:
  # Suleiman A A and Hoogenboom G 2007 
  # Comparison of Priestley-Taylor and FAO-56 Penman-Monteith for Daily Reference Evapotranspiration Estimation in Georgia 
  # J. Irrig. Drain. Eng. 133 175–82 Online: http://ascelibrary.org/doi/10.1061/%28ASCE%290733-9437%282007%29133%3A2%28175%29
  # also: Jia et al. 2013 - doi:10.4172/2168-9768.1000112
  # constants found in: http://www.fao.org/3/X0490E/x0490e07.htm
  # see also r package: Evapotranspiration (consult source code)
  
  # latent heat of vaporisation = 2.45 MJ.kg^-1
  lambda = 2.45
  
  # specific heat at constant pressure, 1.013 10-3 [MJ kg-1 °C-1]
  c_p = 1.013 * 10^-3
  # ratio molecular weight of water vapour/dry air
  epsilon = 0.622
  # atmospheric pressure (kPa); z = elevation above sea level [m]
  P = 101.3 * ((293 - 0.0065 * z) / 293) ^ 5.26
  # psychometric constant (kPa ºC−1)
  gamma = c_p * P / (epsilon * lambda) 
  
  if (method == "PM")
  {
    # Penman-Monteith equation from: fao.org/3/X0490E/x0490e0 ; and from: weap21.org/WebHelp/Mabia_Alg ETRef.htm
    
    # 900 and 0.34 for the grass reference; 1600 and 0.38 for the alfalfa reference
    C_n = 900
    C_d = 0.34
    
    ET0 <- (0.408 * DELTA * R_n + gamma * (C_n / (Temp + 273)) * windSpeed * (e_s - e_a)) / (DELTA + gamma * (1 + C_d * windSpeed))
  }
  
  if (method == "PT")
  {
    # alternatively, using Priestley-Taylor equation 
    # (Priestley and Taylor 1972, https://doi.org/10.1175/1520-0493(1972)100%3C0081:OTAOSH%3E2.3.CO;2)
    
    # Priestley-Taylor coefficient
    alpha = 1.26
    
    ET0 <- (alpha / lambda) * (DELTA / (DELTA + gamma)) * R_n
  }
  
  return(ET0)
}
# End of file
