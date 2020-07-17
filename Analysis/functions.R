# Functions ----

# Make report
makeReport <- function(f){
  # default = html
  rmarkdown::render(input = paste0(here::here("Analysis", f),".Rmd"), # we love here:here() - it helps us find the .Rmd to use
                    params = list(title = title,
                                  subtitle = subtitle,
                                  authors = authors),
                    output_file = paste0(here::here("Documents", f),".html") # where the output goes
  )
}

## Import data
#This function imports LD data and combines the worksheets
import.ld.data <- function(dt){
 dt <- excel_sheets("~/greenGridData/externalData/solarCity/2020-05-24-sampleData/Example 2 - LD Data.xlsx") %>%
 map_df(~read_xlsx("~/greenGridData/externalData/solarCity/2020-05-24-sampleData/Example 2 - LD Data.xlsx",.))
  return(dt)
}

## Time
# These two functions apply lubridate  to create new columns for the CD data set 1st and 2nd worksheet
# it assumes you pass in a data.table. You can pass in any one you want as long as it has `RecordDateTime`
# usage: newDT <- d12_time(oldDT)

time_CD_1 <- function(dt) {
  dt[, rDateTimeOrig := MeasurementDate] # just in case
  dt[, rDateTime := lubridate::as_datetime(MeasurementDate)]
  dt[, rDateTimeNZT := lubridate::force_tz(MeasurementDate, 
                                           tzone = "Pacific/Auckland")]
  dt[, rTime := hms::as_hms(rDateTimeNZT)]
  dt[, rMonth := lubridate::month(rDateTimeNZT, label = TRUE, abbr = TRUE)]
  dt[, rDate := lubridate::date(rDateTimeNZT)]
  return(dt)
}

time_CD_2 <- function(dt) {
  dt[, rDateTimeOrig := RecordDateTime] # just in case
  dt[, rDateTime := lubridate::as_datetime(RecordDateTime)]
  dt[, rDateTimeNZT := lubridate::force_tz(RecordDateTime, 
                                           tzone = "Pacific/Auckland")]
  dt[, rTime := hms::as_hms(rDateTimeNZT)]
  dt[, rMonth := lubridate::month(rDateTimeNZT, label = TRUE, abbr = TRUE)]
  dt[, rDate := lubridate::date(rDateTimeNZT)]
  return(dt)
}

# Apply the same logic on LD data set
time_LD <- function(dt) {
  dt[, rDateTimeOrig := `Date`] # just in case
  dt[, rDateTime := lubridate::as_datetime(`rDateTimeOrig`)]
  dt[, rDateTimeNZT := lubridate::force_tz(`rDateTimeOrig`, 
                                           tzone = "Pacific/Auckland")]
  dt[, rTime := hms::as_hms(rDateTimeNZT)]
  dt[, rMonth := lubridate::month(rDateTimeNZT, label = TRUE, abbr = TRUE)]
  dt[, rDate := lubridate::date(rDateTimeNZT)]
  return(dt)
}



#This function creates a new DT with specific columns from elsewhere, setkey, merging 
#Use this function if you have two variables in different data.tables with unequal length
select.columns <- function(dt, x, y){
  dt <- data.table()
  dt[, rDateTime := x$rDateTime]
  dt[, `Battery Capacity %` := x$`Battery 1 Capacity %`]
  setkey(y, rDateTime)
  setkey(dt, rDateTime)
  dt <- merge(y, dt, all.x=TRUE)
  return(dt)
}


# Convert first data set to power and rename columns, perform calculations
convert_to_power <- function(dt) {
  dt[, powerW := (`Total Used`*4)*1000]
  dt[, `Solar Generated W`:= (`Solar Generated`*4)*1000]
  dt[, `Solar Exported W`:= (`Solar Exported`*4)*1000]
  dt[, `Solar Used W`:= (`Solar Used`*4)*1000]
  dt[, `Battery Discharge W`:= (`Battery Discharge`*4)*1000]
  dt[, `Battery Charge W`:= (`Battery Charge`*4)*1000]
  dt[, `Household demand W`:= powerW-`Battery Charge W`+`Battery Discharge W`]
  dt[, `Grid import W`:= (`Grid Used`*4)*1000]
  dt[, `Grid impact W`:= `Grid import W` - `Solar Exported W`]
}

# Convert second data set to power and rename columns, perform claculations
convert_to_power.ld <- function(dt){
  
 # dt[, `Grid import W`:= ((`TAPGr(kWh)` - shift(`TAPGr(kWh)`)*60)*1000), keyby = .(rDate)]
  dt[, `Solar Generated W`:= ((`TPvA(kWh)` - shift(`TPvA(kWh)`))*60*1000), keyby = .(rDate)]
  dt[, `Solar Generated W`:= ifelse(`Solar Generated W`<0,0,`Solar Generated W`)]#No negatives
  
  dt[, `Battery Discharge W`:= ((`TADCh(kWh)` - shift(`TADCh(kWh)`))*60*1000), keyby = .(rDate)]
  dt[, `Battery Charge W`:= ((`TACh(kWh)` - shift(`TACh(kWh)`))*60*1000), keyby = .(rDate)]

  dt[, `Grid import W`:= ((`TAPGr(kWh)` - shift(`TAPGr(kWh)`))*60*1000), keyby = .(rDate)]
  dt[, `Grid impact W`:= (((`TAPGr(kWh)` - shift(`TAPGr(kWh)`))*60*1000)
                        - (`TAP2Gr(KWh)` - shift(`TAP2Gr(KWh)`))*60*1000)
                          , keyby = .(rDate)]# Import-export
  
  dt[, `Household demand W`:= (`Solar Generated W`+`Grid import W`+`Battery Discharge W`-`Battery Charge W`)]

}

# Plots ----

# Grid impact
grid.impact.initialDT <- function(dt, xy){
  dt <- xy
  dt[`rDate`=="2019-11-04"]
}
grid.impact.initial <- function(dt){
  p <- ggplot2::ggplot(dt, aes(x= rTime)) +
    geom_line(aes(y=`Grid impact W`, colour = "Grid impact W"))+
    scale_color_manual(values = c(`Grid impact W`='red'), 
                       labels = c("Grid impact"),
                       breaks= c("Grid impact W"))+
    theme(axis.text.x = element_text(colour = "black"),
          axis.text.y = element_text(colour ="black"))+
    theme(legend.title=element_blank())+
    labs(x='Time', y='Power (W)')+
    theme(legend.position="bottom")
  return(p)
}

# Battery Charge
battery.charge.initialDT <- function(dt, xy){
  dt <- xy
  dt[`rDate`=="2019-11-04"]
}
battery.charge.initial <- function(dt){
   p <- ggplot2::ggplot(dt, aes(x= rTime)) +
   geom_line(aes(y=`Battery Charge W`, colour = "Battery Charge W"))+
   geom_line(aes(y=`Solar Generated W`, colour = "Solar Generated W"))+
   geom_line(aes(y=`Solar Exported W`, colour = "Solar Exported W"))+
   geom_line(aes(y=`Battery Discharge W`, colour = "Battery Discharge W"))+
   geom_line(aes(y=`Household demand W`, colour = "Household demand W"))+
   geom_line(aes(y=`Grid import W`, colour = "Grid import W"))+
   scale_color_manual(values = c(`Battery Charge W`='red', `Solar Generated W`='orange', `Solar Exported W`='blue', `Battery Discharge W`='black', `Household demand W`='green', `Grid import W`='purple'), 
                       labels = c("Battery Charge", "Solar generation", "Solar export", "Battery Discharge", "Household demand", "Grid import"),
                       breaks= c("Battery Charge W", "Solar Generated W", "Solar Exported W", "Battery Discharge W", "Household demand W", "Grid import W"))+
   theme(axis.text.x = element_text(colour = "black"),
          axis.text.y = element_text(colour ="black"))+
   theme(legend.title=element_blank())+
   labs(x='Time', y='Power (W)')+
   theme(legend.position="bottom")
  return(p)
}
  
  
  
  

