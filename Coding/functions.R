
### UNIVERSAL FUNCTIONS

#This function adds a new column to a data.table and replaces missing values with NA
#Use this function if you have two variables in different data.tables with unequal length
add.col<-function(df, new.col) {n.row<-dim(df)[1]
length(new.col)<-n.row
cbind(df, new.col)
}



### PROJECT SPECIFIC FUNCTIONS


## TIME :: First data set
# This function uses lubridate and hms to create new columns for the first worksheet of the first data set
d11_time <- function(d11) {
  Dummy11DT <- d11
  Dummy11DT[, rDateTimeOrig := MeasurementDate] # just in case
  Dummy11DT[, rDateTime := lubridate::as_datetime(MeasurementDate)]
  Dummy11DT[, rDateTimeNZT := lubridate::force_tz(MeasurementDate, 
                                               tzone = "Pacific/Auckland")]
  Dummy11DT[, rTime := hms::as_hms(rDateTimeNZT)]
  Dummy11DT[, rMonth := lubridate::month(rDateTimeNZT, label = TRUE, abbr = TRUE)]
  Dummy11DT[, rDate := lubridate::date(rDateTimeNZT)]
}

# This function uses lubridate and hms to create new columns for the second worksheet of the first data set
d12_time <- function(d12) {
  Dummy12DT <- d12
  Dummy12DT[, rDateTimeOrig := RecordDateTime] # just in case
  Dummy12DT[, rDateTime := lubridate::as_datetime(RecordDateTime)]
  Dummy12DT[, rDateTimeNZT := lubridate::force_tz(RecordDateTime, 
                                                 tzone = "Pacific/Auckland")]
  Dummy12DT[, rTime := hms::as_hms(rDateTimeNZT)]
  Dummy12DT[, rMonth := lubridate::month(rDateTimeNZT, label = TRUE, abbr = TRUE)]
  Dummy12DT[, rDate := lubridate::date(rDateTimeNZT)]
}

## POWER :: First data set

# Convert first data set first worksheet to power
convert1_to_power <- function(c1) {
  Convert1DT <- c1
  Convert1DT <- Convert1DT[, powerW := (`Total Used`*4)*1000]
  Convert1DT <- Convert1DT[, `Solar Generated W`:= (`Solar Generated`*4)*1000]
  Convert1DT <- Convert1DT[, `Solar Exported W`:= (`Solar Exported`*4)*1000]
  Convert1DT <- Convert1DT[, `Solar Used W`:= (`Solar Used`*4)*1000]
  Convert1DT <- Convert1DT[, `Battery Discharge W`:= (`Battery Discharge`*4)*1000]
  Convert1DT <- Convert1DT[, `Battery Charge W`:= (`Battery Charge`*4)*1000]
  Convert1DT <- Convert1DT[, `Household demand W`:= powerW-`Battery Charge W`+`Battery Discharge W`]
  Convert1DT <- Convert1DT[, `Grid import W`:= (`Grid Used`*4)*1000]
  Convert1DT <- Convert1DT[, `Grid impact W`:= `Grid import W` - `Solar Exported W`]
}


