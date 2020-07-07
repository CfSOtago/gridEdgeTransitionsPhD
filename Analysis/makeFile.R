# Libraries ----
#library(woRkflow) # remember to build it first :-)
#woRkflow::setup() # load env.R set up the default paths etc

# Additional functions
source("/home/carsten/gridEdgeTransitionsPhD/R/loadLibraries.R")
source("/home/carsten/gridEdgeTransitionsPhD/R/setup.R")
source("/home/carsten/gridEdgeTransitionsPhD/R/tidyNum.R")

reqLibs <- c("data.table", # data munching
             "drake", # what's done stays done
             "here", # here
             "lubridate", # dates and times
             "ggplot2", # plots
             "skimr", # for skim
             "knitr", # for kable
             "readr", # for .csv
             "hms", # manipulate hms
             "dplyr", # data munching
             "kableExtra", # for tables
             "visNetwork", # for drake's dependency graph
             "callr", # for drake commands
             "readxl" # for reading excel files
)
# load them
#woRkflow::loadLibraries(reqLibs)

loadLibraries(reqLibs)



# Parameters ----

rmdFile <- "gridEdgeTransitionsPhD" # <- name of the .Rmd file to run at the end 
title <- "Carsten's PhD analysis"
subtitle <- "Solarcity data in action"
authors <- "Carsten Dortans"

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

#These two functions add a new column to a data.table and replaces missing values with NA
#Use this function if you have two variables in different data.tables with unequal length
add.col <- function(df, new.col) {n.row<-dim(df)[1]
  length(new.col)<-n.row
  cbind(df, new.col)
}

new.name <- function(df){
  df[, `Battery Capacity %` := `new.col` ]
  df[, `new.col`:= NULL]
}


# Drake ----

# Set up
startTime <- proc.time()

# Set the drake plan
plan <- drake::drake_plan(
  # Load data
  CD1_raw = data.table::as.data.table(read_excel(file_in("~/greenGridData/externalData/solarCity/2020-05-24-sampleData/Example 1 - CT Data.xlsx"), sheet = "Main")),
  CD2_raw = data.table::as.data.table(read_excel(file_in("~/greenGridData/externalData/solarCity/2020-05-24-sampleData/Example 1 - CT Data.xlsx"), sheet = "Inverter")),
  # Apply lubridate to imported CD data
  CD1DT = time_CD_1(CD1_raw),
  CD2DT = time_CD_2(CD2_raw),
  # Add a column from CD2DT to CD1DT an rename it
  solar1DTa = add.col(CD1DT,CD2DT$`Battery 1 Capacity %`),
  solar1DT = new.name(solar1DTa)



  
)

# Run drake plan
make(plan)

# Load data from cache
loadd(solar1DT)
















# Run the report ----

#makeReport(rmdFile)
message("Data covers ", min(dt$rDateTimeUTC), " to ", max(dt$rDateTimeUTC))

# Finish off ----

t <- proc.time() - startTime # how long did it take?
elapsed <- t[[3]]

print("Done")
print(paste0("Completed in ", round(elapsed/60,3), " minutes using ",
             R.version.string, " running on ", R.version$platform))
