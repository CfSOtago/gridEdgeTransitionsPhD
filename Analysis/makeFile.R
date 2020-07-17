source("/home/carsten/gridEdgeTransitionsPhD/Analysis/plan.R")
source("/home/carsten/gridEdgeTransitionsPhD/Analysis/functions.R")
source("/home/carsten/gridEdgeTransitionsPhD/Analysis/packages.R")


# Libraries ----
library(woRkflow) # remember to build it first :-)
#woRkflow::setup() # load env.R set up the default paths etc

# Additional functions
#source("/home/carsten/gridEdgeTransitionsPhD/R/loadLibraries.R")
#source("/home/carsten/gridEdgeTransitionsPhD/R/setup.R")
#source("/home/carsten/gridEdgeTransitionsPhD/R/tidyNum.R")

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
             "readxl",
             "purrr", # for imprting files
             "networkD3", # for more graphic elements
             "ggraph",
             "purrr" # for importing files
)
# load them
woRkflow::loadLibraries(reqLibs)

#loadLibraries(reqLibs)



# Parameters ----

rmdFile <- "gridEdgeTransitionsPhD" # <- name of the .Rmd file to run at the end 
title <- "Carsten's PhD analysis"
subtitle <- "Solarcity data in action"
authors <- "Carsten Dortans"


# Make report ----

# Set up
startTime <- proc.time()



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






# Run the report ----

#makeReport(rmdFile)
message("Data covers ", min(dt$rDateTimeUTC), " to ", max(dt$rDateTimeUTC))

# Finish off ----

t <- proc.time() - startTime # how long did it take?
elapsed <- t[[3]]

print("Done")
print(paste0("Completed in ", round(elapsed/60,3), " minutes using ",
             R.version.string, " running on ", R.version$platform))
