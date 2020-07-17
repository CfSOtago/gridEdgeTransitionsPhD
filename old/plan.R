

plan1 <- drake_plan(
  # Load data
  d11_raw = data.table::as.data.table(read_excel(file_in("~/greenGridData/externalData/solarCity/2020-05-24-sampleData/Example 1 - CT Data.xlsx"), sheet = "Main")),
  d12_raw = data.table::as.data.table(read_excel(file_in("~/greenGridData/externalData/solarCity/2020-05-24-sampleData/Example 1 - CT Data.xlsx"), sheet = "Inverter")),
  # Using time functions for the first data set
  Dummy11DT = d11_raw %>%
  d11_time,
  
  Dummy12DT = d12_raw %>%
  d12_time,
  
  # Converting into power for the first data set
  Convert1DT = Dummy11DT %>%
  convert1_to_power
)

