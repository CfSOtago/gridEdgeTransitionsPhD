
# Set the drake plan
plan <- drake::drake_plan(
  # Load data
  CD1_raw = data.table::as.data.table(read_excel(file_in("~/greenGridData/externalData/solarCity/2020-05-24-sampleData/Example 1 - CT Data.xlsx"), sheet = "Main")),
  CD2_raw = data.table::as.data.table(read_excel(file_in("~/greenGridData/externalData/solarCity/2020-05-24-sampleData/Example 1 - CT Data.xlsx"), sheet = "Inverter")),
  # Apply lubridate to imported CD data
  CD1DT = time_CD_1(CD1_raw),
  CD2DT = time_CD_2(CD2_raw),
  # Add a column from CD2DT to CD1DT an rename it
  CD1data = select.columns(CD1data, CD2DT, CD1DT),
  # Convert to power values and rename columns, perform calculations
  CD1power = convert_to_power(CD1data),
  # Create a DT used for a plot focussing on the grid impact
  Plot.grid.impact.initialDT = grid.impact.initialDT(Plot.grid.impact.initialDT, CD1power),
  Plot.grid.impact.initial = grid.impact.initial(Plot.grid.impact.initialDT),
  # Create another DT for battery charge plot
  Plot.battery.charge.initialDT = battery.charge.initialDT(Plot.battery.charge.initialDT, CD1power),
  Plot.battery.charge.initial = battery.charge.initial(Plot.battery.charge.initialDT)

  
)
