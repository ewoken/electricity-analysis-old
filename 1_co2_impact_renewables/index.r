source("../utils.r")
source("./common_graphs.r")
source("./correlation_wind_export.r")
source("./correlation_consump_emissions.r")
source("./multilinear_analysis.r")

print("Loading data...")
data = get_all_data()

# thread part 1
plot_correlation_wind_export(data)

#thread part 2

plot_correlation_consump_emissions(data)

results = get_multilinear_results(data)

plot_multilinear_reg_by_season(data, results, "wind", withISO = T)
plot_3D(data, results)
plot_results_by_season(results, "multilinear_method", "wind")

plot_multilinear_reg_by_season(data, results, "solar", withISO = T)

avoided_by_year = get_emissions_avoided_by_year(data, results)
wind_total_impact = sum(avoided_by_year$by_wind_mt_co2) # Mt CO2eq
solar_total_impact = sum(avoided_by_year$by_solar_mt_co2) # Mt CO2eq

total_wind_prod = 141.7 # TWh
total_solar_prod = 50.1 # TWh

print(paste("Wind impact: ", wind_total_impact, " Mt CO2eq (", wind_total_impact/total_wind_prod, " kg CO2eq/kWh)",  sep=""))
print(paste("Solar impact: ", solar_total_impact, " Mt CO2eq (", solar_total_impact/total_solar_prod, " kg CO2eq/kWh)",  sep=""))

source("./comparison_analysis.r")



