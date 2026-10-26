# 10_denominators
# STATUS: the census route to constituency population denominators was not
# viable, so no population-denominated facility-density measure is used.
#
# The KNBS administrative-units file (kenya_pop_admin_units_2019.xlsx) contains
# 14,174 rows across six levels: country, county, sub-county, division,
# location, sub-location. Constituencies do not appear in it. The census
# hierarchy and the electoral geography are separate systems and the file does
# not link them. Hierarchy level is encoded by Excel cell indentation rather
# than by a column, so read_excel cannot recover it without parsing the raw XML.
#
# WorldPop Kenya v2.0 gridded population, summed within constituency polygons,
# was considered as an estimated denominator and not used. It could not be
# established whether health-facility proximity enters the WorldPop model,
# which would make a facility-density measure partly circular given that
# facilities are also the numerator.
#
# Affects: the secondary facility-density model and the private-facility
# density control, neither of which is reported. The population-density
# covariate in the main model is unaffected; it is UN_Population_Density_2020
# from the DHS Geospatial Covariate Extract, attached in 23_model_prep