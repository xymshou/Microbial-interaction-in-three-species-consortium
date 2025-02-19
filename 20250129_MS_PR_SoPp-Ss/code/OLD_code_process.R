library(flopr)

# parsing function
 
flopr::cytation_parse(data_csv = "C:/Users/miran/OneDrive - University College London/data/20250129_MS_PR_SoPp-Ss/processed/20250129_MS_So-Pp-Ss.xlsx", 
                      layout_csv = "C:/Users/miran/OneDrive - University College London/data/20250129_MS_PR_SoPp-Ss/metadata/20250129_PR_layout.csv", # save layout as csv
                      timeseries = TRUE)
# changed absorbance name to OD600 and OD730
