
## categories.csv
Input file with the parameters to look for in the databases
normVocab: for each parameter the normalized vocabulary
keywords: Which keywords to look for (regex expressions)

## input_stations.csv
Input file for data without temporal replication at stations (e.g. latitude):
* path:relative path to the database (.csv or .xlsx)
* sheet: for xlsx list of files (separated by ;) to extract
* stationID: column name for stations name or ID
* Date ID: should be blank
* units: column for units (if applicable)
* parameters: **only for databases in the long format** name of the column with parameters 
* values: **only for databases in the long format** name of the column with values
* NAvalue: specify how NAs are coded in the database

## temporalDB.csv
Input file for data with temporal replication at stations:
* Date ID:column with dates (best if yyyy-mm-dd)


