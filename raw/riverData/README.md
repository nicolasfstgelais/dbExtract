Remove the two pwqmn files and add required information on the new databases in the *dbInput.csv* file:
  * The relatives path to each database
  * The extension (.csv or .xslx)
  * The column name with : stations ID, Dates, units, parameters (wideVar), measurements (wideResult)
  * The value used for NA
  * if the matrix need to be tranposed (Y/NA)
  * Number of lines to skip before reading
  
  In the *categories.csv* file for each variable of interest there is the regex expression to extract relavant parameters, possible to add or remove varaibles or change regex expression.
  When running the DB extract function, check the log files (in /logs) to make sure that the right parameters are selected and can also explore the colNames.csv
  file to check if all relevant variables were extracted. 
