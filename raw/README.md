## HOW TO ADD A NEW DATABASE:

1. Each new database should be added in raw/riverData. Databases can be organized in sub-folders to better orgnize files.
2. Each data daseshould be checked to make sure that the following minimal criteria are meet. Is some cleaning is needed, we 
recomand using OpenRefine. 
	- If in long format, one column should contain both the parameter measured (e.g. phosphosrus) as well as the fraction
	 measured (e.g. total)
	- Dates should be in a consistent format (even if dbExtract is robust to multiple date format)
	- A stationID column, with associated station information in the [raw/station](https://github.com/nicolasfstgelais/dbExtract/blob/master/stations) folder 
3. Each database to be processed by dbExtract() should be added in an input file (see [raw/inputs](https://github.com/nicolasfstgelais/dbExtract/blob/master/inputs) for more details on the format)
