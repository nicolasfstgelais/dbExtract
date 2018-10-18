# dbExtract: aggregate water quality databases
 `dbExtract` is a package to extract data from water quality databases and create an aggregated database based on keywords.

 ## Install

 You can install the development version version from gitHub

 ```{r eval=FALSE}
 install.packages("devtools")
 devtools::install_github("nicolasfstgelais/dbExtract")
 ```
 # Using `dbExtract`

 Load the package:

 ```{r}
 library("dbExtract")
 ```
## Initialize  file structure
 ```{r}
 `dbExtract_init()`
 ```
`dbExtract()` is based on a specific structure of files to initialize (with examples) the folder structure run the `dbExtract_init()`.
If thedirectory where you want to initialize dbExtract is different from the working directory, change the working directory using `setwd(path)`. We highly recommand to run all the *dbExtract* functions from a R project created within the working directory. 

`dbExtract()`Is used to 


In the raw folder, read each README to make sure that the inputs are what the function is expecting.

## Normalize stations file
 ```{r}
DBnormStations()
 ``` 
 
 `DBnormStations()`  normalize the stations data based on *dbInputStations.csv* and create *data/stations_norm.csv* as an output

## Prepare data
 ```{r}
dataPrep()
 ``` 
 
## Classify sites
 ```{r}
sitesClassification()
 ``` 
`sitesClassification()` evaluate for each service in *raw/criteria/guideline.csv*
