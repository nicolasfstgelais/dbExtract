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
 dbExtract_init()
 ```
`dbExtract()` is based on a specific structure of files to initialize (with examples) the folder structure run the `dbExtract_init()`.
If thedirectory where you want to initialize dbExtract is different from the working directory, change the working directory using `setwd(path)`. We highly recommand to run all the *dbExtract* functions from a R project created within the working directory. 

## Extract and merge data
`dbExtract()` can be use to extract and merge data from multiple locations, coming from multiple databases. The `dbExtract()` function look for specifc keywords in each database (in wide or long format) based on the keyword in *raw/inputs/categories.csv* (see [input README](https://github.com/nicolasfstgelais/dbExtract/blob/master/raw/inputs/README.md) for details on how to fill the categories file).

### For data with temporal replication 
(see [input README](https://github.com/nicolasfstgelais/dbExtract/blob/master/raw/inputs/README.md) for details)

 ```{r}
 dbExtract_init(inputFile = "temporalDB.csv")
 ``` 
 
 ### For data without temporal replication 
(see [input README](https://github.com/nicolasfstgelais/dbExtract/blob/master/raw/inputs/README.md) for details)
 
 ```{r}
 dbExtract_init(inputFile = "stationsDB.csv")
 ``` 

## Prepare data
 ```{r}
dataPrep()
 ``` 
 
## Classify sites
 ```{r}
sitesClassification()
 ``` 
`sitesClassification()` evaluate for each service in *raw/criteria/guideline.csv*
