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

## Add a dataset 
Datasets should be added in the raw folder (see [data README](https://github.com/nicolasfstgelais/dbExtract/blob/master/raw/README.md) for details)



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
dataPrep(stationsPath="data/dbExtract_stationsDB.csv",guidePath="raw/criteria/guidelines.csv",temporalPath="data/dbExtract_temporalDB.csv",by="ym") to *data/temporalDBwide.csv"
 ``` 
 This function prepare the different data source needed for the classification step. First the *guidelines.csv* is normalized. Units are checked in the *dbExtract_temporalDB.csv* and the data is summarized either by date (by="d"), year+month (by="ym") or month by="m") and exported in a wide format 
 
## Classify sites
 ```{r}
sitesClassification(temporalPath="data/temporalDBwide.csv",selSpaces=c("irrigation","livestock","drink","aquatic","recreational","oligotrophic","mesotrophic","eutrophic")
 ``` 
`sitesClassification()` evaluate for each service seleted in the *selSpaces* argument (by default irrigation, livestock, drink, aquatic, recreational, oligotrophic, mesotrophic et eutrophic) based on the temporal database
