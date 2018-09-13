#' dbExtract_init  main function
#'
#' This function is designed to explore databases and summarize
#' the spatial and temporal coverage of pre-selected varaibles (need to fill the input.xls file)
#' @param inputFile inputFile filename, needs to be in the working directory
#' @param dirPath   the path to the databases (see input file)
#' @param startAt at which line to start in input
#' @param append = F,
#' @param lineSkip
#' @param lvl
#' @keywords cats
#' @export
#' @examples
#' dbExtract_init ()
dbExtract_init()
dbExtract_init<-function(){
dir.create("raw")}
