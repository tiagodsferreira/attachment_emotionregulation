############################
# Packages Loading
############################
# install.packages("pacman")
library(pacman)
# help(package=pacman)
p_load(data.table, foreign, plyr, dplyr, haven, psych, nFactors, GPArotation, psychTools, lavaan, semTools, semPlot, ggplot2, worcs, lcmm, GGally, jtools, interactions, apaTables, semPower, naniar, missMDA, Amelia, mice, missForest, FactoMineR, Tidyverse, ggcorrplot)

###########################
#Creating user defined functions
###########################
# The "imb_extractvars" function takes the imbalance data set and extracts all variables with the same name reported across informant and time. The arguments are:
#DF = name of the data frame
#varname: character vector of variable(s) name(s)
#scale_name: character with the scale name
#time: numeric vector with the assessment time(s) to extract (options are 1:4)
#method: numeric vector with the method(s) to be extracted (1 = fathers report, 2 = mother report)
imb_extractvars <- function (DF, varname, scale_name=NULL, time=1:4, method=1:2) {
  char_time <- as.character(time)
  char_method <- as.character(method)
  index <- sapply(varname, rep, length(time))
  index <- paste0(index, ".", char_time)
  index <- sapply(index, rep, length(method))
  index <- paste0(index, ".", char_method)
  mat_vars <- matrix(index, ncol=length(varname))
  list_vars <- split(mat_vars, 1:nrow(mat_vars))
  list_name <- expand.grid(time, method)
  list_name <- list_name[order(list_name$Var1),]
  list_name <- apply(list_name, 1, paste0, collapse='.')
  names(list_vars) <- list_name
  names(list_vars) <- paste(scale_name, names(list_vars), sep = ".")
  index <- na.omit(match(index, names(DF)))
  return(list(selected_vars = varname, variable_names = index, variablelist = list_vars,
              dataframe = DF[, index]))
}

# The "key_list" function returns the key for computing scales scores- use the value as the impute argument for the function psych::scoreItems
# variablelist = list of variables used to compute the scores form each moment and time
# key = numeric vector indicating the need for inverting certain items; -1 indicates reversed items
key_list <- function(variablelist, key=1){
  key <- gsub("1", "", as.character(key))
  var <- paste0(key, unlist(variablelist))
  mt <- matrix(var, nrow=length(variablelist), byrow=TRUE)
  key_list <- split(mt, seq(nrow(mt)))
  names(key_list) <- names(variablelist)
  return(key_list)
}

# keys.list <- key_list(wfc$variablelist,key= c(-1,1))
# scoreItems(keys.list, df, missing = TRUE, impute = "none", min=min, max=max)
##
#Creating parcels
create.parcel <- function(data, varnames){
  rowSums(data[, c(varnames)])/length(c(varnames))
}