
if (!(file.exists(file.path(input.data.route, "aucheaders.csv")) && file.exists(file.path(input.data.route, "aucmeans.csv")))) {
    #input.auc.table = read.csv(file.path(input.data.route, "aucdata.csv"))
    setwd(source.route)
    source("CreateHeaderTable.R")
}