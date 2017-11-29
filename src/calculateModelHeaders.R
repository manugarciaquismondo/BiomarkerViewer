header.descriptions = as.matrix(auc.headers[order(as.numeric(auc.headers[, "Model"])),])
header.descriptions.body = header.descriptions[, "Headers"]
model.descriptions = c()
for (header.index in 1:nrow(header.descriptions))
    model.descriptions = c(model.descriptions, paste("Model number ",toString(header.descriptions[header.index, "Model"]), ": ", header.descriptions.body[header.index], sep = ""))
names(header.descriptions.body) = model.descriptions
#max.auc.value = max(auc.means[, auc.order.criterion])
#max.model.index = auc.means[auc.means[, auc.order.criterion] == max.auc.value, "Model"]