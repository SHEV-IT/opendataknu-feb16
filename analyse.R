getUniqueNumTable <- function(dataset){
    sub_dataset <- dataset[,3:10] ## without transaction, date, sum and reason
    u_table <- sapply(sub_dataset, function(column){return(length(unique(column)))})
    return (u_table)    
}

getTableSumOnName <- function(dataset,type="sender"){
    base <- if(type=="sender") {dataset$name_sender} else {dataset$name_rec}
    dataset_table <- table(base)
    middle_table <- sapply(names(dataset_table),function(x){return(sum(dataset$sum[base==x]))})
    average <- middle_table/dataset_table
    result_table <- as.data.frame(cbind(middle_table,dataset_table,average))
    result_table <- result_table[order(middle_table,decreasing = TRUE),]
    names(result_table) <- c("sum","times","average")
    return (result_table)
}