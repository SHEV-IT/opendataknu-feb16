formatGeneral <- function(dataset){
    dataset <- formatCode(dataset)
    dataset <- formatReason(dataset)
    dataset <- formatName(dataset)
    dataset <- formatBank(dataset)
    return (dataset)
}

formatCode <- function(dataset){
    code_length <- 8
    code_rec <- dataset$code_rec
    code_sender <- dataset$code_sender
    for(i in 1:length(code_rec)){
        diff <- code_length-nchar(code_rec[i])
        if(is.na(code_rec[i])) next
        else if(diff>0) dataset$code_rec[i] <- paste0(paste0(rep("0",times=diff),collapse=""),code_rec[i],collapse="")
    }
    for(i in 1: length(code_sender)){
        diff <- code_length-nchar(code_sender[i])
        if(is.na(code_sender[i])) next
        else if(diff>0) dataset$code_sender[i] <- paste0(paste0(rep("0",times=diff),collapse=""),code_sender[i],collapse="")
    }
    return(dataset)
}

formatReason <- function(dataset){
    reason_list <- strsplit(dataset$reason,"#")
    reason_list_pure <- sapply(reason_list, gsub, pattern="(^[ ]*|[ ]*$|\"|\\.$)", replacement="")
    reason_list_pure <- sapply(reason_list_pure,function(set){return(set[set!=","&set!=""])})
    dataset$reason <- sapply(reason_list_pure, function(list_pure){return(paste0(list_pure,collapse=" "))})
    return(dataset)
}

formatName <- function(dataset){
    dataset$name_sender <- gsub("\\.|,"," ",dataset$name_sender)
    dataset$name_sender <- gsub("\"|\\.$","",dataset$name_sender)
    dataset$name_sender <- gsub("[ ]+"," ",dataset$name_sender)
    dataset$name_sender <- tolower(dataset$name_sender)
    
    dataset$name_rec <- gsub("\\.|,"," ",dataset$name_rec)
    dataset$name_rec <- gsub("\"|\\.$","",dataset$name_rec)
    dataset$name_rec <- gsub("[ ]+"," ",dataset$name_rec)
    dataset$name_rec <- tolower(dataset$name_rec)
    return (dataset)
}

formatBank <- function(dataset){
    dataset$bank_sender <- gsub("\"|\\.$","",dataset$bank_sender)
    dataset$bank_rec <- gsub("\"|\\.$","",dataset$bank_rec)
    return (dataset)
}

validateCode <- function(dataset){
    u_code <- unique(c(dataset$code_sender,dataset$code_rec))
    u_code <- u_code[!is.na(u_code)]
    unsure <- character()
    for(code in u_code){
        temp <- 0
        type_one <- TRUE
        if (nchar(code)!=8) {
            unsure <- c(unsure,code)
            print(paste0("Check: ",code,collapse=""))
        } else {
            sequence <- as.numeric(unlist(strsplit(code,split="")))
            code_num <- as.numeric(code)
            if (code_num>30000000 && code_num<60000000) type_one <- FALSE
            
            if (type_one) temp <- sum(sequence*c(1,2,3,4,5,6,7,0))
            else temp <- sum(sequence*c(7,1,2,3,4,5,6,0))
            
            if(temp%%11==10) {
                if (type_one) temp <- sum(sequence*c(3,4,5,6,7,8,9,0))
                else temp <- sum(sequence*c(9,3,4,5,6,7,8,0))
                
                if(temp%%11!=sequence[8]) {
                    unsure <- c(unsure,code)
                    print(paste0("Fault?: ",code,collapse=""))
                }
            }
            else if(temp%%11!=sequence[8]) {
                unsure <- c(unsure,code)
                print(paste0("Fault?: ",code,collapse=""))
            }
        }
    }
    return (unsure)
}