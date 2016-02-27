## type : "get", "send"
## spring : "knu", "kpi", "nau", "mon"

## knu_get <- getData("get","knu")
## knu_get <- formatReason(knu_get)
## knu_get <- formatName(knu_get)

getData <- function(spring, type="send"){
    path <- paste0(getwd(),"/",collapse="")
    files <- list.files(path)
    relevant <- grep(paste0("^",spring,"_",type), files)
    dataset <- data.frame()
    for (file in files[relevant]) {
        dataset <- rbind(dataset,as.data.frame(read.csv(paste0(path, file),sep=";",colClasses = "character"),stringsAsFactors = FALSE))
    }
    names(dataset) <- c("transaction","date","code_sender","name_sender","mfo_sender","bank_sender",
                        "code_rec","name_rec","mfo_rec","bank_rec","sum","reason")
    dataset$date <- as.Date(dataset$date,format="%d.%m.%Y")
    dataset$sum <- as.numeric(gsub(",",".",dataset$sum))
    return (dataset)
}

subsetData <- function(dataset, what){   ## code, name, mfo, bank
    sender <- eval(parse(text=paste0("dataset$",what,"_sender")))
    rec <- eval(parse(text=paste0("dataset$",what,"_rec")))
    u_sender <- unique(sender)
    u_rec <- unique(rec)
    return (list(sender=sender,rec=rec,u_sender=u_sender,u_rec=u_rec))
}

getInternal <- function(dataset, what="code"){
    subset <- subsetData(dataset, what)
    if(length(subset$u_sender)==1) return (dataset[!is.na(subset$rec) & subset$rec==subset$u_sender,])
    else if (length(subset$u_rec)==1) return (dataset[!is.na(subset$sender) & subset$sender==subset$u_rec,])
    else {
        print(paste0("more than one unique ",what,", return all"))
        return (dataset)
    }
}

getExternal <- function(dataset, what="code"){
    subset <- subsetData(dataset, what)
    if(length(subset$u_sender)==1) return (dataset[subset$rec!=subset$u_sender,])
    else if (length(subset$u_rec)==1) return (dataset[subset$sender!=subset$u_rec,]) 
    else {
        print(paste0("more than one unique ",what,", return all"))
        return (dataset)
    }
}