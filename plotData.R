suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(knitr))
plotNamesSenderEntries <- function(dataset){
    ggplot(dataset, aes(x=dataset$name_sender))+geom_bar()+coord_flip()
}

plotNamesRecEntries <- function(dataset){
    ggplot(dataset, aes(x=dataset$name_rec))+geom_bar()+coord_flip()
}

plotSumOnTransaction <- function(dataset){
    ggplot(dataset, aes(x=transaction, y=sum))+
        geom_bar(stat="identity")+
        scale_y_continuous(labels = scales::comma)+
        coord_flip()
}

plotSumOnName <- function(dataset,start=1,end=20){
    ggplot(dataset[start:end,], aes(x=row.names(dataset)[start:end], y=sum))+
        geom_bar(stat="identity")+
        theme(axis.text.x = element_text(size=13))+
        theme(axis.text.y = element_text(size=13))+
        scale_y_continuous(labels = scales::comma)+
        coord_flip()
}

plotAverageOnName <- function(dataset,start=1,end=20){
    ggplot(dataset[start:end,], aes(x=row.names(dataset)[start:end], y=average))+
        geom_bar(stat="identity")+
        theme(axis.text.x = element_text(size=13))+
        theme(axis.text.y = element_text(size=13))+
        scale_y_continuous(labels = scales::comma)+
        coord_flip()
}

plotTimesOnName <- function(dataset,start=1,end=20){
    ggplot(dataset[start:end,], aes(x=row.names(dataset)[start:end], y=times))+
        geom_bar(stat="identity")+
        theme(axis.text.x = element_text(size=13))+
        theme(axis.text.y = element_text(size=13))+
        scale_y_continuous(labels = scales::comma)+
        coord_flip()
}