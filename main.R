setwd("С:\\R_material") ## path to files 
source(".\\format.R")
source(".\\getData.R")
source(".\\plotData.R")
source(".\\analyse.R")

knu_get <- getData(spring = "knu",type = "get")
knu_send <- getData(spring = "knu",type = "send")

getUniqueNumTable(knu_get)
getUniqueNumTable(knu_send)

knu_get <- formatGeneral(knu_get)
knu_send <- formatGeneral(knu_send)

getUniqueNumTable(knu_get)
getUniqueNumTable(knu_send)

sort(unique(knu_get$name_rec))
sort(unique(knu_send$name_sender))

u_knu_get_name_rec <- sort(unique(knu_get$name_rec))

u_words_name_rec <- table(unlist(strsplit(unique(knu_get$name_rec),split=" ")))
u_words_name_rec_sub <- u_words_name_rec[u_words_name_rec>1]
sort(u_words_name_rec_sub)
#u_words_name_rec_sub
u_words_name_rec_sub <- u_words_name_rec_sub[-3]
u_words_name_rec_sub_names <- names(u_words_name_rec_sub[order(nchar(names(u_words_name_rec_sub)),decreasing = TRUE)])
u_words_name_rec_sub_names
junk <- lapply(u_words_name_rec_sub_names,function(s){ u_knu_get_name_rec <<- gsub(paste0("\\b",s,"\\b",collapse = ""),"",u_knu_get_name_rec)})
u_knu_get_name_rec
u_knu_get_name_rec <- gsub("^[ ]+|[ ]+$","",u_knu_get_name_rec)
u_knu_get_name_rec <- gsub("[ ]+"," ",u_knu_get_name_rec)
sort(unique(u_knu_get_name_rec))
knu_get[grep("мон",knu_get$name_rec),]
knu_get[grep("приватбанк",knu_get$name_rec),]

knu_get_inner_code <- getInternal(knu_get)
knu_send_inner_code <- getInternal(knu_send)
knu_get_inner_code <- knu_get_inner_code[order(knu_get_inner_code$transaction),]
knu_send_inner_code <- knu_send_inner_code[order(knu_send_inner_code$transaction),]

all(knu_get_inner_code == knu_send_inner_code)

plotSumOnTransaction(knu_get_inner_code)
plotSumOnTransaction(knu_get_inner_code[knu_get_inner_code$sum>100000,])

#sort(sub("x+[x -]+[.0-9 ]*","",knu_send_inner_code$reason))

knu_get_complete <- knu_get[complete.cases(knu_get),]
knu_get_incomplete <- knu_get[!complete.cases(knu_get),]
knu_send_complete <- knu_send[complete.cases(knu_send),] 
knu_send_incomplete <- knu_send[!complete.cases(knu_send),]

knu_get_invalid <- validateCode(knu_get_complete)
knu_send_invalid <- validateCode(knu_send_complete)

knu_get_outer_code <- getExternal(knu_get_complete)
knu_send_outer_code <- getExternal(knu_send_complete)

knu_send_rec_table <- getTableSumOnName(knu_send_outer_code,type="rec")
plotSumOnName(knu_send_rec_table,start=13)
plotAverageOnName(knu_send_rec_table,start=13)
plotTimesOnName(knu_send_rec_table,start=13)
