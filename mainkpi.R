setwd("С:\\R_material_kpi") ## path to files 

#attach necessary libraries
source(".\\format.R")
source(".\\getData.R")
source(".\\plotData.R")
source(".\\analyse.R")

#load data 
kpi_get <- getData(spring = "kpi",type = "get")
kpi_send <- getData(spring = "kpi",type = "send")

#get number of unique values for each column 
getUniqueNumTable(kpi_get)
getUniqueNumTable(kpi_send)

#polish formatting
kpi_get <- formatGeneral(kpi_get)
kpi_send <- formatGeneral(kpi_send)

#get number of unique values for each column 
getUniqueNumTable(kpi_get)
getUniqueNumTable(kpi_send)

#get different names of KPI
sort(unique(kpi_get$name_rec))
sort(unique(kpi_send$name_sender))

#place additional space arround frequent words
u_kpi_get_name_rec <- sort(unique(kpi_get$name_rec))
u_kpi_get_name_rec <- gsub("(нтуу|кпі|ндч|україни)(?![ ])"," \\1 ", u_kpi_get_name_rec, perl = TRUE)

#split by space and fetch words which occur 2 or more times
u_words_name_rec <- table(unlist(strsplit(unique(u_kpi_get_name_rec),split=" ")))
u_words_name_rec_sub <- u_words_name_rec[u_words_name_rec>1]

#check them (pairs: word + number of occurences)
sort(u_words_name_rec_sub)

#take only words from mentioned pairs, sort them 
u_words_name_rec_sub_names <- names(u_words_name_rec_sub[order(nchar(names(u_words_name_rec_sub)),decreasing = TRUE)])
u_words_name_rec_sub_names

#delete frequent substrings from names of KPI
junk <- lapply(u_words_name_rec_sub_names,function(s){ u_kpi_get_name_rec <<- gsub(paste0("\\b",s,"\\b",collapse = ""),"",u_kpi_get_name_rec)})
u_kpi_get_name_rec

#get rid of brakets and excessive spaces
u_kpi_get_name_rec <- gsub("[()]","",u_kpi_get_name_rec)
u_kpi_get_name_rec <- gsub("^[ ]+|[ ]+$","",u_kpi_get_name_rec)
u_kpi_get_name_rec <- gsub("[ ]+"," ",u_kpi_get_name_rec)
sort(unique(u_kpi_get_name_rec))

#check suspicious transactions
kpi_get[grep("дебют",kpi_get$name_rec),]
kpi_send[kpi_send$code_rec=="39421842",]
apply(kpi_send[kpi_send$code_rec=="39421842",3:10],2,unique)
min(kpi_send[kpi_send$code_rec=="39421842",]$date)
kpi_send[2588,]

#fetch transactions between KPI & KPI
kpi_get_inner_code <- getInternal(kpi_get)
kpi_send_inner_code <- getInternal(kpi_send)
kpi_get_inner_code <- kpi_get_inner_code[order(kpi_get_inner_code$transaction),]
kpi_send_inner_code <- kpi_send_inner_code[order(kpi_send_inner_code$transaction),]
all(kpi_get_inner_code == kpi_send_inner_code)
all(kpi_get_inner_code == kpi_send_inner_code, na.rm = TRUE)

#separate complete from incomplete cases
kpi_get_complete <- kpi_get[complete.cases(kpi_get),]
kpi_get_incomplete <- kpi_get[!complete.cases(kpi_get),]
kpi_send_complete <- kpi_send[complete.cases(kpi_send),] 
kpi_send_incomplete <- kpi_send[!complete.cases(kpi_send),]

#validate edrpou codes
kpi_get_invalid <- validateCode(kpi_get_complete)
kpi_send_invalid <- validateCode(kpi_send_complete)

#check suspicious codes
kpi_send[kpi_send$code_rec=="xxxxxxxxxx",]
sum(kpi_send[kpi_send$code_rec=="xxxxxxxxxx",]$sum)
sort(table(kpi_send[kpi_send$code_rec=="xxxxxxxxxx",]$name_rec))
sum(kpi_send[kpi_send$code_rec=="xxxxxxxxxx"&kpi_send$name_rec=="фоп туровець п д ",]$sum)

#check other suspicious cases
kpi_send[kpi_send$code_rec=="568362439",]
kpi_send[kpi_send$code_rec=="200009888",]
dim(kpi_send[!is.na(kpi_send$bank_rec) & kpi_send$bank_rec=="Test Bank",])
sum(kpi_send[!is.na(kpi_send$bank_rec) & kpi_send$bank_rec=="Test Bank",]$sum)

#fetch every transaction where KPI is not at both sides
kpi_get_outer_code <- getExternal(kpi_get_complete)
kpi_send_outer_code <- getExternal(kpi_send_complete)

#construct table which contains name of money receiver, total sum, aerage sum on transaction and number of transactions
kpi_send_rec_table <- getTableSumOnName(kpi_send_outer_code,type="rec")

#plot graphs on each column in table
plotSumOnName(kpi_send_rec_table)
plotAverageOnName(kpi_send_rec_table)
plotTimesOnName(kpi_send_rec_table)

#see total number of money receivers
dim(kpi_send_rec_table)[1]

#reject rows related to banks of taxes
kpi_send_rec_table_sub <- kpi_send_rec_table[!grepl("дпі|дпi|дксу|банк$",row.names(kpi_send_rec_table)),]

#plot graphs on each column in table
plotSumOnName(kpi_send_rec_table_sub)
plotAverageOnName(kpi_send_rec_table_sub)
plotTimesOnName(kpi_send_rec_table_sub)

#reject all rows with kiev mentioned and all uncaught tax
kpi_send_rec_table_sub <- kpi_send_rec_table_sub[!grepl("дфс|київ",row.names(kpi_send_rec_table_sub)),]

#see total number of money receivers
dim(kpi_send_rec_table_sub)[1]

#plot graphs on each column in table
plotSumOnName(kpi_send_rec_table_sub)
plotAverageOnName(kpi_send_rec_table_sub)
plotTimesOnName(kpi_send_rec_table_sub)

#who ate kpi?
ruol <- kpi_send[!is.na(kpi_send$name_rec)&grepl("руол",kpi_send$name_rec),]
sum(ruol$sum)
apply(ruol[,3:10],2,unique)
summary(ruol$sum)

#and once again who ate kpi? 
ocean <- kpi_send[!is.na(kpi_send$name_rec)&grepl("океан",kpi_send$name_rec),]
sum(ocean$sum)
apply(ocean[,3:10],2,unique) #note: there is no bank receiver and edrpou of receiver - invalid
summary(ocean$sum)
