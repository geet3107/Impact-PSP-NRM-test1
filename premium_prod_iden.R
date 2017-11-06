options(scipen = 999)
library(dplyr)
library(stringr)

dimprod <- read.csv("/home/ashish/data/in/DimProduct.csv", quote="",colClasses= "character")
dimprice <- read.csv("/home/ashish/data/in/DimPrice.csv",quote="",colClasses= "character")
#merging dimproduct and dimprice
merged<-merge(dimprod,dimprice,by.x="ItemId",by.y="ItemID",all.x=TRUE)
merged$ItemId<-as.numeric(merged$ItemId)
merged$ItemId<-as.character(merged$ItemId)
merged<-na.omit(merged)

#combining the columns Department,Sub-Department,class,Sub-class
merged$DSCS<-paste(merged$Department,merged$SubDepartment,merged$Class,merged$SubClass,sep = "_")
merged$DSC<-paste(merged$Department,merged$SubDepartment,merged$Class,sep = "_")
merged$DS<-paste(merged$Department,merged$SubDepartment,sep = "_")

#removing the special characters from within the string
merged$DSCS <- gsub("\\ <","", merged$DSCS)
merged$DSCS <- gsub("\\ >","", merged$DSCS)
merged$DSC <- gsub("\\ <","", merged$DSC)
merged$DSC <- gsub("\\ >","", merged$DSC)
merged$DS <- gsub("\\ <","", merged$DS)
merged$DS <- gsub("\\ >","", merged$DS)
merged$DSCS<-str_replace_all(merged$DSCS, "[[:punct:]]", " ")
merged$DSC<-str_replace_all(merged$DSC, "[[:punct:]]", " ")
merged$DS<-str_replace_all(merged$DS, "[[:punct:]]", " ")
merged$RetailMeasure<-toupper(merged$RetailMeasure)
Standard_measures<-c("EA", "CT", "LB","OZ","GM","GL","GAL","FT","LT", "ML","QT","CI","CC","lB","KG","PK","PT","MG","OZ","L")    
merged$RetailMeasure_revised<- ifelse(merged$RetailMeasure %in% Standard_measures,merged$RetailMeasure,"EA")
Length<-c("FT")
Volume<-c("CI")
Liquid<-c("CC","GAL","GL","L","LT","ML","PK","PK","PT","PT","QT")
Quantity<-c("CT","EA")
Weight<-c("GM","KG","LB","MG","OZ")
merged$measure_type <- 0
merged$measure_type[merged$RetailMeasure_revised %in% Length ] <- "Length"
merged$measure_type[merged$RetailMeasure_revised %in% Liquid ] <- "Liquid"
merged$measure_type[merged$RetailMeasure_revised %in% Quantity ] <- "Quantity"
merged$measure_type[merged$RetailMeasure_revised %in% Weight ] <- "Weight"
merged$measure_type[merged$RetailMeasure_revised %in% Volume ] <- "Volume"

##########Add code for conversion########################
Conversion_units <- read.csv("~/Product_category_identification/Conversion_units.csv")
names(Conversion_units)[1]<-'RetailMeasure'
Conversion_units$`Count of ItemId`->NULL
Conversion_units$Expansion->NULL
Conversion_units$Type->NULL

Conversion_units$RetailMeasure<-toupper(Conversion_units$RetailMeasure)

Merged_conversion<-merge(merged,Conversion_units,by.x="RetailMeasure_revised",by.y="RetailMeasure",all.x=TRUE)

Merged_conversion$RetailUnit<-as.numeric(Merged_conversion$RetailUnit)

Merged_conversion$RetailUnit_New<-(Merged_conversion$RetailUnit)*(Merged_conversion$Conversion)

#################Group Identification##########################
Premium_segment_identification <- read.csv("~/Product_category_identification/Premium_segment_identification.csv")
Premium_segment_identification$DSCS <- gsub("\\ <","", Premium_segment_identification$DSCS)
Premium_segment_identification$DSCS <- gsub("\\ >","", Premium_segment_identification$DSCS)
Premium_segment_identification$DSCS<-str_replace_all(Premium_segment_identification$DSCS, "[[:punct:]]", " ")

Merged_conversion_grouped<-merge(Merged_conversion,Premium_segment_identification,by.x="DSCS",by.y="DSCS",all.x=TRUE)

######################################
merged<-Merged_conversion_grouped
#coverting base_retail for perunit quantity
merged$RetailUnit_New[is.na(merged$RetailUnit_New)] = 1
merged[is.na(merged)]<-0
merged$Base.Retail<-as.numeric(merged$Base.Retail)
merged$RetailUnit_New<- as.numeric(merged$RetailUnit_New)
merged$Base_retail_pu<- (merged$Base.Retail)/(merged$RetailUnit_New)

#Grouping by DSCS 
# extract<-merged[,c(1,51,46,49,50)]
extract<-merged
extract$Base_retail_pu<-as.numeric(extract$Base_retail_pu)
sum(is.na(extract$Base_retail_pu))

DSCS<-group_by(extract,Group,measure_type)
DSCS<-summarise(DSCS,count_of_itemid=length(ItemId),mean=mean(Base_retail_pu,na.rm=T),min=min(Base_retail_pu,na.rm=T),max=max(Base_retail_pu,na.rm=T),median=median(Base_retail_pu,na.rm=T),sdev=sd(Base_retail_pu,na.rm=T))
DSCS$mean_sd<- (DSCS$mean)+(DSCS$sdev)
DSCS_fin<- DSCS

######Mapping back#############
extract_fin<-merge(extract,DSCS_fin,by.x = c("Group","measure_type"),by.y = c("Group","measure_type"),all.x = T)
extract_fin$loop_tag<-paste(extract_fin$Group,extract_fin$measure_type,sep = "_")
# Premium tagging "1"
tag_for_loop<- unique(extract_fin$loop_tag)
rm(collated1)
for (i in 1:length(tag_for_loop)) {
  item_data<-subset(extract_fin,extract_fin$loop_tag==tag_for_loop[i])
  item_data_2 = item_data[order(item_data$Base_retail_pu), ]
  nrow_take<- round(0.2*nrow(item_data_2),0)
  item_data<-tail(item_data_2,nrow_take)
  collated1 <- rbind(if(exists("collated1")) collated1,item_data)
  cat("Done-", i,"of" ,length(tag_for_loop), "\n")
  
  
}

collated1$Premium<-1
collated1<-collated1[,c("ItemId","Premium")]
extract_fin<-merge(extract_fin,collated1,by.x = c("ItemId"),by.y = c("ItemId"),all.x = T)
extract_fin$Premium[is.na(extract_fin$Premium)] = 0
##################################
# extract_fin$sdev[is.na(extract_fin$sdev)] = 0
# extract_fin$Premium<- ifelse(extract_fin$Base_retail_pu>= extract_fin$mean_sd | (extract_fin$sdev==0 & grep(' Treats', extract_fin$DSCS) >0),1,0)
# extract_fin$Premium<-as.character(extract_fin$Premium)
# extract_fin$Premium[is.na(extract_fin$Premium)] = 0
# extract_fin$Premium<-as.numeric(extract_fin$Premium)
# res<- extract_fin[is.na(extract_fin$Premium),]
write.csv(extract_fin,"extract_fin.csv",row.names = F)
extract_fin1<-extract_fin[,c("ItemId","Premium")]
############## uploading on bigquery #############
library(devtools)
#install.packages('devtools') 
#devtools::install_github("rstats-db/bigrquery")
library(bigrquery)
project <- "psp-nrm"
dataset = "Customer_segmentation_Geetanjali"

insert_upload_job(project, dataset, "Product_category", extract_fin1, write_disposition = "WRITE_TRUNCATE")

##### Mail

library(mailR)
sender <- "pspsupport@impactanalytics.co"
#recipients <- c("manikanta.talluri@impactanalytics.co")
email_body <- paste("Hi Team,
                    
                    Product classification for Customer-segmentation is done , refer to - https://bigquery.cloud.google.com/table/psp-nrm:Customer_segmentation_Geetanjali.Product_category?pli=1&tab=preview")
recipients <- c("Impact-NRM@impactanalytics.co")
send.mail(from = sender,
          to = recipients,
          subject = "PSP-NRM:Product classification for Customer-segmentation",
          body = email_body,
          smtp = list(host.name = "smtp.gmail.com", port = 465,
                      user.name = sender,
                      passwd = "uctibl3g80a9mia", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)

