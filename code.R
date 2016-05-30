## Split by area file created
cust_mast <- fread("customer_master.csv" , stringsAsFactors= TRUE)
sp <- split(cust_mast,as.factor(cust_mast$station))
names(sp)
length(cust_mast$custcode)
cust_Bori <- rbindlist(sp[c(5:6,41:161)])
cust_dahi <- rbindlist(sp[169:272])
cust_kandi <- rbindlist(sp[284:400])
cust_r <- rbindlist(sp[c(7:40,162:168,401:541)])
cust_r1 <- rbindlist(sp[1:4])
write.csv(cust_r1, "check.csv")
spl <- split(cust_r1, as.factor(cust_r1$area))


cust_r1_b <- cust_r1[grepl("*bori*",cust_r1$area, ignore.case= TRUE),]
cust_r1_k <- cust_r1[grepl("*kandi*",cust_r1$area, ignore.case= TRUE),]
cust_r1_d <- cust_r1[grepl("*dahi*",cust_r1$area, ignore.case= TRUE),]
cust_r1_rem2 <- cust_r1[!grepl("*dahi|kandi|bori*",cust_r1$area, ignore.case= TRUE),]

cust_bori_T <- rbind(cust_Bori,cust_r1_b)
write.csv(cust_bori_T, "borivali_customer.csv")
cust_kandi_T <-rbind(cust_kandi, cust_r1_k)
write.csv(cust_kandi_T, "kandivali_customer.csv")
cust_dahi_T <-rbind(cust_dahi, cust_r1_d)
write.csv(cust_dahi_T, "dahisar_customer.csv")
cust_other_T <- rbind(cust_r,cust_r1_rem2)
cust_other_T <- cust_other_T[!grepl("*bori|kandi|dahi*",cust_other_T$station,ignore.case=TRUE),]
cust_other_T <- cust_other_T[!grepl("*bori|kandi|dahi*",cust_other_T$area,ignore.case=TRUE),]
#cust_other_T <- cust_other_T[!grepl("*bori|kandi|dahi*",cust_other_T$add3,ignore.case=TRUE),]
op <- read.csv("other_customer.csv")
length(cust_bori_T$custcode)+length(cust_dahi_T$custcode)+length(cust_kandi_T$custcode)+length(cust_other_T$custcode)
t <-subset(cust_mast,cust_mast$custcode %in% c(cust_dahi_T$custcode,cust_bori_T$custcode,cust_kandi_T$custcode,cust_other_T$custcode)==FALSE)
ww <- t <-subset(cust_mast,cust_mast$custcode %in% c(cust_dahi_T$custcode,cust_bori_T$custcode,cust_kandi_T$custcode,cust_other_T$custcode)==FALSE)
cust_other_T <- rbind(cust_other_T, t)
write.csv(cust_other_T, "other_customer.csv")




## best customers on the basis of time (here borivali considered) 
##OUTPUT IS A LIST OF BEST CUSTOMERS AND THEIR TIME
library(data.table)
borivali_cust <- fread("borivali_customer.csv")
dob <- borivali_cust$create_datetime
updt <- borivali_cust$update_datetime
dob <- ymd_hms(borivali_cust$create_datetime)
updt <- ymd_hms(borivali_cust$update_datetime)
cust_period <- updt-dob
units(cust_period) <- "days"
cust_period <- round(cust_period,4)
cust_period <- as.numeric(as.character(cust_period))
f <- cbind(borivali_cust,cust_period)
f <- f[order(f$cust_period,decreasing=TRUE),]
best_customers_borivali <- head(f, n=11)   # top 11 printed



## building division file created   BORIVALI CONSIDERED
library(data.table)
r <- fread("borivali_customer.csv")
length(r$building)
length(unique(r$building))
library(plyr)
r <- as.data.frame(r)
build <- tapply(r$building,(r$building),length)  
g <- as.data.frame(build)
write.csv(g,"hmmm.csv")
k <- read.csv("hmmm.csv")
names(k)=c("name","no")
for( i in seq(along=2:length(k$name))){
  if(identical(substr(k$name[i],1,4),substr(k$name[i-1],1,4))){
    k$name[i]=k$name[i-1]
  }
}
zx <- aggregate(no~name,k,sum)
write.csv(zx,"building_no_of_cust.csv")



## number of cust per date GRAPH OUTPUT
library(data.table)
r_order <- fread("orders.csv")
order_date <- r_order$order_date
order_date <- gsub(" ","",order_date)
order_date <- substr(order_date,1,10)
cust_no <- tapply(order_date, (order_date), length)
#date <- unique(order_date)
cust_no <-unname(cust_no, force=TRUE)
cust_no <- as.data.frame(cust_no) 
date <- as.Date(unique(order_date))
cust_date <- data.frame(cbind(date,cust_no,month(date),year(date),mday(date),yday(date)))
names(cust_date) <- c("date","customers","month","year","month_day","year_day")
library(ggplot2)
g <-ggplot(cust_date,aes(date,customers))+geom_point()+geom_smooth(method="lm",lwd=1)+labs(title="NUMBER OF CUSTOMERS EVERY YEAR")



library(data.table)
r <- fread("order_detail.csv")
#head(r)
item <- data.frame(r$item_code,r$qty,row.names=NULL)
names(item) <- c("item","qty")
library(plyr)
item_no <- aggregate(qty~item,item,sum)
item_no <- item_no[order(item_no$qty,decreasing=TRUE,na.last=TRUE),]
write.csv(item_no,"rrrrr.csv")
r_master <- fread("item_master.csv")
r_master <- setkey(r_master,item_code)
#head(r_master)
group <- data.frame(r_master$item_code,r_master$item_group_code)
names(group) <- c("item","group")
item_2 <- merge(item_no,group,all.x=TRUE, all.y=TRUE)
r_desc <- fread("item_group.csv")
group_desc <-data.frame(r_desc$item_group_code,r_desc$item_group_desc)
names(group_desc) <- c("group","description")
item_3 <- merge(item_2,group_desc,all.x=TRUE, all.y=TRUE)
#head(item_3)
item_3 <- item_3[order(item_3$qty,decreasing=TRUE),]
#head(item_3)
write.csv(item_3,"items_by_groups.csv")


## item with time WHOLE DATA output is PLOT/GRAPH
library(ggplot2)
library(data.table)
library(gridExtra)
setwd("C:\\Users\\ANANYA\\Downloads\\coursera\\pune\\Hiren\\customer_master")
data <- fread("complete_item.csv")
#head(data)
item_time<- NULL
item_time <- function(name){
  item <- data[data$item_code==name,]
  group_code <- unique(item$item_group)
  item_g <- data[data$item_group==group_code,]
  #length(item$month)
  if(length(item$month)!=0){
    group_month <- aggregate(qty~month+year,data=item_g,sum)
    item_month <- aggregate(qty~month+year,data=item,sum)
    #png("specified_item_with_group_time.png",width=1000,height=600)
    par(mfrow=c(2,1))
    g1 <-ggplot(data=item_month, aes(x=month,y=qty))+geom_point()+geom_line()+facet_grid(.~year)+labs(x="Month",y=("Item Quantity"),title=name)  
    g2 <-ggplot(data=group_month, aes(x=month,y=qty))+geom_point()+geom_line()+facet_grid(.~year)+labs(x="Month",y=("Group Quantity"),title=item_g$description)   
    grid.arrange(g1,g2,nrow=2,main="ITEM VS GROUP")
    #dev.off()
  }
  else{
    print("data insufficient")
  }
}
#item_time("IT683")



##item with time/qty BY AREA......Borivali considered
# OUTPUT IS GRAPH OR LIST DEPENDING ON FUNCTION
setwd("C:\\Users\\ANANYA\\Downloads\\coursera\\pune\\Hiren\\customer_master")
l <- list.files(pattern="*.csv")
l <-l[c(1,3,7)]
library(data.table)
read <- lapply(l,fread)
r_bori <- read[[1]]
r_order <- read[[3]]
r_item <- read[[2]]
# to separate area of interest from the whole data
bori_cust <- r_bori$custcode
bori_number <- r_order[r_order$custcode %in% bori_cust,]
bori_number <- bori_number$order_num
bori_item <- r_item[r_item$order_num %in% bori_number,]       #IIII
#names(bori_item)
#item_time_area("IT5555")
item_time_area<- NULL
#Function for area item with time.....input is the item code
item_time_area <- function(name){
  item <- bori_item[bori_item$item_code==name,]
  group_code <- unique(item$item_group)
  item_g <- bori_item[bori_item$item_group==group_code,]
  #length(item$month)
  if(length(item$month)!=0){
    group_month <- aggregate(qty~month+year,data=item_g,sum)
    item_month <- aggregate(qty~month+year,data=item,sum)
    png("areawise_item_group_time2.png",height=600,width=1000)
    par(mfrow=c(2,1))
    g1 <-ggplot(data=item_month, aes(x=month,y=qty))+geom_point()+geom_line()+facet_grid(.~year)+labs(x="Month",y=("Item Quantity"),title=name)  
    g2 <-ggplot(data=group_month, aes(x=month,y=qty))+geom_point()+geom_line()+facet_grid(.~year)+labs(x="Month",y=("Group Quantity"),title=item_g$description)   
    grid.arrange(g1,g2,nrow=2,main="ITEM VS GROUP")
    dev.off()
  }
  else{
    print("data insufficient")
  }
}
#item_time_area("IT5555")
#function for item qty sold high low per area..... input is area data as in IIII above
# Input 2 is the year for which the data required, input 3 is
# whether low values(1) are required or high values required
x <- NULL
item_qty_area <- function(data,year,k){
  rank <- aggregate(qty~item_code+month+year+item_group+description,data=data,sum)
  ra <-rank[order(rank$year,rank$month,rank$qty),]    #items ordered according to sales
  sp_year <-split(ra,ra$year)   ##year_wise split
  high<- NULL
  low <- NULL
  code_low= NULL
  code_high <- NULL
  year<- year-2009
  if(k==1){
    code_low <- data.frame(tapply(sp_year[[year]]$item_code,sp_year[[year]]$month,head))
    qty_low <- data.frame(tapply(sp_year[[year]]$qty,sp_year[[year]]$month,head))
    low <- cbind(code_low,qty_low)
    names(low) <- c("item_code","quantity sold")
    View(low)
  }
  else{
    code_high <- data.frame(tapply(sp_year[[year]]$item_code,sp_year[[year]]$month,tail))  
    qty_high <- data.frame(tapply(sp_year[[year]]$qty,sp_year[[year]]$month,tail))  
    high <- cbind(code_high,qty_high)
    names(high) <- c("high_item_code","quantity sold")
    View(high)
  }
}
item_qty_area(bori_item,2011,3) #1 for low.....other numbers for high 



## high low item qty by month/yr WHOLE AREA
#OUTPUT IS LIST BY YEAR....1,2,3,4,5,6 specifying 2010....2015
library(data.table)
setwd("C:\\Users\\ANANYA\\Downloads\\coursera\\pune\\Hiren\\customer_master")
dat <- fread("complete_item.csv")
rank <- aggregate(qty~item_code+month+year+item_group+description,data=dat,sum)
head(rank)
ra <-rank[order(rank$year,rank$month,rank$qty),]    #items ordered according to sales
sp_year <-split(ra,ra$year)   ##year_wise split
high<- NULL
low <- NULL
code_low= NULL
code_high <- NULL
low_item <- NULL
low_qty <- NULL
high_qty <- NULL
high_item <- NULL
for(i in seq(along=1:6)){
  code_low <- data.frame(tapply(sp_year[[i]]$item_code,sp_year[[i]]$month,head))
  low_item[[i]] <- code_low
  qty_low <- data.frame(tapply(sp_year[[i]]$qty,sp_year[[i]]$month,head))
  low_qty[[i]]<- qty_low
  low[[i]] <- cbind(low_item[[i]],low_qty[[i]])
  names(low[[i]]) <- c("item_code","quantity sold")
  code_high <- data.frame(tapply(sp_year[[i]]$item_code,sp_year[[i]]$month,tail))  
  high_item[[i]] <- code_high
  qty_high <- data.frame(tapply(sp_year[[i]]$qty,sp_year[[i]]$month,tail))  
  high_qty[[i]] <- qty_high
  high[[i]] <- cbind(high_item[[i]],high_qty[[i]])
  names(high[[i]]) <- c("high_item_code","quantity sold")
}
names(high) <- c("2010","2011","2012","2013","2014","2015")
names(low) <- c("2010","2011","2012","2013","2014","2015")

#View(low[[1]])















