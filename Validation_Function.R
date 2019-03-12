# code to validate NPDES data
#check data in AWQMS through data pull function
#output an excel sheet that contains validation results

#having issues with factors, convert to numeric
#function to do so
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

  #x is dataset
  #checks to run

  #1. QL checks
  ql<-function(x){
    require(openxlsx)
    require(dplyr)
    source("//deqlab1/abrits/Permit Job/R_Scripts/ShinyNPDES_AWQMS/NameandFraction.R")
    #subset just parameter, cas, and MDL, MRL, and units, only take unique values
    #x<-subset(x,select=c("Char_Name","Sample_Fraction","CASNumber","MDLValue","MDLUnit","MRLValue","MRLUnit"))
    
    #read in table with QLs
    qls<-read.xlsx("2_21_19_QLs.xlsx",colNames=TRUE,na.string="NA")
    
    #need to do unit conversions to compare data and ql list, all need to be in ug/l, except for Alkalinity, which should be in mg/L
    #checked AWQMS database, all alkalinity is reported in mg/l or mg/l CaCO3, no need for conversion
    #get list of char names 
    names<-unique(x$Char_Name)
    #remove alkalinity, that needs to stay as mg/l
    names<-names[names !="Alkalinity, total"]
    
    x<-unit_conv(x,names,"mg/l","ug/l")
    x<-unit_conv(x,names,"ng/l","ug/l")
    
    #do a join of qls and x based on Char_Name (need to combine metals and fractions)
    x<-namefrac(x)
    
    x<-left_join(x,qls,by="Char_Name")
    
    #throw warning if unit mismatch, else find the rows where MRL is greater than QL and return them
    
    if (any(!(is.na(x$QL_Unit)) & x$MDLUnit!=x$QL_Unit)) {return("Warning: Unit Mismatch")}
    
    else {x$issue<-ifelse(
      x$MRLValue>x$QL & x$Result_Numeric<x$MRLValue,
      "QL not met, Result below QL",
      NA)
    
    return(subset(x,!(is.na(issue)),
           select=c("Char_Name","CASNumber","act_id","Result","Result_Unit","MDLValue","MDLUnit","MRLValue","MRLUnit","QL","QL_Unit","issue")))}
    
  }
  
  #2. Dissolved vs Total/Total Recoverable
  dvst<-function(x){
    y<-subset(x,x$Sample_Fraction %in% c("Total Recoverable","Dissolved","Total"),
              select=c("Char_Name","Sample_Fraction","Result_Numeric","MRLValue","MDLValue","Result_Unit","SampleStartDate",
                       "SampleStartTime","OrganizationID","MLocID","Project1","act_id"))
    
    #create new identifier out of activity id and characteristic name
    y$comb<-paste0(y$act_id,",",y$Char_Name)
    
    #get unique identifies, put into new dataset
    new<-unique(subset(y,select=c("act_id","Char_Name","comb")))
    
    #select all dissolved
    dis<-subset(y,y$Sample_Fraction %in% "Dissolved")
    
    #select all total recoverable/total
    tot<-subset(y,y$Sample_Fraction %in% c("Total Recoverable","Total"))
    
    #add dissolved and total columns to new dataset
    new$dissolved<-dis$Result_Numeric[match(new$comb,dis$comb,nomatch=NA)]
    new$total<-tot$Result_Numeric[match(new$comb,tot$comb,nomatch=NA)]
    
    #calculate the difference
    new$diff<-new$total-new$dissolved
    
    #if difference is negative, and if it is larger than the MDL, then it is an issue
    issues<-subset(new,new$diff<0 & abs(new$diff)>new$MDLValue)
    cont<-merge(issues,y, by="comb")
    
    final<-subset(cont,select=c("act_id.x","Char_Name.x","Sample_Fraction","Result_Numeric","MRLValue","MDLValue","Result_Unit","diff","SampleStartDate",
                                "SampleStartTime","OrganizationID","MLocID","Project1"))
    
    return(final)
  }
  
  
  #3. Method Checks- get list of methods used and list of characteristics
  mc<-function(x){
    #x is dataset
    sub<-unique(subset(x,select=c(CASNumber,Char_Name,Method_Code,Method_Context)))
    sub<-sub[order(sub$Method_Code),]
  }
  


#x<-NPDES_AWQMS_Qry(startdate = "2000-01-01", enddate = "2019-01-01" , org = "GP-WM", HUC8 = NULL, HUC8_Name = NULL,reject=FALSE)
