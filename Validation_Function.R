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
           select=c("Char_Name","CASNumber","act_id","SampleStartDate","SampleStartTime","Result","Result_Unit","MDLValue","MDLUnit","MRLValue","MRLUnit","QL","QL_Unit","Result_Type","Result_Comment","issue")))}
    
  }
  
  #2. Dissolved vs Total/Total Recoverable
  dvst<-function(x){
    y<-subset(x,x$Sample_Fraction %in% c("Total Recoverable","Dissolved","Total"))
    
    #create new identifier out of activity id and characteristic name
    y$comb<-paste0(y$act_id,",",y$Char_Name)
    
    #get unique identifiers, put into new dataset
    new<-unique(subset(y,select=c("act_id","Char_Name","comb")))
    
    #select all dissolved
    dis<-subset(y,y$Sample_Fraction %in% "Dissolved")
    
    #select all total recoverable/total
    tot<-subset(y,y$Sample_Fraction %in% c("Total Recoverable","Total"))
    
    #add dissolved and total columns to new dataset
    new$dissolved<-dis$Result_Numeric[match(new$comb,dis$comb,nomatch=NA)]
    new$total<-tot$Result_Numeric[match(new$comb,tot$comb,nomatch=NA)]
    
    #add MDL value (take lower value if one is lower than the other (makes for more conservative test))
    new$totMDL<-tot$MDLValue[match(new$comb,tot$comb,nomatch=NA)]
    new$disMDL<-dis$MDLValue[match(new$comb,dis$comb,nomatch=NA)]
    new$MDL<-ifelse(new$totMDL<new$disMDL,new$totMDL,new$disMDL)
    
    #calculate the difference
    new$diff<-new$total-new$dissolved
    
    #calculate the RPD
    new$RPD<-((new$dissolved-new$total)/2)*100
    
    #if difference is negative, and if it is larger than the MDL, then it is an issue
    issues<-subset(new,new$diff<0 & abs(new$diff)>new$MDL)
    cont<-merge(issues,y, by="comb")
    
    final<-subset(cont,select=c("act_id.x","Char_Name.x","Sample_Fraction","Result_Numeric","MRLValue","MDLValue","Result_Unit","diff","RPD","SampleStartDate",
                                "SampleStartTime","OrganizationID","MLocID","Project1","Result_status","Result_Comment"))
    
    return(final)
  }
  
  
  #3. Method Checks- get list of methods used and list of characteristics
  #will want to significantly expand this to be able to check against the CFR
  mc<-function(x){
    #x is dataset
    require(openxlsx)
    require(dplyr)
   
    
    #read in AWQMS-compatible CFR method list- NOTE THAT THIS LIST IS NOT EXHAUSTIVE YET-BULDING AS WE GO
    cfr<-read.xlsx("3_13_2019_CFR_Methods.xlsx",colNames=TRUE,na.string="NA")
    
    #join datasets on characteristic name
    x<-left_join(x,cfr,by=c("Char_Name","Method_Code","Method_Context"))
    
    
    
    
    #note that EPA method 625 and 624 were valid CFR methods up until the 2017 revision of CFR 136, where they were replaced with 
    #625.1 and 624.1, make sure this is noted in the code so we don't flag pre 2017 data as out of compliance when it was actually fine
    x$CFR_Method<-ifelse(x$Method_Code %in% c("624","625","608") & x$SampleStartDate<"2017-09-27",
                         paste0("Prior to 2017 CFR method change, Method is CFR approved"),
                         x$CFR_Method)
    
    x$CFR_Method<-ifelse(x$Method_Code %in% c("624","625","608") & x$SampleStartDate>"2017-09-27",
                         paste0("Method Changed 9/27/2017, update needed"),
                         x$CFR_Method)
    
    #no methods for tributyl phosphate, salinity, Demeton (8065-48-3) in CFR 136
    x$CFR_Method<-ifelse(x$Char_Name %in% c("Tributyl phosphate","Salinity","Demeton"),
                     paste0("No CFR method specified for characteristic"),
                     x$CFR_Method)
    
    x$CFR_Method<-ifelse(is.na(x$CFR_Method),
                         paste0("No, check CFR 136 for appropriate methods"),
                         x$CFR_Method)
    
    #return method results
    sub<-subset(x, CFR_Method %in% c("No, check CFR 136 for appropriate methods","Method Changed 9/27/2017, update needed"),
                select=c(CASNumber.x,Char_Name,Method_Code,Method_Context,CFR_Method,SampleStartDate))
    sub<-unique(sub)
    
    return(sub)
  }
  
  rej<-function(x){
    #x is dataset
    x<-namefrac(x)
    sub<-subset(x,Result_status %in% c("Rejected"),
                select=c(MLocID,act_id,SampleStartDate,SampleStartTime,Char_Name,Char_Speciation,
                         CASNumber,Result,Result_Unit,Method_Code,Result_Comment,Result_status))
  }
  


#x<-NPDES_AWQMS_Qry(startdate = "2015-01-01", enddate = "2019-01-01" , org = "GP-WM", HUC8 = NULL, HUC8_Name = NULL,reject=TRUE)
