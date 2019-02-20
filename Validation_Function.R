## code to validate NPDES data
#check data in AWQMS through data pull function
#output an excel sheet that contains validation results


RPA_Validate<-function(x) {
  #x is dataset
  #checks to run
  
  #1. QL checks
 # ql<-
  
  #2. Dissolved vs Total/Total Recoverable
  dvst<-function(x){
    y<-subset(x,x$Sample_Fraction %in% c("Total Recoverable","Dissolved","Total"),
              select=c("Char_Name","Sample_Fraction","Result_Numeric","SampleStartDate",
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
    
    issues<-subset(new,new$diff<0)
    cont<-merge(issues,y, by="comb")
    
    final<-subset(cont,select=c("act_id.x","Char_Name.x","Sample_Fraction","Result_Numeric","diff","SampleStartDate",
                                "SampleStartTime","OrganizationID","MLocID","Project1"))
    
    return(final)
  }
  
  
  #3. Method Checks- get list of methods used and list of characteristics
  mc<-function(x){
    #x is dataset
    sub<-unique(subset(x,select=c(CASNumber,Char_Name,Method_Code,Method_Context)))
    sub<-sub[order(sub$Method_Code),]
  }
  
}

x<-NPDES_AWQMS_Qry(startdate = "2000-01-01", enddate = "2019-01-01" , org = "GP-WM", HUC8 = NULL, HUC8_Name = NULL,reject=FALSE)
