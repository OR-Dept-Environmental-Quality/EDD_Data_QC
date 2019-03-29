
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
######This app is meant to help QC data submitted under EDD toxics by NPDES permittees


print("Initial data queries may take a few minutes.")

library(shiny)
library(AWQMSdata)
library(dplyr)
library(openxlsx)
library(shinybusy)
library(rmarkdown)



#attempt to turn off scientific notation
options(scipen=999)

#in case the shinybusy package needs to be installed/reinstalled
#remotes::install_github("dreamRs/shinybusy")

#Need to remake query, cannot use AWQMS_Data as it pulls out too much data for the app to work,
#plus, for NPDES only need a subset of data- 
source("E:/Permit Job/R_Scripts/ShinyNPDES_AWQMS/NPDES_AWQMSQuery.R")
#function for data validations
source("Validation_Function.R")



# Query out the valid values ---------------------------------------------
#only need date, org,station, and whether rejected or not for query


# Check to see if saved cache of data exists. If it does not, or is greater than
# 7 days old, query out stations and organizations and save the cache
if(!file.exists("query_cache.RData") | 
   difftime(Sys.Date() ,file.mtime("query_cache.RData") , units = c("days")) > 7){
  

#NPDES_AWQMS_Stations functions only pulls stations 
station <- NPDES_AWQMS_Stations()
station <- station$MLocID
station <- sort(station)

organization <- AWQMS_Orgs()
organization <- organization$OrganizationID
organization <- sort(organization)

save(station, organization, file = 'query_cache.RData')
} else {
  load("query_cache.RData")
}


# Define UI 
ui <- fluidPage(

   # Sidebar with parameter inputs

  sidebarLayout(
      sidebarPanel(
        #permittee name
        textInput("permittee",
                  label="Permit Number"),
        #permit #
        textInput("data_sub",
                  label="Data Submission"),
        # Add line
        tags$hr(),
        #Add break
        tags$br(),
        
        # Start Date (make start date six months ago)
        dateInput("startd",
                  label = "Select Start Date",
                  min = '1949-09-15',
                  value = Sys.Date()-182),
        
        # End date
        dateInput("endd",
                  label = "Select End Date",
                  min = '1900-1-1'),

       #Orgs
       selectizeInput("orgs",
                       "Select organization",
                       choices = organization,
                      multiple = TRUE),
                     
       # Monitoring locations 
       selectizeInput("monlocs",
                      "Select Monitoring Locations",
                      choices = station,
                      multiple = TRUE), 
       
       #add action button, idea is to not run query until the button is clicked)
       actionButton("goButton","Run Query"),
       #add an excel download button
       downloadButton('downloadData', 'Download Data'),
       #add Report download button
       downloadButton('report','Download PDF Report')
        ),


     # Setup main panel
       mainPanel(
        h1("EDD Toxics Data QC"),
        # Add line
        tags$hr(),
        #Add break
        tags$br(),
        
        #one tab for each QC
        tabsetPanel(
          #all data
          tabPanel("Data",dataTableOutput("table")),
          #quantitation limit issues
          tabPanel("Quantitation Limit Issues",
                   dataTableOutput("ql"),
                   textInput("comm1","Comments")),
          #Total vs dissolved
          tabPanel("Total vs Dissolved",
                   dataTableOutput("diff"),
                   textInput("comm2","Comments")),
          #CFR issues
          tabPanel("Methods",
                   dataTableOutput("methods"),
                   textInput("comm3","Comments")),
          #rejected data
          tabPanel("Rejected Data",
                   dataTableOutput("rejected"),
                   textInput("comm4","Comments"))
        )
   )
),

add_busy_spinner(spin = "fading-circle"))

# Define server logic required to display query
server <- function(input, output) {
  
   
   #have to make dates into strings, otherwise they come out as funny numbers
   #all other variables are reactive 'as is' except for reject button
   #isolate data so that you have to click a button so that it runs the query using eventReactive.

   data<-eventReactive(input$goButton,{
     
   rstdt<-toString(sprintf("%s",input$startd))
   rendd<-toString(sprintf("%s",input$endd))
   
   #actual query for data
   dat<-NPDES_AWQMS_Qry(startdate=rstdt,enddate=rendd,org=c(input$orgs),station=c(input$monlocs),reject=TRUE)
   
   })
   
   #take data, make a subtable for VIEWING in the shiny app so it only shows desired columns from the AWQMS pull in desired order
   tsub<-eventReactive(input$goButton,{
     tsub<-select(data(),Org_Name,Project1,StationDes,MLocID,MonLocType,SampleStartDate,SampleMedia,
               SampleSubmedia,Activity_Type,Statistical_Base,Char_Name,Char_Speciation,
               Sample_Fraction,CASNumber,Result,Result_Unit,Analytical_method,
               Activity_Comment,Result_Comment,Result_status,Result_Type)
   tsub
   })
   
   #take data, make a subtable for DOWNLOAD so that we only show the desired columns from the AWQMS data pull and in the desired order
   dsub<-eventReactive(input$goButton,{
     dsub<-select(data(),OrganizationID,Org_Name,Project1,act_id,StationDes,MLocID,MonLocType,SampleStartDate,SampleStartTime,SampleMedia,
                 SampleSubmedia,Activity_Type,Statistical_Base,Time_Basis,Char_Name,Char_Speciation,
                 Sample_Fraction,CASNumber,Result,Result_Unit,Analytical_method,Method_Code,Method_Context,Analytical_Lab,
                 MDLType,MDLValue,MDLUnit,MRLType,MRLValue,MRLUnit,
                 Activity_Comment,Result_Comment,Result_status,Result_Type)
     dsub
   })
   
   
   #table of queried data for Shiny app view  
   output$table<-renderDataTable({
     
    tsub()
   })
   
   #do QL checks
   qlchk<-eventReactive(input$goButton,{
     ql(data())
     })
                        
   #QL checks for shiny app view
   output$ql<-renderDataTable({
     qlchk()
   })
   #run dissolved vs total checks
   dtchk<-eventReactive(input$goButton,{
     dvst(data())
   })
   
   #dissolved vs total for shiny app view
   output$diff<-renderDataTable({
     dtchk()
   })
  
   #get methods used
   metchk<-eventReactive(input$goButton,{
     mc(data())
   })

   #methods for shiny app view
   output$methods<-renderDataTable({
     metchk()
   })
   
   #get rejected data
   rejchk<-eventReactive(input$goButton,{
     rej(data())
   })
   
   #rejected view for shiny app
   output$rejected<-renderDataTable({
     rejchk()
   })

#create workbook
   dwnld<-eventReactive(input$goButton,{
     wb<-createWorkbook()
     #add data
     addWorksheet(wb,"Data")
     writeDataTable(wb,sheet="Data",x=dsub(),tableStyle="none")
     #add ql checks
     addWorksheet(wb,"QL_Check")
     writeDataTable(wb,sheet="QL_Check",x=qlchk(),tableStyle="none")
     #add diff checks
     addWorksheet(wb,"Diff_Check")
     writeDataTable(wb,sheet="Diff_Check",x=dtchk(),tableStyle="none")
     #method CFR check
     addWorksheet(wb,"Method_Check")
     writeDataTable(wb,sheet="Method_Check",x=metchk(),tableStyle="none")
     #add rejected check
     addWorksheet(wb,"Rejected_Data")
     writeDataTable(wb,sheet="Rejected_Data",x=rejchk(),tableStyle="none")
     
     wb
   })

# Download button- only works in Chrome
#set to give NAs as blank cells
output$downloadData <- downloadHandler(
  
  filename = function() {paste(input$permittee,"_EDD_Data_Check_",Sys.Date(),"_",input$data_sub,".xlsx", sep="")},
  content = function(file) {
    saveWorkbook(dwnld(),file)

    })

#R markdown report
output$report<-downloadHandler(
  filename = function() {paste(input$permittee, Sys.Date() ,"_EDDToxics_Report.pdf", sep="")},
  content=function(file){
    
    #create a file in a temporary directory
    tempReport<-file.path(tempdir(),"EDDToxics_Rmarkdown.Rmd")
    #copy our report to the temporary directory file
    file.copy("EDDToxics_Rmarkdown.Rmd",tempReport,overwrite=TRUE)
    
    #create list of characteristics
    #set up parameters to pass to our Rmd document
    params<-list(org=unique(data()$Org_Name),
                 startdate=input$startd,
                 enddate=input$endd,
                 qls=qlchk(),
                 methods=metchk(),
                 diff=dtchk(),
                 reject=rejchk(),
                 data=data(),
                 qlcom=input$comm1,
                 mcom=input$comm2,
                 dfcom=input$comm3,
                 rejcom=input$comm4)
      
    rmarkdown::render(tempReport, output_file=file,
                      params=params,
                      clean=TRUE,
                      envir=new.env(parent= globalenv())
                      
   )
  }
)

}

# Run the application

shinyApp(ui = ui, server = server)


#make sure you do runApp(launch.browser=TRUE) or in the Run App tab, click "Run External" if you want to download-
#only works in Chrome