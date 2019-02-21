#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
######This app is meant to help QC data submitted under EDD toxics by NPDES permittees

print("Initial data queries may take a few minutes.")

library(shiny)
library(AWQMSdata)
library(dplyr)
library(xlsx)
library(xlsxjars)
library(shinybusy)

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

#NPDES only needs a limited # of Chars, this should help speed up the program

#make it so only the groupings are shown in the drop down, 
#have the actual characteristics in separate variables to be called by program
chars <- c("All RPA","All Toxics","Copper BLM","ph RPA","Ammonia RPA","DO RPA","Pesticides and PCB RPA","Base Neutral RPA", "Acid Extractable RPA",
           "VOC RPA","Metals RPA")

#RPA specific characteristics
#pH RPA
phrpa<-c("Alkalinity, total","pH","Temperature, water","Salinity","Conductivity")

#Ammonia RPA
ammrpa<-c("Alkalinity, total","Ammonia ","Ammonia and ammonium","Ammonia-nitrogen","Conductivity","pH","Temperature, water","Salinity")

#Copper BLM
cuB<-c("Alkalinity, total","Calcium","Chloride","Copper","Magnesium","pH","Potassium","Sodium","Sulfate","Organic carbon",
       "Temperature, water","Total Sulfate","Sulfide")

#Dissolved Oxygen RPA
dorpa<-c("Dissolved oxygen (DO)","Dissolved oxygen saturation","Biochemical oxygen demand, non-standard conditions",
         "Biochemical oxygen demand, standard conditions","Kjeldahl nitrogen","Total Kjeldahl nitrogen","Temperature, water",
         "Ammonia ","Ammonia and ammonium","Ammonia-nitrogen")  

#Pesticides and PCBs
pestrpa<-c("p,p'-DDT","Parathion","Chlordane","Lindane","Dieldrin","Endrin","Methoxychlor","p,p'-DDD","p,p'-DDE","Heptachlor",
           "Azinphos-methyl","Malathion","Aldrin",".alpha.-Hexachlorocyclohexane",".beta.-Hexachlorocyclohexane",
           "Benzene Hexachloride, Beta (BHC)","1,2,3,4,5,6-Hexachlorocyclohexane",".alpha.-Endosulfan","Heptachlor epoxide",
           "Endosulfan sulfate","Mirex","Chlorpyrifos","Endrin aldehyde","Toxaphene","Demeton","Aroclor 1260","Aroclor 1254",
           "Aroclor 1221","Aroclor 1232","Aroclor 1248","Aroclor 1016",".beta.-Endosulfan","Aroclor 1242")

#Base Neutral
bneut<-c("Benzo[a]pyrene","Dibenz[a,h]anthracene","Benz[a]anthracene","N-Nitrosodimethylamine","Hexachloroethane",
         "Hexachlorocyclopentadiene","Isophorone","Acenaphthene","Diethyl phthalate","Dibutyl phthalate","Phenanthrene",
         "Butyl benzyl phthalate","N-Nitrosodiphenylamine","Fluorene","Hexachlorobutadiene","Naphthalene","2-Chloronaphthalene",
         "3,3'-Dichlorobenzidine","Benzidine","1,2,4,5-Tetrachlorobenzene","Nitrobenzene","p-Bromophenyl phenyl ether",
         "Bis(2-chloro-1-methylethyl) ether","Bis(2-chloroethyl) ether","Bis(2-chloroethoxy)methane","Di(2-ethylhexyl) phthalate",
         "Di-n-octyl phthalate","Hexachlorobenzene","Anthracene","1,2,4-Trichlorobenzene","2,4-Dinitrotoluene","1,2-Diphenylhydrazine",
         "Pyrene","Dimethyl phthalate","Benzo[ghi]perylene","Indeno[1,2,3-cd]pyrene","Benzo(b)fluoranthene","Fluoranthene",
         "Benzo[k]fluoranthene","Acenaphthylene","Chrysene","2,6-Dinitrotoluene","Pentachlorobenzene","N-Nitrosodi-n-propylamine",
         "p-Chlorophenyl phenyl ether")

#Acid Extractable
aext<-c("2,4-Dinitrophenol","p-Chloro-m-cresol","Pentachlorophenol","2,4,6-Trichlorophenol","o-Nitrophenol","o-Chlorophenol",
        "2,4,5-Trichlorophenol","p-Nitrophenol","2,4-Dimethylphenol","Phenol","2,4-Dichlorophenol","4,6-Dinitro-o-cresol")

#Volatile Organic Carbons
vocrpa<-c("Carbon tetrachloride","Chloroform","Benzene","1,1,1-Trichloroethane","Methyl bromide","Chloromethane","Chloroethane",
          "Vinyl chloride","Methylenechloride","Tribromomethane","Dichlorobromomethane","1,1-Dichloroethane","1,1-Dichloroethylene",
          "1,2-Dichloropropane","1,1,2-Trichloroethane","Trichloroethene(TCE)","1,1,2,2-Tetrachloroethane","o-Dichlorobenzene",
          "Ethylbenzene","p-Dichlorobenzene","Acrolein","Allyl chloride","1,2-Dichloroethane","Toluene","Chlorobenzene",
          "2-Chloroethyl vinyl ether","Chlorodibromomethane","Tetrachloroethene","Tetrachloroethylene","trans-1,2-Dichloroethylene",
          "m-Dichlorobenzene","1,3-Dichloropropene")

#Metals and Hardness
metalsrpa<-c("Cyanide","Aluminum","Iron","Lead","Mercury","Nickel","Silver","Thallium","Antimony","Arsenic","Arsenic, Inorganic",
             "Beryllium","Cadmium","Chromium","Copper","Zinc","Selenium","Nitrate","Inorganic nitrogen (nitrate and nitrite)",
             "Nitrate + Nitrite","Chromium(III)","Chromium(VI)","Arsenic ion (3+)","Total hardness","Hardness, Ca, Mg",
             "Hardness, carbonate","Hardness, non-carbonate","Ammonia ","Ammonia and ammonium","Ammonia-nitrogen")

#all toxics (metals, voc, acid extractable, base neutral,pesticides and PCBs,metals)
tox<-c(metalsrpa,vocrpa,aext,bneut,pestrpa)


# Check to see if saved cache of data exists. If it does not, or is greater than
# 7 days old, query out stations and organizations and save the cache
if(!file.exists("query_cache.RData") | 
   difftime(Sys.Date() ,file.mtime("query_cache.RData") , units = c("days")) > 7){
  

#NPDES_AWQMS_Stations functions only pulls stations 
station <- NPDES_AWQMS_Stations()
Mtype<-station$MonLocType
station <- station$MLocID
station <- sort(station)

organization <- AWQMS_Orgs()
organization <- organization$OrganizationID
organization <- sort(organization)

save(station, Mtype, organization, file = 'query_cache.RData')
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
                  label="Permittee Name"),
        #permit #
        textInput("permit_num",
                  label="Permit Number"),
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
       #Reject button
       checkboxInput("Reject",
                     label = "Keep Rejected data",
                     value = FALSE),
       
       #add action button, idea is to not run query until the button is clicked)
       actionButton("goButton","Run Query"),
       #add a download button
       downloadButton('downloadData', 'Download Data')
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
        tabPanel("Quantitation Limit Issues",dataTableOutput("ql")),
        #Total vs dissolved
        tabPanel("Total vs Dissolved",dataTableOutput("diff")),
        #methods used
        tabPanel("Methods",dataTableOutput("methods"))
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
   rrej<-if(input$Reject) {TRUE} else {FALSE} 
   
   #actual query for data
   dat<-NPDES_AWQMS_Qry(startdate=rstdt,enddate=rendd,org=c(input$orgs),reject=rrej)
   
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
# Download button- only works in Chrome
#gives an excel with two sheets, the first is the serach parameters (needs some work), the second is the data
#set to give NAs as blank cells
output$downloadData <- downloadHandler(
  
  filename = function() {paste("AWQMS_Download-", Sys.Date(),"_",input$permit_num,".xlsx", sep="")},
  content = function(file) {
    #sheet with data
    write.xlsx(dsub(), file,sheetName="Data",row.names = FALSE,showNA=FALSE,append=TRUE)
    #sheet with QL issues
    #sheet with issues between total and dissolved
    #sheet with methods used

    })

}
# Run the application
shinyApp(ui = ui, server = server)

#make sure you do runApp(launch.browser=TRUE) or in the Run App tab, click "Run External" if you want to download-
#only works in Chrome
