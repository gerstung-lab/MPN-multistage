library(shiny)
library(xtable)
library(RColorBrewer)
library(CoxHD)
library(Rcpp)
library(ggplot2)
load("www/MPNmultistate.RData", envir=globalenv())

function(input, output,clientData,session) {

newdataplot<-function(newdata,ET=newdata$ET,PV=newdata$PV,MF=newdata$MF){
newdata$MF==MF;newdata$PV==PV;newdata$ET==ET; lab=c(0,5,10,15,20,25)
multistate<-MultiRFX5(cp_fit, aml_fit, cp_to_mf_fit, mf_fit, aml_fit, newdata, x=365*25)
if (MF==0) {
MEFS<-NULL
if(length(which(rowSums(multistate[,1:5,1])>0.5))==0){
MEFS="Expected median EFS > 25 years"
}else{
MEFS<-paste0("Expected median EFS: ",round(min(which(rowSums(multistate[,1:5,1])>0.5))*25/365,0)," year(s)")
}
sedimentPlot(-multistate[seq(1,361,30),1:5,1], x=seq(1,361,30),y0=1, y1=0,  col=c(pastel2[c(1,2,3,4,5)], "#D6E3DE"), xlab="Time from diagnosis (years)",ylab="Proportion of patients", xaxt="n")
    lines(x=seq(1,361,30), y=1-rowSums(multistate[seq(1,361,30),1:4,1]), lwd=1)
axis(side=1,at=lab*365.25/25,labels=lab)
text(x=4.5*365/25,y=0.15,labels=paste0("5yr OS: ",round(1-sum(multistate[5*365/25,1:4,1]),3)*100,"%"))
text(x=4.5*365/25,y=0.1,labels=paste0("5yr AML risk: ",round(sum(multistate[5*365/25,3:4,1]),3)*100,"%"))
text(x=4.5*365/25,y=0.05,labels=paste0("5yr MF risk: ",round(sum(multistate[5*365/25,c(2,4,5),1]),3)*100,"%"))
text(x=12.5*365/25,y=0.15,labels=paste0("10yr OS: ",round(1-sum(multistate[10*365/25,1:4,1]),3)*100,"%"))
text(x=12.5*365/25,y=0.1,labels=paste0("10yr AML risk: ",round(sum(multistate[10*365/25,3:4,1]),3)*100,"%"))
text(x=12.5*365/25,y=0.05,labels=paste0("10yr MF risk: ",round(sum(multistate[10*365/25,c(2,4,5),1]),3)*100,"%"))
text(x=20*365/25,y=0.15,labels=paste0("20yr OS: ",round(1-sum(multistate[20*365/25,1:4,1]),3)*100,"%"))
text(x=20*365/25,y=0.1,labels=paste0("20yr AML risk: ",round(sum(multistate[20*365/25,3:4,1]),3)*100,"%"))
text(x=20*365/25,y=0.05,labels=paste0("20yr MF risk: ",round(sum(multistate[20*365/25,c(2,4,5),1]),3)*100,"%"))
text(x=12.5*365/25,y=0.5,labels=MEFS)
} else {
MEFS<-NULL
if(length(which(rowSums(multistate[,6:7,1])>0.5))==0){
MEFS="Expected median EFS > 25 years"
}else{
MEFS<-paste0("Expected median EFS: ",round(min(which(rowSums(multistate[,6:7,1])>0.5))*25/365,0)," year(s)")
}
sedimentPlot(-multistate[seq(1,361,30),6:7,1], x=seq(1,361,30),y0=1, y1=0,  col=c(pastel2[c(6,7)], "#D1C299"), xlab="Time from diagnosis (years)",ylab="Proportion of patients", xaxt="n")
    lines(x=seq(1,361,30), y=1-rowSums(multistate[seq(1,361,30),6:7,1]), lwd=1)
axis(side=1,at=lab*365.25/25,labels=lab)
text(x=4.5*365/25,y=0.1,labels=paste0("5yr OS: ",round(1-sum(multistate[5*365/25,6:7,1]),3)*100,"%"))
text(x=12.5*365/25,y=0.1,labels=paste0("10yr OS: ",round(1-sum(multistate[10*365/25,6:7,1]),3)*100,"%"))
text(x=20*365/25,y=0.1,labels=paste0("20yr OS: ",round(1-sum(multistate[20*365/25,6:7,1]),3)*100,"%"))
text(x=4.5*365/25,y=0.05,labels=paste0("5yr AML risk: ",round(multistate[5*365/25,7,1],3)*100,"%"))
text(x=12.5*365/25,y=0.05,labels=paste0("10yr AML risk: ",round(multistate[10*365/25,7,1],3)*100,"%"))
text(x=20*365/25,y=0.05,labels=paste0("20yr AML risk: ",round(multistate[20*365/25,7,1],3)*100,"%"))
text(x=12.5*365/25,y=0.5,labels=MEFS)
}
}

combo<-function(data,a,b)
{
A<-0
A<-data.frame(c("ET","ET","ET","PV","PV","PV","MF","MF","MF"),
if(a==b){
c(a,a,a)
}else{
c(paste0(a," alone"),paste0(b," alone"),paste0(a,"+",b))},
c(
as.data.frame(xtabs(~data[,which(colnames(data)==a)]+data[,which(colnames(data)==b)]+ET,data))[6:8,4]*100/sum(data$ET),
as.data.frame(xtabs(~data[,which(colnames(data)==a)]+data[,which(colnames(data)==b)]+PV,data))[6:8,4]*100/sum(data$PV),
as.data.frame(xtabs(~data[,which(colnames(data)==a)]+data[,which(colnames(data)==b)]+MF,data))[6:8,4]*100/sum(data$MF)
))
colnames(A)<-c("Diagnosis","Gene","Percentage"); ggplot(A,aes(x=Diagnosis,y=Percentage))+geom_col(aes(fill=Gene))+theme(text = element_text(size=20),panel.background = element_blank(),axis.line = element_line(colour = "black"))+ylab("Percentage of patients with mutation(s)")
}


observe({
datasetInput<-reactive({switch(input$dataset,
           "Essential Thrombocytosis (n=1244)" = MPNinput[which(MPNinput$ET==1),],
           "Polycythemia Vera (n=312)" = MPNinput[which(MPNinput$PV==1),],
           "Primary/Secondary Myelofibrosis (n=276)" = MPNinput[which(MPNinput$MF==1),],
           "Other MPN (n=43)" = MPNinput[which(MPNinput$ET==0&MPNinput$PV==0&MPNinput$MF==0),])})
#Make patient list specific to chosen diagnosis:
updateSelectInput(session, "patient",choices=dput(datasetInput()$id)[2:nrow(datasetInput())])
PID<-eventReactive(input$update,{input$patient})

#Display patient characteristics:
output$UPN <- renderText({
paste("Patient Selected:", as.character(PID()))
})

PNO<-reactive({
if(length(which(datasetInput()$id==as.character(PID())))==0){1
}else{
which(datasetInput()$id==as.character(PID()))}
})

  output$MutationDesc <- renderText({
if(length(which(datasetInput()[PNO(),1:55,drop=FALSE]!=0))==0){
     h4("Patient Description")
     paste0("Mutations detected: Nil")
     }else{
      h4("Patient Description")
paste0("Mutations detected: ",toString(
variables[1,which(datasetInput()[PNO(),c(1:55),drop=FALSE]!=0)] ))
  }
  })

output$Demographics <- renderText({
paste("Age: ",round(datasetInput()$Age[PNO()]*10,0),". Gender:",datasetInput()$Gender[PNO()], ". Haemoglobin (g/l): ",round(datasetInput()$Hb[PNO()]*100,0), ". White cell count (x10^9/l):", round(datasetInput()$WCC[PNO()]*100,1),". Platelet count (x10^9/l):", round(datasetInput()$Pl[PNO()]*1000,0), collapse="\t")
})

output$OutcomeMF<-renderText({
if(datasetInput()$MF[PNO()]!=1){
if(datasetInput()$MFTC[PNO()]==1){
paste("Patient developed secondary myelofibrosis within",ceiling(datasetInput()$MFT[PNO()]/365.25)," year(s) of diagnosis.")
}else{paste("")}
}else{paste("")}
})


output$Outcome <- renderText({
if(datasetInput()$AMLTC[PNO()]==1){
paste("Patient developed AML within",ceiling(datasetInput()$AMLT[PNO()]/365.25)," year(s) of diagnosis")
}else if(datasetInput()$DeathC[PNO()]==1){
paste("Patient did not develop AML during follow-up time, but died within",ceiling(datasetInput()$Death[PNO()]/365.25)," year(s) of diagnosis.")
}else{
paste("AML transformation or death did not occur during follow-up")
}
})

output$Gene1stat1<-renderText({
paste0("Present in ",round(sum(TGSgenes[,which(colnames(TGSgenes)==input$Gene1)])*100/2041,1),"% of cohort.") })

output$Gene1stat2<-renderText({
paste0("(",
round(length(which(TGSgenes[,input$Gene1]==1&TGSgenes$Sex=="M"))*100/length(which(TGSgenes[,input$Gene1]==1)),1),"% Male, Median age at diagnosis ",median(TGSgenes$Age[which(TGSgenes[,input$Gene1]==1)],na.rm=TRUE)," years)")
})

output$Gene1stat3<-renderText({
paste0("Mean number of mutations in patients with ",input$Gene1,": ",round(mean(TGSgenes$Mutations[which(TGSgenes[,input$Gene1]==1)],na.rm=TRUE),2))
})
output$Gene2stat1<-renderText({
paste0("Present in ",round(sum(TGSgenes[,which(colnames(TGSgenes)==input$Gene2)])*100/2041,1),"% of cohort.") })

output$Gene2stat2<-renderText({
paste0("(",
round(length(which(TGSgenes[,input$Gene2]==1&TGSgenes$Sex=="M"))*100/length(which(TGSgenes[,input$Gene2]==1)),1),"% Male, Median age at diagnosis ",median(TGSgenes$Age[which(TGSgenes[,input$Gene2]==1)],na.rm=TRUE)," years)")
})

output$Gene2stat3<-renderText({
paste0("Mean number of mutations in patients with ",input$Gene2,": ",round(mean(TGSgenes$Mutations[which(TGSgenes[,input$Gene2]==1)],na.rm=TRUE),2))
})



values<-reactiveValues(val=NULL)
observeEvent(input$update,{
values$val<-datasetInput()[PNO(),1:71]
if(input$newdata=="Input new patient data"){
values$val$Age<-input$Age/10
values$val$Hb<-input$Hb/100
values$val$WCC<-input$WCC/100
values$val$Pl<-input$Pl/1000
values$val$Sex<-as.numeric(input$Sex)
values$val$Splen<-as.numeric(input$Splen)
values$val$PriorThrom<-as.numeric(input$PriorThrom)
values$val$CALR1<-as.numeric(input$CALR)*(2-as.numeric(input$CALR))
values$val$CALR2<-(as.numeric(input$CALR)/2)*(as.numeric(input$CALR)-1)
values$val$JAK2<-as.numeric(input$JAK2)
values$val$JAK2e12<-as.numeric(input$JAK2e12)values$val$MPL<-as.numeric(input$MPL)values$val$TET2<-as.numeric(input$TET2)values$val$ASXL1<-as.numeric(input$ASXL1)values$val$DNMT3A<-as.numeric(input$DNMT3A)values$val$PPM1D<-as.numeric(input$PPM1D)values$val$EZH2<-as.numeric(input$EZH2)values$val$NF1<-as.numeric(input$NF1)values$val$NFE2<-as.numeric(input$NFE2)values$val$SF3B1<-as.numeric(input$SF3B1)values$val$SRSF2<-as.numeric(input$SRSF2)values$val$TP53<-as.numeric(input$TP53)values$val$U2AF1<-as.numeric(input$U2AF1)values$val$CBL<-as.numeric(input$CBL)values$val$MLL3<-as.numeric(input$MLL3)values$val$ZRSR2<-as.numeric(input$ZRSR2)values$val$GNAS<-as.numeric(input$GNAS)values$val$KRAS<-as.numeric(input$KRAS)values$val$SH2B3<-as.numeric(input$SH2B3)values$val$IDH2<-as.numeric(input$IDH2)values$val$PTPN11<-as.numeric(input$PTPN11)values$val$KIT<-as.numeric(input$KIT)values$val$SETBP1<-as.numeric(input$SETBP1)values$val$BCOR<-as.numeric(input$BCOR)values$val$NRAS<-as.numeric(input$NRAS)values$val$CUX1<-as.numeric(input$CUX1)values$val$STAG2<-as.numeric(input$STAG2)values$val$IDH1<-as.numeric(input$IDH1)values$val$FLT3<-as.numeric(input$FLT3)values$val$RUNX1<-as.numeric(input$RUNX1)values$val$PHF6<-as.numeric(input$PHF6)values$val$GATA2<-as.numeric(input$GATA2)values$val$MBD1<-as.numeric(input$MBD1)
#values$val$RB1<-as.numeric(input$RB1)values$val$GNB1<-as.numeric(input$GNB1)values$val$C1p<-as.numeric(input$C1p)values$val$C1q<-as.numeric(input$C1q)values$val$C4<-as.numeric(input$C4)values$val$C5<-as.numeric(input$C5)values$val$C7<-as.numeric(input$C7)values$val$C8<-as.numeric(input$C8)values$val$C9U<-as.numeric(input$C9U)values$val$C9g<-as.numeric(input$C9g)values$val$C11<-as.numeric(input$C11)values$val$C12<-as.numeric(input$C12)values$val$C13<-as.numeric(input$C13)values$val$C14<-as.numeric(input$C14)values$val$C17<-as.numeric(input$C17)values$val$C18<-as.numeric(input$C18)values$val$C19<-as.numeric(input$C19)values$val$C20<-as.numeric(input$C20)
}else if(input$newdata=="Input data from file"){
inFile <- input$file1
filedata<-read.csv(inFile$datapath, header=TRUE, sep=",")
values$val[1:55]<-filedata[1,1:55]
values$val$Age<-filedata$Age[1]/10
values$val$Sex<-as.numeric(filedata$Sex[1])
values$val$Hb<-filedata$Hb[1]/100
values$val$WCC<-filedata$WCC[1]/100
values$val$Pl<-filedata$Pl[1]/1000
}
})

observeEvent(input$update,{
output$medianEFS<-
output$msplot<-
renderPlot({par(bty="L", xaxs="i",yaxs="i", mar=c(5,5,1,1))
newdataplot(values$val)
},height=function(){400},width=function(){600}
)})

observe(if(input$newdata=="Input new patient data"){ updateNumericInput(session, "patient", value=dput(datasetInput()$id[1]))
})

output$downloadData <- downloadHandler(
    filename = "template.csv",
    content = function(file) {
      write.csv(datasetInput()[1,1:65], file,row.names=FALSE)
    })

output$comboplot<-renderPlot({par(bty="L", xaxs="i",yaxs="i")
combo(TGSgenes,input$Gene1,input$Gene2)
},height=function(){400},width=function(){600}
)


})
#observe({session$sendCustomMessage(type = 'testmessage', message = "There are no more patients with this diagnosis.")})
}
