
df_income2<-df_income%>%
  filter(Indicators_en!='Other')

######

fvariables_xts2<-function(){
  fvariable1<-function(filter){
    dat=df_income2%>%
      select(Date,Indicators_en,Total1)%>%group_by(Indicators_en,Date,)%>%
      summarise(Total1=sum(Total1,na.rm = T))%>%
      filter(Indicators_en==filter)%>%data.frame()
    
    return(dat$Total1
           # xts::xts(dat$Total1,order.by = as.Date(dat$Date))
           
    )
  }
  ###
  
  
  ##
  lparts1=unique(df_income2$Indicators_en)
  listXTS_parts1=NULL
  for (i in 1:length(lparts1)) {
    listXTS_parts1[[i]]=fvariable1(filter=lparts1[i])
  }
  listXTS_parts1=as.data.frame(listXTS_parts1)
  colnames(listXTS_parts1)=lparts1
   #################33
  dates=sort(unique(df$Date))
  fvar_xts<-function(x){
    y=xts::to.quarterly(xts::xts(x,order.by = as.Date(dates)))[,4]
    y=xts::xts(x,order.by = as.Date(dates))
    names(y)=names(x)
    return(y
           #xts::to.quarterly(xts::xts(x,order.by = as.Date(dates)))[,4]
    )
  }
  zo=NULL  
  for (i in 1:length(lparts1)) {
    
    zo[[i]]=fvar_xts(listXTS_parts1[,lparts1[i]])/1000000 # in thousand HTG
    
  }
  names(zo)=colnames(listXTS_parts1)  
  return(zo)
  #################3
  #return(listXTS_parts1) # return a data frame of total of each variable
  
}

df_incomeXTS=do.call(cbind,fvariables_xts2())
