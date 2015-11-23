###### Author: Jacques Booysen #####
###### booysenjacques@gmail.com ####
library(jsonlite)
library(leaflet)  
library(sp)
library(rgdal)
library(data.table)
library(RJSONIO)  
library(RSQLite)
library(R.cache)
library(memoise)
library(shiny)
dir.exists = file.exists

availabledashboards = c("Planner view"="planner.R",
                        "Environmentalist View"="environmentalist.R",
                        "Economist View"="econimist.R")

# used for unconstraint
makeMs <- function(crit){
  l <- lapply(crit$run_id, function(run_id){
    if(dir.exists(system.file('rdata', package='cridfdata'))) {
      load(sprintf("%s/%s.Rdata", system.file('rdata', package='cridfdata'), run_id))
    } else {
      load(sprintf("data/rdata/%s.Rdata", run_id))
    }
    df
  })
  ms <- as.data.table(do.call(rbind,l))
  ms[ms$unit=="MWyr",]$value = ms[ms$unit=="MWyr",]$value * 8.76581277 
  ms[ms$unit=="MWyr",]$unit = "GWh"
  return(ms)
}
makeMs2 <- memoise(makeMs)

# used for constrant
makeMs3 <- function(crit){
  l <- lapply(crit$run_id, function(run_id){
    if(dir.exists(system.file('rdata', package='cridfdata'))) {
      load(sprintf("%s/%s.Rdata", system.file('rdata', package='cridfdata'), run_id))
    } else {
      load(sprintf("data/rdata/%s.Rdata", run_id))
    }
    df
  })
  ms <- as.data.table(do.call(rbind,l))
  ms[ms$unit=="MWyr",]$value = ms[ms$unit=="MWyr",]$value * 8.76581277 
  ms[ms$unit=="MWyr",]$unit = "GWh"
  return(ms)
}
makeMs4 <- function(crit) { return(memoizedCall(makeMs3,crit)) }


getunconstraint <- function(water,coaluclf,txuclf,
                            exclGI=FALSE,adjcons=FALSE,cons=1) {
  
  crit <- subset(runMasterdata, policy_id==14 & 
                   water.availability==water &
                   coal.uclf==coaluclf & 
                   transmission.uclf==txuclf &
                   grand.inga.out==as.integer(exclGI) &
                   consumption.adjustment==cons
  )
  
  ms <- makeMs2(crit)
  ms <- subset(ms,show=='yes')
  
  return(ms)
}

getconstraint <- function(designwater,designcoaluclf,designtxuclf,designexclGI,designcons, 
                          water, coaluclf,txuclf,exclGI,cons,
                          fixwater=TRUE,fixcoaluclf=TRUE,fixtxuclf=TRUE,fixexclGI=TRUE,fixcons=TRUE) {
  
  if(!fixwater) {
    crit <- subset(runMasterdata, policy_id==15 & 
                     coal.uclf==coaluclf & 
                     transmission.uclf==txuclf & 
                     consumption.adjustment==cons &
                     grand.inga.out==as.integer(exclGI) & 
                     design.water.availability == designwater &
                     design.coal.uclf == designcoaluclf & 
                     design.consumption.adjustment == designcons & 
                     design.grand.inga.out==as.integer(designexclGI) & 
                     design.transmission.uclf == designtxuclf
    )
  } else if(!fixcoaluclf) {
    crit <- subset(runMasterdata, policy_id==15 & 
                     water.availability==water &
                     transmission.uclf==txuclf & 
                     consumption.adjustment==cons &
                     grand.inga.out==as.integer(exclGI) & 
                     design.water.availability == designwater &
                     design.coal.uclf == designcoaluclf & 
                     design.consumption.adjustment == designcons & 
                     design.grand.inga.out==as.integer(designexclGI) & 
                     design.transmission.uclf == designtxuclf
    )
  } else if(!fixtxuclf) {
    crit <- subset(runMasterdata, policy_id==15 & 
                     water.availability==water &
                     coal.uclf==coaluclf & 
                     consumption.adjustment==cons &
                     grand.inga.out==as.integer(exclGI) & 
                     design.water.availability == designwater &
                     design.coal.uclf == designcoaluclf & 
                     design.consumption.adjustment == designcons & 
                     design.grand.inga.out==as.integer(designexclGI) & 
                     design.transmission.uclf == designtxuclf
    )
  } else if(!fixcons) {
    crit <- subset(runMasterdata, policy_id==15 & 
                     water.availability==water &
                     coal.uclf==coaluclf & 
                     transmission.uclf==txuclf & 
                     grand.inga.out==as.integer(exclGI) & 
                     design.water.availability == designwater &
                     design.coal.uclf == designcoaluclf & 
                     design.consumption.adjustment == designcons & 
                     design.grand.inga.out==as.integer(designexclGI) & 
                     design.transmission.uclf == designtxuclf
    )
  } else if(!fixexclGI) {      
    crit <- subset(runMasterdata, policy_id==15 & 
                     water.availability==water &
                     coal.uclf==coaluclf & 
                     transmission.uclf==txuclf & 
                     consumption.adjustment==cons &
                     design.water.availability == designwater &
                     design.coal.uclf == designcoaluclf & 
                     design.consumption.adjustment == designcons & 
                     design.grand.inga.out==as.integer(designexclGI) & 
                     design.transmission.uclf == designtxuclf
    )
  } else {
    crit <- subset(runMasterdata, policy_id==15 & 
                     water.availability==water &
                     coal.uclf==coaluclf & 
                     transmission.uclf==txuclf & 
                     consumption.adjustment==cons &
                     grand.inga.out==as.integer(exclGI) & 
                     design.water.availability == designwater &
                     design.coal.uclf == designcoaluclf & 
                     design.consumption.adjustment == designcons & 
                     design.grand.inga.out==as.integer(designexclGI) & 
                     design.transmission.uclf == designtxuclf
    )
  }
  
  ms <- makeMs4(crit) 
  ms <- subset(ms,show=='yes')
  return(ms)
}


# 1.1
demo1 <- function(thewater,thecoaluclf,thetxuclf,thecountry, thedom="",stext="",thelevel="All",startyear=2011,endyear=2040,
                  exclGI=FALSE,adjcons=FALSE,cons=0,cum=FALSE) {
  
  
  td = getunconstraint(thewater/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,cons/100)
  if(length(td[,1])==0) {return(NULL);}
  
  seriesname = "New Capacity"
  tfinal = subset(td, series == seriesname)  
  units = as.character(tfinal$unit[1])
  
  if (thecountry!="All") {
    tfinal = subset(tfinal, country.name == thecountry)          
  }
  if (thelevel!="All") {
    tfinal = subset(tfinal, level == thelevel)          
  }
  tfinal = subset(tfinal, time %in% (seq(startyear,endyear,1)))          
  
  
  #x = unique(tfinal$time)
  #x = x[order(x)]
  
  if(nrow(tfinal)>0) {
    
    tfinal2 = tfinal[, c("time","value","energy.source"),with=F]
    tfinal3 = tfinal2[, lapply(.SD, sum), by = c("time","energy.source")]   
    #save(tfinal3,file="/tmp/debug.rdata")
    
    if(cum) {
     a = by(tfinal3,tfinal3$energy.source, function(x) {
       if(length(x)>0) {
         as.data.frame(cbind(time=x$time,energy.source=as.character(x$energy.source),value=cumsum(x$value)))
       } 
      })
     a = a[lapply(a,length)>0]
     tfinal4 = as.data.frame(do.call(rbind,a))
     tfinal4$value = as.numeric(as.character(tfinal4$value))
     tfinal4$time = as.numeric(as.character(tfinal4$time))
     tfinal3 = tfinal4
    }
    
    tfinal3 = tfinal3[(tfinal3$time>2010) & (tfinal3$time<2050) ,]
    
    tdat = as.data.frame(t(reshape(tfinal3,idvar=c("energy.source"),direction="wide")),stringsAsFactors=F)
    colnames(tdat) = as.character(unlist(tdat[1,]))
    tdat = tdat[-1,]
    rownames(tdat) = gsub("value\\.","",rownames(tdat))      
    
    x = as.data.frame(apply(tdat,2,as.numeric),stringsAsFactors=F)
    rownames(x) = rownames(tdat)
    
    nn = colnames(x)
    x2 = as.data.frame(x[,order(nn)])
    nn = nn[order(nn)]
    colnames(x2) = nn
    x = x2
    
  }
  
  h1 <- rCharts:::Highcharts$new()
  h1$chart(type = "area",marginLeft=100,height=300)
  h1$title(text = paste("New Capacity (",thecountry,")",sep=""))
  h1$subtitle(text = paste(stext,sep=""))
  
  if(nrow(tfinal)>0) {
    h1$xAxis(categories = paste("",rownames(x),sep="") )
    h1$yAxis(title = list(text = units),stackLabels= list(enabled=T))
    h1$data(x)      
    # Print chart
  }
  
  
  h1$legend(symbolWidth = 10)
  h1$set(dom = thedom)
  h1$plotOptions(animation=FALSE,
                 area=list(
                   stacking= 'normal',
                   animation=FALSE,
                   events=list(
                     legendItemClick = paste("#! function() {
                                             console.log(this);
                                             Shiny.onInputChange(\'",thedom,"LegendItemClick\', {
                                             name: this.name,
                                             visible: this.visible    }) } !#",sep="")
                     #legendItemClick = "#! function() {alert(this.name);  } !#"
                     )
                   )
                 )
  h1$exporting(enabled = T)    
  
  return(h1)       
}  

# 2.1
demo2 <- function(thewater,thecoaluclf,thetxuclf,thecountry, thedom="",stext="",thelevel="All",startyear=2011,endyear=2040,
                  exclGI=FALSE,adjcons=FALSE,cons=0,seriesname = "Avg Price",units = "Difference in Average Price") {
  
  td =  getunconstraint(110/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,100/100)
  td1 = getunconstraint(100/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,100/100)
  td2 = getunconstraint(120/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,100/100)
  
  if(length(td[,1])==0) {return(NULL);}
  if(length(td1[,1])==0) {return(NULL);}
  if(length(td2[,1])==0) {return(NULL);}
  
  tfinal = subset(td, series == seriesname)
  tfinal1 = subset(td1, series == seriesname)  
  tfinal2 = subset(td2, series == seriesname)  
  #if(units!="Difference in Average Price") {
  #  units = as.character(tfinal$unit[1])
  #}
  
  if (thecountry!="All") {
    tfinal = subset(tfinal, country.name == thecountry)          
    tfinal1 = subset(tfinal1, country.name == thecountry)
    tfinal2 = subset(tfinal2, country.name == thecountry)
  }
  if (thelevel!="All") {
    tfinal = subset(tfinal, level == thelevel)
    tfinal1 = subset(tfinal1, level == thelevel)          
    tfinal2 = subset(tfinal2, level == thelevel)          
  }
  tfinal = subset(tfinal, time %in% (seq(startyear,endyear,1)))          
  tfinal1 = subset(tfinal1, time %in% (seq(startyear,endyear,1)))          
  tfinal2 = subset(tfinal2, time %in% (seq(startyear,endyear,1)))          
  
  if(nrow(tfinal)>0) {
    tfinala = tfinal[, c("time","value","country.name"),with=F]
    tfinalb = tfinala[, lapply(.SD, mean), by = c("country.name")]     # Mean of AVG Price
    tfinalb = tfinalb[(tfinalb$time>2010) & (tfinalb$time<2050) ,c("country.name","value"),with=F]
  }
  if(nrow(tfinal1)>0) {
    tfinal1a = tfinal1[, c("time","value","country.name"),with=F]
    tfinal1b = tfinal1a[, lapply(.SD, mean), by = c("country.name")]     # Mean of AVG Price
    tfinal1b = tfinal1b[(tfinal1b$time>2010) & (tfinal1b$time<2050) ,c("country.name","value"),with=F]
  }
  if(nrow(tfinal2)>0) {
    tfinal2a = tfinal2[, c("time","value","country.name"),with=F]
    tfinal2b = tfinal2a[, lapply(.SD, mean), by = c("country.name")]     # Mean of AVG Price
    tfinal2b = tfinal2b[(tfinal2b$time>2010) & (tfinal2b$time<2050) ,c("country.name","value"),with=F]
  }
  
  # tfinal1b$value - tfinalb$value # 20% less consumption
  # tfinal2b$value - tfinalb$value # 20% more consumption
  
  
  h1 <- rCharts:::Highcharts$new()
  h1$chart(type = "column",marginLeft=100,height=500)
  if(seriesname == "Avg Price") {
    h1$title(text = paste("The Difference in Average Price (",thecountry,")",sep=""))
  } else {
    h1$title(text = paste("The Difference in ",seriesname," (",thecountry,")",sep=""))
  }
  h1$subtitle(text = paste(stext,sep=""))
  
  if(nrow(tfinal)>0) {
    h1$xAxis(categories = as.character(tfinalb$country.name), labels = list(rotation = -90))
    h1$yAxis(title = list(text = units))
    h1$series(list(  list(name="10% less water (100%) compared to baseline (110%)", data=(tfinal1b$value - tfinalb$value)),
                     list(name="10% more water (120%) compared to baseline (110%)",data=(tfinal2b$value - tfinalb$value))
    ))      
    # Print chart
  }
  
  
  h1$legend(symbolWidth = 10)
  h1$set(dom = thedom)
  h1$plotOptions(animation=FALSE,
                 column=list(
                   animation=FALSE,
                   events=list(
                     legendItemClick = paste("#! function() {
                                             console.log(this);
                                             Shiny.onInputChange(\'",thedom,"LegendItemClick\', {
                                             name: this.name,
                                             visible: this.visible  }) } !#",sep="")
                     #legendItemClick = "#! function() {alert(this.name);  } !#"
                     )
                   )
                 )
  h1$exporting(enabled = T)    
  
  return(h1)       
}  

# 4.1
demo3 <- function(thewater,thecoaluclf,thetxuclf,thecountry, thedom="",stext="",thelevel="All",startyear=2011,endyear=2040,
                  exclGI=FALSE,adjcons=FALSE,cons=0) {
  
  td = getconstraint(1,1,1,FALSE,1,110/100,  thecoaluclf/100,thetxuclf/100, exclGI,100/100)
  td1 = getconstraint(1,1,1,FALSE,1,100/100, thecoaluclf/100,thetxuclf/100, exclGI,100/100)
  td2 = getconstraint(1,1,1,FALSE,1,120/100, thecoaluclf/100,thetxuclf/100, exclGI,100/100)
  
  if(length(td[,1])==0) {return(NULL);}
  if(length(td1[,1])==0) {return(NULL);}
  if(length(td2[,1])==0) {return(NULL);}
  
  seriesname = c("Fuel Cost","O&M Costs")
  tfinal = subset(td, series %in% seriesname)
  tfinal1 = subset(td1, series %in% seriesname)  
  tfinal2 = subset(td2, series %in% seriesname)  
  units = "Percentage change in fuel and O&M Costs"
  if (thecountry!="All") {
    tfinal = subset(tfinal, country.name == thecountry)          
    tfinal1 = subset(tfinal1, country.name == thecountry)
    tfinal2 = subset(tfinal2, country.name == thecountry)
  }
  if (thelevel!="All") {
    tfinal = subset(tfinal, level == thelevel)
    tfinal1 = subset(tfinal1, level == thelevel)          
    tfinal2 = subset(tfinal2, level == thelevel)          
  }
  tfinal = subset(tfinal, time %in% (seq(startyear,endyear,1)))          
  tfinal1 = subset(tfinal1, time %in% (seq(startyear,endyear,1)))          
  tfinal2 = subset(tfinal2, time %in% (seq(startyear,endyear,1)))          
  
  if(nrow(tfinal)>0) {
    tfinala = tfinal[, c("time","value","country.name"),with=F]
    tfinalb = tfinala[, lapply(.SD, sum), by = c("country.name")]     
    tfinalb = tfinalb[ ,c("country.name","value"),with=F]
  }
  if(nrow(tfinal1)>0) {
    tfinal1a = tfinal1[, c("time","value","country.name"),with=F]
    tfinal1b = tfinal1a[, lapply(.SD, sum), by = c("country.name")]  
    tfinal1b = tfinal1b[ ,c("country.name","value"),with=F]
  }
  if(nrow(tfinal2)>0) {
    tfinal2a = tfinal2[, c("time","value","country.name"),with=F]
    tfinal2b = tfinal2a[, lapply(.SD, sum), by = c("country.name")]  
    tfinal2b = tfinal2b[ ,c("country.name","value"),with=F]
  }
  
  
  
  h1 <- rCharts:::Highcharts$new()
  h1$chart(type = "column",marginLeft=100)
  h1$title(text = paste("Percentage change in fuel and O&M Costs (",thecountry,")",sep=""))
  h1$subtitle(text = paste(stext,sep=""))
  
  if(nrow(tfinal)>0) {
    h1$xAxis(categories = as.character(tfinalb$country.name), labels = list(rotation = -90 ) )
    h1$yAxis(title = list(text = units))
    h1$series(list( list(name="10% less water than Baseline",data=((tfinal1b$value - tfinalb$value)/tfinalb$value)*100 ),
                    list(name="10% more water than Baseline",data=((tfinal2b$value - tfinalb$value)/tfinalb$value)*100 )
    ))      
    # Print chart
  }
  
  
  h1$legend(symbolWidth = 10)
  h1$set(dom = thedom)
  h1$plotOptions(animation=FALSE,
                 column=list(
                   animation=FALSE,
                   events=list(
                     legendItemClick = paste("#! function() {
                                             console.log(this);
                                             Shiny.onInputChange(\'",thedom,"LegendItemClick\', {
                                             name: this.name,
                                             visible: this.visible    }) } !#",sep="")
                     #legendItemClick = "#! function() {alert(this.name);  } !#"
                     )
                   )
                 )
  h1$exporting(enabled = T)    
  
  return(h1)       
}  

# 4.2
demo4 <- function(thewater,thecoaluclf,thetxuclf,thecountry, thedom="",stext="",thelevel="All",startyear=2011,endyear=2040,
                  exclGI=FALSE,adjcons=FALSE,cons=0) {
  
  td = getconstraint(1,1,1,FALSE,1,100/100, 100/100,thetxuclf/100, exclGI,100/100)
  td1 = getconstraint(1,1,1,FALSE,1,120/100, 100/100,thetxuclf/100, exclGI,100/100)
  
  if(length(td[,1])==0) {return(NULL);}
  if(length(td1[,1])==0) {return(NULL);}
  
  seriesname = c("Fuel Cost","O&M Costs","Import cost","Export revenue")
  tfinal = subset(td, series %in% seriesname)
  tfinal1 = subset(td1, series %in% seriesname)  
  
  units = "Percentage change in Cost"
  if (thecountry!="All") {
    tfinal = subset(tfinal, country.name == thecountry)          
    tfinal1 = subset(tfinal1, country.name == thecountry)
  }
  if (thelevel!="All") {
    tfinal = subset(tfinal, level == thelevel)
    tfinal1 = subset(tfinal1, level == thelevel)          
  }
  tfinal = subset(tfinal, time %in% (seq(startyear,endyear,1)))          
  tfinal1 = subset(tfinal1, time %in% (seq(startyear,endyear,1)))          
  
  if(nrow(tfinal)>0) {
    tfinala = tfinal[, c("time","value","country.name","series"),with=F]
    tfinalb = tfinala[, lapply(.SD, sum), by = c("country.name","series")]     
    tfinalb = tfinalb[ ,c("country.name","series","value"),with=F]
  }
  if(nrow(tfinal1)>0) {
    tfinal1a = tfinal1[, c("time","value","country.name","series"),with=F]
    tfinal1b = tfinal1a[, lapply(.SD, sum), by = c("country.name","series")]  
    tfinal1b = tfinal1b[ ,c("country.name","series","value"),with=F]
  }
  
  h1 <- rCharts:::Highcharts$new()
  h1$chart(type = "column",marginLeft=100,height=500)
  h1$title(text = paste("Percentage change in Cost from -10% to +10% water from Baseline (",thecountry,")",sep=""))
  h1$subtitle(text = paste(stext,sep=""))
  
  tfinalb[tfinalb$value==0,]$value = 1
  tfinal1b[tfinal1b$value==0,]$value = 1
  
  if(nrow(tfinal)>0) {
    h1$xAxis(categories = as.character(tfinalb$country.name) )
    h1$yAxis(title = list(text = units))
    h1$series(list( 
      list(name="Export revenue",data=((tfinal1b[tfinal1b$series=="Export revenue",]$value - tfinalb[tfinalb$series=="Export revenue",]$value)/tfinalb[tfinalb$series=="Export revenue",]$value)*100 ),
      list(name="Fuel Cost",data=((tfinal1b[tfinal1b$series=="Fuel Cost",]$value - tfinalb[tfinalb$series=="Fuel Cost",]$value)/tfinalb[tfinalb$series=="Fuel Cost",]$value)*100 ),
      list(name="Import cost",data=((tfinal1b[tfinal1b$series=="Import cost",]$value - tfinalb[tfinalb$series=="Import cost",]$value)/tfinalb[tfinalb$series=="Import cost",]$value)*100 ),
      list(name="O&M Costs",data=((tfinal1b[tfinal1b$series=="O&M Costs",]$value - tfinalb[tfinalb$series=="O&M Costs",]$value)/tfinalb[tfinalb$series=="O&M Costs",]$value)*100 )
    ))      
    # Print chart
  }
  
  
  h1$legend(symbolWidth = 10)
  h1$set(dom = thedom)
  h1$plotOptions(animation=FALSE,
                 column=list(
                   animation=FALSE,
                   events=list(
                     legendItemClick = paste("#! function() {
                                             console.log(this);
                                             Shiny.onInputChange(\'",thedom,"LegendItemClick\', {
                                             name: this.name,
                                             visible: this.visible      
})
                                             } !#",sep="")
                     #legendItemClick = "#! function() {alert(this.name);  } !#"
                   )
                   )
                 )
  h1$exporting(enabled = T)    
  
  return(h1)       
}  
# 5.1
demo5 <- function(thewater,thecoaluclf,thetxuclf,thecountry, thedom="",stext="",thelevel="All",startyear=2011,endyear=2040,
                  exclGI=FALSE,adjcons=FALSE,cons=0) {
  
  td = getconstraint(1,1,1,FALSE,1,110/100, thecoaluclf/100,thetxuclf/100, exclGI,100/100)
  td1 = getconstraint(1,1,1,FALSE,1,100/100, thecoaluclf/100,thetxuclf/100, exclGI,100/100)
  td2 = getconstraint(1,1,1,FALSE,1,120/100, thecoaluclf/100,thetxuclf/100, exclGI,100/100)
  
  #td = getunconstraint(100/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,100/100)
  #td1 = getunconstraint(100/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,100/100)
  #td2 = getunconstraint(120/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,100/100)
  
  if(length(td[,1])==0) {return(NULL);}
  if(length(td1[,1])==0) {return(NULL);}
  if(length(td2[,1])==0) {return(NULL);}
  
  seriesname = "Avg Price"
  tfinal = subset(td, series == seriesname)
  tfinal1 = subset(td1, series == seriesname)  
  tfinal2 = subset(td2, series == seriesname)  
  units = "Percentage change in electricity price"
  if (thecountry!="All") {
    tfinal = subset(tfinal, country.name == thecountry)          
    tfinal1 = subset(tfinal1, country.name == thecountry)
    tfinal2 = subset(tfinal2, country.name == thecountry)
  }
  if (thelevel!="All") {
    tfinal = subset(tfinal, level == thelevel)
    tfinal1 = subset(tfinal1, level == thelevel)          
    tfinal2 = subset(tfinal2, level == thelevel)          
  }
  tfinal = subset(tfinal, time %in% (seq(startyear,endyear,1)))          
  tfinal1 = subset(tfinal1, time %in% (seq(startyear,endyear,1)))          
  tfinal2 = subset(tfinal2, time %in% (seq(startyear,endyear,1)))          
  
  if(nrow(tfinal)>0) {
    tfinala = tfinal[, c("time","value","country.name"),with=F]
    tfinalb = tfinala[, lapply(.SD, mean), by = c("country.name")]     # Mean of AVG Price
    tfinalb = tfinalb[(tfinalb$time>2010) & (tfinalb$time<2050) ,c("country.name","value"),with=F]
  }
  if(nrow(tfinal1)>0) {
    tfinal1a = tfinal1[, c("time","value","country.name"),with=F]
    tfinal1b = tfinal1a[, lapply(.SD, mean), by = c("country.name")]     # Mean of AVG Price
    tfinal1b = tfinal1b[(tfinal1b$time>2010) & (tfinal1b$time<2050) ,c("country.name","value"),with=F]
  }
  if(nrow(tfinal2)>0) {
    tfinal2a = tfinal2[, c("time","value","country.name"),with=F]
    tfinal2b = tfinal2a[, lapply(.SD, mean), by = c("country.name")]     # Mean of AVG Price
    tfinal2b = tfinal2b[(tfinal2b$time>2010) & (tfinal2b$time<2050) ,c("country.name","value"),with=F]
  }
  
  # tfinal1b$value - tfinalb$value # 20% less consumption
  # tfinal2b$value - tfinalb$value # 20% more consumption
  
  
  h1 <- rCharts:::Highcharts$new()
  h1$chart(type = "column",marginLeft=100,height=500)
  h1$title(text = paste("Percentage change in electricity price (",thecountry,")",sep=""))
  h1$subtitle(text = paste(stext,sep=""))
  
  if(nrow(tfinal)>0) {
    h1$xAxis(categories = as.character(tfinalb$country.name) )
    h1$yAxis(title = list(text = units))
    h1$series(list( list(name="Low  water (100% constraint) compared to baseline (110%)",data=(tfinal1b$value - tfinalb$value)),
                    list(name="High water (120% constraint) compared to baseline (110%)",data=(tfinal2b$value - tfinalb$value))
    ))
    
    # Print chart
  }
  
  
  h1$legend(symbolWidth = 10)
  h1$set(dom = thedom)
  h1$plotOptions(animation=FALSE,
                 column=list(
                   animation=FALSE,
                   events=list(
                     legendItemClick = paste("#! function() {
                                             console.log(this);
                                             Shiny.onInputChange(\'",thedom,"LegendItemClick\', {
                                             name: this.name,
                                             visible: this.visible      
})
                                             } !#",sep="")
                     #legendItemClick = "#! function() {alert(this.name);  } !#"
                   )
                   )
                 )
  h1$exporting(enabled = T)    
  
  return(h1)       
}  


barunconstraint <- function(thewater,thecoaluclf,thetxuclf,thecountry, thedom="",stext="",thelevel="All",startyear=2011,endyear=2040,
                            exclGI=FALSE,adjcons=FALSE,cons=0,seriesname = "New Capacity") {
  
  
  td = getunconstraint(thewater/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,cons/100)
  if(length(td[,1])==0) {return(NULL);}
  
  #seriesname = "New Capacity"
  tfinal = subset(td, series == seriesname)  
  units = as.character(tfinal$unit[1])
  
  if (thecountry!="All") {
    tfinal = subset(tfinal, country.name == thecountry)          
  }
  if (thelevel!="All") {
    tfinal = subset(tfinal, level == thelevel)          
  }
  tfinal = subset(tfinal, time %in% (seq(startyear,endyear,1)))          
  
  
  #x = unique(tfinal$time)
  #x = x[order(x)]
  
  if(nrow(tfinal)>0) {
    
    tfinal2 = tfinal[, c("time","value","energy.source"),with=F]
    tfinal3 = tfinal2[, lapply(.SD, sum), by = c("time","energy.source")]     
    tfinal3 = tfinal3[(tfinal3$time>2010) & (tfinal3$time<2050) ,]
    
    tdat = as.data.frame(t(reshape(tfinal3,idvar=c("energy.source"),direction="wide")),stringsAsFactors=F)
    colnames(tdat) = as.character(unlist(tdat[1,]))
    tdat = tdat[-1,]
    rownames(tdat) = gsub("value\\.","",rownames(tdat))      
    
    x = as.data.frame(apply(tdat,2,as.numeric),stringsAsFactors=F)
    rownames(x) = rownames(tdat)
    #print(x)
    
    nn = colnames(x)
    x2 = as.data.frame(x[,order(nn)])
    nn = nn[order(nn)]
    colnames(x2) = nn
    x = x2
    #print(x)
  } else {x = rep(0,9)}
  
  h1 <- rCharts:::Highcharts$new()
  h1$chart(type = "column",marginLeft=60,height=300)
  h1$title(text = paste(seriesname," (",thecountry,")",sep=""))
  h1$subtitle(text = paste(stext,sep=""))
  
  if(nrow(tfinal)>0) {
    h1$xAxis(categories = paste("",rownames(x),sep="") )
    h1$yAxis(title = list(text = units),stackLabels= list(enabled=T))
    h1$data(round(x,2))      
    # Print chart
  }
  
  
  cols = unlist(lapply(colnames(x), function(i) { switch(i,
                                                         "Coal"= "#7CB5EC",
                                                         "Gas" = "#434348",
                                                         "Nuclear" = "#90ED7D",
                                                         "Oil" = "#F7A35C",
                                                         "Pump Storage" = "#91E8E1",
                                                         "Biomass" = "#8085E9",
                                                         "Hydro" = "#F15C80",
                                                         "Solar PV" = "#E4D354",
                                                         "Solar Thermal" = "#8085E8",
                                                         "Wind" = "#8D4653") }))
  
  #h1$colors(cols)
  h1$legend(symbolWidth = 10)
  h1$set(dom = thedom)
  h1$plotOptions(animation=FALSE,
                 column=list(
                   #colorByPoint=TRUE,
                   #colors=cols,
                   stacking= 'normal',
                   animation=FALSE,
                   events=list(
                     legendItemClick = paste("#! function() {
                                             console.log(this);
                                             Shiny.onInputChange(\'",thedom,"LegendItemClick\', {
                                             name: this.name,
                                             visible: this.visible      
})
                                             } !#",sep="")
                     #legendItemClick = "#! function() {alert(this.name);  } !#"
                   )
                   )
                 )
  h1$exporting(enabled = T)    
  
  return(h1)       
}  

barunconstraintGV <- function(thewater,thecoaluclf,thetxuclf,thecountry, thedom="",stext="",thelevel="All",startyear=2011,endyear=2040,
                              exclGI=FALSE,adjcons=FALSE,cons=0,shinyid="",h=300,seriesname,year,charttype) {
  
  td = getunconstraint(thewater/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,cons/100)
  if(length(td[,1])==0) {return(NULL);}
  
  #seriesname = "New Capacity"
  #seriesname = input$d1seriesgv;
  
  tfinal = subset(td, series == seriesname)  
  units = as.character(tfinal$unit[1])
  
  #if (thecountry!="All") {
  #  tfinal = subset(tfinal, country.name == thecountry)          
  #}
  if (thelevel!="All") {
    tfinal = subset(tfinal, level == thelevel)          
  }
  
  if(nrow(tfinal)>0) {
    
    tfinal2 = tfinal[, c("time","value","country.name"),with=F]
    tfinal3 = tfinal2[, lapply(.SD, sum), by = c("time","country.name")]     
    tfinal3 = tfinal3[(tfinal3$time==year) ,]    # input$d1yeargv)  ,]
    
    tdat = as.data.frame(t(reshape(tfinal3,idvar=c("country.name"),direction="wide")),stringsAsFactors=F)
    colnames(tdat) = paste( as.character(unlist(tdat[1,])), as.character(unlist(tdat[2,])),sep=".")
    tdat = tdat[-1,]
    tdat = tdat[-1,]
    rownames(tdat) = gsub("value\\.","",rownames(tdat))      
    
    x = as.data.frame(apply(tdat,2,as.numeric),stringsAsFactors=F)
    rownames(x) = rownames(tdat)
    #print(x)
    
    nn = colnames(x)
    x2 = as.data.frame(x[,order(nn)])
    nn = nn[order(nn)]
    colnames(x2) = nn
    x = x2
    #print(x)
  } else {x = rep(0,9)}
  
  G1a = ""
  if (charttype=="GeoChart") {
    G1a <- gvisGeoChart(tfinal3, locationvar='country.name', colorvar='value', shinyid=shinyid,
                        options=list(gvis.editor= "Edit Graph", 
                                     region="002", 
                                     width="100%", height=h,
                                     title=seriesname
                        ))
  } else if (charttype=="PieChart") { 
    tfinal3 = tfinal3[, c("country.name","value"),with=F]
    G1a <-  gvisPieChart(tfinal3,shinyid=shinyid,
                         options=list(gvis.editor= "Edit Graph", 
                                      width="100%", height=h,
                                      title=seriesname
                         )
    )
  } else if (charttype=="LineChart") { 
    G1a <-  gvisLineChart(tfinal3, xvar="country.name", yvar=c("value"),shinyid=shinyid,
                          options=list(gvis.editor= "Edit Graph", 
                                       width="100%", height=h,
                                       title=seriesname
                          )
    )
  } else if (charttype=="AreaChart") {        
    G1a <-  gvisAreaChart(tfinal3, xvar="country.name", yvar=c("value"),shinyid=shinyid,
                          options=list(gvis.editor= "Edit Graph", 
                                       width="100%", height=300,
                                       title=seriesname
                          )
    )
  } else if (charttype=="ColumnChart") {
    G1a <- gvisColumnChart(tfinal3, xvar=c("country.name"), yvar=c("value"),shinyid=shinyid,
                           options=list(isStacked=TRUE,gvis.editor= "Edit Graph", 
                                        width="100%", height=h,
                                        title=seriesname))
  } else if (charttype=="BarChart") {
    G1a <- gvisBarChart(tfinal3, xvar=c("country.name"), yvar=c("value"),shinyid=shinyid,
                        options=list(isStacked=TRUE,gvis.editor= "Edit Graph", 
                                     width="100%", height=h,
                                     title=seriesname))
  } else { # Other
    
    G1a <- gvisGeoChart(tfinal3, locationvar='country.name', colorvar='value',shinyid=shinyid,
                        options=list(gvis.editor= "Edit Graph", 
                                     region="002", 
                                     width="100%", height=h,title=seriesname
                        ))
  }
  
  return(G1a)
}  


barunconstraintGVTS <- function(thewater,thecoaluclf,thetxuclf,thecountry, thedom="",stext="",thelevel="All",startyear=2011,endyear=2040,
                                exclGI=FALSE,adjcons=FALSE,cons=0,shinyid="",h=300,seriesname, charttypets) {
  
  td = getunconstraint(thewater/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,cons/100)
  if(length(td[,1])==0) {return(NULL);}
  
  #seriesname = "New Capacity"
  #seriesname = input$d1seriesgvts;
  
  tfinal = subset(td, series == seriesname)  
  units = as.character(tfinal$unit[1])
  
  if (thecountry!="All") {
    tfinal = subset(tfinal, country.name == thecountry)          
  }
  if (thelevel!="All") {
    tfinal = subset(tfinal, level == thelevel)          
  }
  
  G1a = NULL;
  
  tfinal2 = tfinal[, c("time","energy.source","value"),with=F]
  tfinal3 = tfinal2[, lapply(.SD, sum), by = c("time","energy.source")]     
  
  if((nrow(tfinal3)>0) && (!is.null(tfinal3[,"energy.source"])) ) {
    
    
    #tfinal3 = tfinal3[(tfinal3$time==input$d1yeargv)  ,]
    
    tdat = as.data.frame(t(reshape(tfinal3,idvar=c("energy.source"),direction="wide")),stringsAsFactors=F)
    colnames(tdat) = paste( as.character(unlist(tdat[1,])) ,sep=".")
    rownames(tdat) = gsub("value\\.","",rownames(tdat))      
    tdat$time = rownames(tdat)
    tdat = tdat[-1,]
    
    
    x = as.data.frame(apply(tdat,2,as.numeric),stringsAsFactors=F)
    rownames(x) = rownames(tdat)
    
    #print(x)
    onn=colnames(x)
    
    nn = colnames(x)
    x2 = as.data.frame(x[,order(nn)])
    nn = nn[order(nn)]
    colnames(x2) = nn
    x = x2
    #print(x)
    
    if (charttypets=="GeoChart") {
      G1a <- gvisBarChart(x, xvar="time", yvar=onn[1:(length(onn)-1)],shinyid=shinyid,
                          options=list(isStacked=TRUE,gvis.editor= "Edit Graph", 
                                       width="100%", height=h,
                                       title=paste(seriesname," (",thecountry,")",sep="") ))
    } else if (charttypets=="PieChart") { 
      G1a <- gvisBarChart(x, xvar="time", yvar=onn[1:(length(onn)-1)],shinyid=shinyid,
                          options=list(isStacked=TRUE,gvis.editor= "Edit Graph", 
                                       width="100%", height=h,
                                       title=paste(seriesname," (",thecountry,")",sep="") ))
    } else if (charttypets=="LineChart") { 
      G1a <-  gvisLineChart(x, xvar="time", yvar=onn[1:(length(onn)-1)],
                            shinyid=shinyid,
                            options=list(gvis.editor= "Edit Graph", 
                                         width="100%", height=h,
                                         title=paste(seriesname," (",thecountry,")",sep="") ))
      
      
    } else if (charttypets=="AreaChart") {        
      G1a <-  gvisAreaChart(x, xvar="time", yvar=onn[1:(length(onn)-1)],shinyid=shinyid,
                            options=list(gvis.editor= "Edit Graph", 
                                         width="100%", height=h,
                                         title=paste(seriesname," (",thecountry,")",sep="") ))
    } else if (charttypets=="ColumnChart") {
      G1a <- gvisColumnChart(x, xvar="time", yvar=onn[1:(length(onn)-1)],shinyid=shinyid,
                             options=list(isStacked=TRUE,gvis.editor= "Edit Graph", 
                                          width="100%", height=h,
                                          title=paste(seriesname," (",thecountry,")",sep="") ))
    } else if (charttypets=="BarChart") {
      G1a <- gvisBarChart(x, xvar="time", yvar=onn[1:(length(onn)-1)],shinyid=shinyid,
                          options=list(isStacked=TRUE,gvis.editor= "Edit Graph", 
                                       width="100%", height=h,
                                       title=paste(seriesname," (",thecountry,")",sep="") ))
    } else { # Other
      
      G1a <- gvisBarChart(x, xvar="time", yvar=onn[1:(length(onn)-1)],shinyid=shinyid,
                          options=list(isStacked=TRUE,gvis.editor= "Edit Graph", 
                                       width="100%", height=h,
                                       title=paste(seriesname," (",thecountry,")",sep="") ))
    }  
    
    
  } else {x = rep(0,9)}
  
  
  
  return(G1a)
}  



barconstraint <- function(designwater,designcoaluclf,designtxuclf,fixyear,thewater,thecoaluclf,thetxuclf,thecountry, 
                          thedom="",stext="",thelevel="All",startyear=2011,endyear=2040) {
  
  td = getconstraint(designwater,designcoaluclf,designtxuclf,fixyear,thewater/100, thecoaluclf/100,thetxuclf/100)
  if(length(td[,1])==0) {return(NULL);}
  
  seriesname = "New Capacity"
  tfinal = subset(td, series == seriesname)  
  units = as.character(tfinal$unit[1])
  
  if (thecountry!="All") {
    tfinal = subset(tfinal, country.name == thecountry)          
  }
  if (thelevel!="All") {
    tfinal = subset(tfinal, level == thelevel)          
  }
  tfinal = subset(tfinal, time %in% (seq(startyear,endyear,1)))          
  
  
  #x = unique(tfinal$time)
  #x = x[order(x)]
  
  if(nrow(tfinal)>0) {
    
    tfinal2 = tfinal[, c("time","value","energy.source"),with=F]
    tfinal3 = tfinal2[, lapply(.SD, sum), by = c("time","energy.source")]     
    tfinal3 = tfinal3[(tfinal3$time>2010) & (tfinal3$time<2050) ,]
    
    tdat = as.data.frame(t(reshape(tfinal3,idvar=c("energy.source"),direction="wide")),stringsAsFactors=F)
    colnames(tdat) = as.character(unlist(tdat[1,]))
    tdat = tdat[-1,]
    rownames(tdat) = gsub("value\\.","",rownames(tdat))      
    
    x = as.data.frame(apply(tdat,2,as.numeric),stringsAsFactors=F)
    rownames(x) = rownames(tdat)
  }
  
  h1 <- rCharts:::Highcharts$new()
  h1$chart(type = "column",marginLeft=50,height=300)
  h1$title(text = paste("New Capacity (",thecountry,")",sep=""))
  h1$subtitle(text = paste(stext,sep=""))
  
  if(nrow(tfinal)>0) {
    h1$xAxis(categories = paste("",rownames(x),sep="") )
    h1$yAxis(title = list(text = units),stackLabels= list(enabled=T))
    h1$data(x)      
    # Print chart
  }
  
  
  h1$legend(symbolWidth = 10)
  h1$set(dom = thedom)
  h1$plotOptions(animation=FALSE,column=list(stacking= 'normal',animation=FALSE))
  h1$exporting(enabled = T)    
  
  return(h1)       
}  





 af = readOGR(dsn="www/africa.geojson",layer = 'OGRGeoJSON') 
 thebb = bbox(af)
 geojson <- readLines("www/africa.geojson", warn = FALSE) %>%
   paste(collapse = "\n") %>%
   fromJSON(simplifyVector = FALSE)
 geojson$style = list(weight = 1,color = "#000000",opacity = 1,fillOpacity = 0) 
 gistranslate = list("Angola"="Angola","Botswana"="Botswana","Bur"="Burundi",
                     "Congo"="Congo-Brazzaville","Democratic Republic of Congo"="Congo DRC",
                     "Equatorial Guinea"="Equatorial Guinea",
                     "Gabon"="Gabon","Kenya"="Kenya","Lesotho"="Lesotho",
                     "Malawi"="Malawi","Mozambique"="Mozambique","Namibia"="Namibia",                     
                     "Rwanda"="Rwanda",  #"South Afirca"="South Africa",
                     "South Africa"="South Africa"
                     ,"Swaziland"="Swaziland","Tanzania"="Tanzania",
                     "Uganda"="Uganda","Zambia"="Zambia","Zimbabwe"="Zimbabwe")  

dt = readOGR(dsn="www/thedata.geojson",layer = 'OGRGeoJSON') 
dt2 = dt@data
dt2$ID = as.numeric(as.character(dt2$ID))
dt2$ID = dt2$ID + 1
dt@data = dt2

dtfrom = dt@data[,c("CODE","COUNTRY")]
names(dtfrom) = c("source","producing.country")
dtto = dt@data[,c("CODE","COUNTRY")]
names(dtto) = c("target","consuming.country")  

load("data/sample.Rdata")
series1 = df; # get series names masterdata
series1[series1$unit=="MWyr",]$value = series1[series1$unit=="MWyr",]$value * 8.76581277 
series1[series1$unit=="MWyr",]$unit = "GWh"

#load("data/run.masterdata.rdata")
#load("data/run.masterdata_11_12.rdata") 
load("data/run.masterdata_14_15.rdata")
#runMasterdata = as.data.table(runMasterdata)

nodes <- subset(ref_objects, show=='yes')

uniqueAndSorted <- function(col){
  u <- unique(as.character(col))      
  sort(u) 
}

fix.status <- function(col){
  col <- as.character(col)
  col[col=="N/A"] <- "Active"
  col[col=="UC"] <-  "Active"
  col
}

countries <- uniqueAndSorted(nodes$country.name)
level <- uniqueAndSorted(nodes$level)
nodes$status <- fix.status(nodes$status)

idedata = read.csv("data/20150821_input_explorer_dataset.csv")
idedata$value = as.numeric(as.character(idedata$value))
#ideseries = read.csv("data/20150821_masterdata_series.csv")
#ideobjects = read.csv("data/20150821_masterdata_objects.csv")



# Avg Price Diff - Side by Side Bars (Map Icon Graph)
plotIcons2 = function(values, pch = 0:14, year=2015, width = 30, height = 30, type="stacked", ...) {
  n = length(pch)
  files = character(n)
  # create a sequence of png images
  themin = 0
  themax = 0
  
  if(type=="sidebar") {
    for (i in seq_len(n)) {
      val = values[[i]]
      lmin = min(unlist(lapply(val,function(x) {x$data})))
      lmax = max(unlist(lapply(val,function(x) {x$data})))
      if(lmin<themin) {
        themin = lmin
      }
      if(lmax>themax) {
        themax = lmax
      }    
    }  
  }
  
  for (i in seq_len(n)) {
    f = tempfile(fileext = '.png')        
    val = values[[i]]
    cols=NULL;
    
    if(type=="sidebar") {
      png(f, width = width, height = height, bg = 'transparent')        
      par(mar = c(1, 4, 1, 0))
      if(!is.null( unlist(lapply(val,function(x) {x$data})) )) {
        barplot(unlist(lapply(val,function(x) {x$data})),col=c(4,1), ylim=c(themin-5,themax+5),ylab="%",las=1,cex.lab=1,
                cex.axis= 0.75)
      } else {
        plot(c(1,2),type="n",xaxt="n",yaxt="n",ylab="",xlab="",bty="n")
      }
      dev.off()        
    } else if(type=="stacked") {
      
      if(length(val)>0) {
        cols = unlist(lapply(names(val), function(x) { switch(x,
                                                              "Coal"= "black",
                                                              "Gas" = "chartreuse2",
                                                              "Nuclear" = "darkorchid",
                                                              "Oil" = "firebrick3",
                                                              "Pump Storage" = "gold1",
                                                              "Biomass" = "dodgerblue",
                                                              "Hydro" = "darkgoldenrod2",
                                                              "Solar PV" = "darkorchid3",
                                                              "Solar Thermal" = "darkred",
                                                              "Wind" = "cyan1") }))
      }
      
      png(f, width = width, height = height, bg = 'transparent')        
      par(mar = c(1, 4, 1, 0))
      
      if(sum( as.numeric(as.character(unlist(val))) )>0) {
        barplot(do.call(rbind,val),  
                col=cols,las=1,cex.names= 0.75,cex.axis= 0.75,
                main= paste("'",substr(as.character(year),3,4),sep=""),
                cex.main=0.8,ylab="MW",cex.lab=0.75 )
      } else {
        plot(c(1,2),type="n",xaxt="n",yaxt="n",ylab="",xlab="",bty="n")
      }
      dev.off()        
    }
    
    files[i] = f
  }
  files
}   

