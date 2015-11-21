ctype = component$type
# Read component template code
thecode <- readLines(paste('components/',ctype,'.R',sep=''))
thecode2 <- readLines('generic.R')

uuid <- "";
print(componentfields)
# Replace place holders...
lapply(c("uuid",componentfields),function(field) {
  
  if(field=="uuid") {
    replaceval = digest(paste(component,rnorm(1)))
    uuid <<- replaceval;
  } else if(field=="width") {
    replaceval = component[as.character(field)]
    if(is.na(replaceval)) {
      if(length(thecode[grepl("width=\\d",thecode)])>0) {
        replaceval=as.character(as.numeric(strsplit(strsplit(thecode[grepl("width=\\d",thecode)],"#")[[1]][1],"=")[[1]][2]))
      } 
    }
  } else if(field=="height") {
    replaceval = component[as.character(field)]
    if(is.na(replaceval)) {
      if(length(thecode[grepl("height=\\d",thecode)])>0) {
        replaceval=as.character(as.numeric(strsplit(strsplit(thecode[grepl("height=\\d",thecode)],"#")[[1]][1],"=")[[1]][2]))
      }
    }
  } else {
    replaceval = component[as.character(field)]
  }
  thecode <<- gsub( paste("DASHBOARD.",field,sep=""), replaceval, thecode )
  thecode2 <<- gsub( paste("DASHBOARD.",field,sep=""), replaceval, thecode2 )
})

#print(paste('component-instances/',ctype,component$id,'.R',sep=''))
#print(thecode);

# Create Instance
file.create(paste('component-instances/',ctype,component$id,'.R',sep=''),overwrite=TRUE)
cat(thecode, file=paste('component-instances/',ctype,component$id,'.R',sep=''), sep="\n")
cat(thecode2, file=paste('component-instances/',ctype,component$id,'.R',sep=''), sep="\n",append=TRUE)

# Run Instance returning ui and start observe({}) blocks
#withReactiveDomain(session, eval(parse(text="source(paste('component-instances/',component$type,component$id,'.R',sep=''),local=TRUE) ")) )
js = ""
minheight=400
minwidth=400
source(paste('component-instances/',component$type,component$id,'.R',sep=''),local=TRUE)
resizejs <- gsub( paste("DASHBOARD.minheight",sep=""), minheight, resizejs )
resizejs <- gsub( paste("DASHBOARD.minwidth",sep=""), minwidth, resizejs )
cat(resizejs, file=paste('component-instances/resize',ctype,component$id,'.js',sep=''), sep="\n")
if(js!="") {
  cat(js, file=paste('component-instances/resize',ctype,component$id,'.js',sep=''), sep="\n",append=TRUE)
}


ui = tags$div(id=paste("component2",uuid,sep=""),style="overflow:hidden;",ui,
                 includeScript(paste('component-instances/resize',ctype,component$id,'.js',sep='')))

