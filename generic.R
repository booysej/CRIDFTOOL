############ Inject Common Generic Code #################
#########################################################
# Update Dashboard SIZE global structure (on Resize)
pos=length(observerpool[["DASHBOARD.uuid"]])+1 # Add to last element of observer pool
observerpool[["DASHBOARD.uuid"]][[pos]] <- observe({
  print(input$dashboard_DASHBOARD.uuid_height);
  print(input$dashboard_DASHBOARD.uuid_width);
  print(input$dashboard_DASHBOARD.uuid_order);
  
  isolate({
    thedash = as.character(values$dashboard)
    thev = dashboardconfig[[thedash]]
    if (length(thev)>0) {
      tf = unlist(lapply(thev,function(x){x$id==DASHBOARD.id}))
      if (length(tf)>0) {
        if(!is.null(input$dashboard_DASHBOARD.uuid_width)) { thev[tf][[1]]$width <- input$dashboard_DASHBOARD.uuid_width; }
        if(!is.null(input$dashboard_DASHBOARD.uuid_height)) { thev[tf][[1]]$height <- input$dashboard_DASHBOARD.uuid_height; }
      }
      
      if(!is.null(input$dashboard_DASHBOARD.uuid_order)) {
        current = unlist(lapply(thev,function(x){x$id}))
        newlist = input$dashboard_DASHBOARD.uuid_order
        thev = thev[match(current,newlist)]
        print(thev)
      }
      
    } else {
      thev = list()
    }
    dashboardconfig[[thedash]] <<- thev
    print(thev)
  });
})

####### Dynamic Javascript ##################
resizejs = "$(document).ready(function() { 

 function getSetWidth(d) {
        $('.layer').width(d);  
        return d;
  }
  function getSetHeight(d) {
        $('.layer').height(d); 
        return d;
  }

 $('#DASHBOARD.uuid').resizable({
      minHeight: DASHBOARD.minheight,
      minWidth: DASHBOARD.minwidth,
      resize: function( event, ui ) {
        Shiny.onInputChange('dashboard_DASHBOARD.uuid_height',ui.element.context.clientHeight);
        Shiny.onInputChange('dashboard_DASHBOARD.uuid_width',ui.element.context.clientWidth);
        Shiny.onInputChange('dashboard_DASHBOARD.uuid_left',ui.element.context.offsetLeft);
        Shiny.onInputChange('dashboard_DASHBOARD.uuid_top',ui.element.context.offsetTop);
        
        $('#component2DASHBOARD.uuid').css({
                'left' : $(this).position().left,
                'top' : $(this).position().top,
                'width' : getSetWidth( $(this).width()-20 ),
                'height' : getSetHeight( $(this).height()-30 )
            });

        $('#componentDASHBOARD.uuid').css({
                'left' : $(this).position().left,
                'top' : $(this).position().top,
                'width' : getSetWidth( $(this).width()-20 ),
                'height' : getSetHeight( $(this).height()-30 )
            });
         


      }
    });

});
"

if(!exists("width")) {
  width=400 # default
}
if(!exists("height")) {
  height=400 # default
}