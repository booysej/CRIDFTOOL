$(document).ready(function() { 

 function getSetWidth(d) {
        $('.layer').width(d);  
        return d;
  }
  function getSetHeight(d) {
        $('.layer').height(d); 
        return d;
  }

 $('#DASHBOARD.uuid').resizable({
      minHeight: 300,
      resize: function( event, ui ) {
        Shiny.onInputChange('dashboard_DASHBOARD.uuid_height',ui.element.context.clientHeight);
        Shiny.onInputChange('dashboard_DASHBOARD.uuid_width',ui.element.context.clientWidth);
        Shiny.onInputChange('dashboard_DASHBOARD.uuid_left',ui.element.context.offsetLeft);
        Shiny.onInputChange('dashboard_DASHBOARD.uuid_top',ui.element.context.offsetTop);

        $('#componentDASHBOARD.uuid').css({
                'left' : $(this).position().left,
                'top' : $(this).position().top,
                'width' : getSetWidth( $(this).width()-40 ),
                'height' : getSetHeight( $(this).height()-40 )
            });
      }
    });

});
