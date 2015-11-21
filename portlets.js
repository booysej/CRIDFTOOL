$(document).ready(function() {
  

$('.sortable').sortable({
      update: function( event, ui ) {
        a = $('.sortable').sortable('toArray');
        a.pop();
        b = a.map(function(item) {
          it = $('#'+item);
          if(it!==undefined) {
            if(it.data()!==undefined) {
              return(it.data().id);
            } else {
              return(null);
            }
          } else {
            return(null);
          }
        });
        Shiny.onInputChange('dashboard_'+ui.item.context.id+'_order',b);
        Shiny.onInputChange('dashboard_order',b);
      }
}).disableSelection(); 



$('.portlet')
    .addClass('ui-widget ui-widget-content ui-corner-all')
    .find('.portlet-header')
        .addClass('ui-widget-header ui-corner-all')
        .prepend('<span class="ui-icon ui-icon-minusthick"></span>')
    .end();


$(".portlet-header .ui-icon-minusthick").click(function () {
    if($(this).hasClass("ui-icon-minusthick") === true) {
        $(this).removeClass("ui-icon-minusthick");
        $(this).addClass("ui-icon-plusthick");
        $(this).parents(".portlet:first").find(".portlet-content").slideToggle();
        $(this).parents(".portlet:first").animate({height: "40px"}, 500).resizable( "disable" );
    }
    else
    {
        $(this).removeClass("ui-icon-plusthick");
        $(this).addClass("ui-icon-minusthick");
        $(this).parents(".portlet:first").animate({height: "100%"}, 500).resizable( "enable" );
        $(this).parents(".portlet:first").find(".portlet-content").slideToggle();
    }
});

/*$('.portlet-header .ui-icon').on('click', function() {
    $(this).toggleClass('ui-icon-minusthick ui-icon-plusthick');
    //$(this).closest('.portlet').toggleClass('portlet-minimized');
     //$(this).parents(".portlet:first").find(".portlet-content").toggle();
     $(this).parents(".portlet:first").find(".portlet-content").toggle();
});*/ 

/* $('.rescontainer').resizable({
      minHeight: 300,
      alsoResize: ".cridfmap"
    }); 
    */



});
