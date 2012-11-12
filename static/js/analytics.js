// alert("window.location: " + window.location);

var loc = new String(window.location);
var analytics_id = 'UA?';

if ( loc.indexOf("devrand", 0) >= 0) analytics_id = 'UA-8826810';

//alert(analytics_id);
var _gaq = _gaq || [];
_gaq.push(['_setAccount', analytics_id]);
_gaq.push(['_trackPageview']);

(function() {
   var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
   ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
   var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
})();

