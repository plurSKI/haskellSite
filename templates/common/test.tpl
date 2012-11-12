<bind tag="foo">BAR</bind>
<html xmlns="http://www.w3.org/1999/xhtml">
  <body>
    <H1 faz="$(foo)">$(foo)</H1>
    <debug faz="$(foo)"> </debug> 
    <menuList>popular</menuList> 
    <menuList>recent</menuList> 
  </body>
</html>
