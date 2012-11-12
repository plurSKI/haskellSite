<html xmlns="http://www.w3.org/1999/xhtml">
  <apply template="page-head" />
  <script type="text/javascript" 
            src="js/postPreview.js"> 
  </script>
  <body>
    <div id="framecontent">
      <apply template="side-menu" />
    </div>
    <div id="maincontent">
     <div class="innertube">
       <table class="contentTable" cellpadding="2" cellspacing="2" width="100%" height="100%">
          <td class="tdContent">
           <div id="postPreview">
           </div>
          </td>
       </table>
       <BR /><BR />
       <img src="imgs/rl.png" width="100%" /> 
       <BR /><BR />
       <form action="admin-addpost" method="POST">
         <table>
            <tr>
              <td>Id: </td>
              <td><input type="text" id="postId" name="postId" /></td>
              <td>&nbsp;&nbsp;&nbsp;&nbsp;</td>
              <td>Title: </td>
              <td><input type="text" id="postTitle" name="postTitle" /></td>
            </tr>
            <tr>
              <td>Ranking: </td>
              <td>
<select name="rating">
  <option value="1">1 (Most Visible)</option>
  <option value="2">2</option>
  <option value="3">3</option>
  <option value="4">4</option>
  <option value="5">5</option>
  <option value="6">6</option>
  <option value="7">7</option>
  <option value="8">8</option>
  <option value="9">9</option>
  <option value="10">10 (Least Visible)</option>
</select>
</td>
              <td>&nbsp;&nbsp;&nbsp;&nbsp;</td>
              <td>Visible: </td>
              <td>
                 <select id="visible" name="visible">
                   <option value="True">True</option>
                   <option value="False">False</option>
                 </select>
              </td>
</tr>
<tr>
              <td>Thumbnail: </td>
              <td colspan="4"><input type="text" size="75" id="postImage" name="postImage" /></td>
            </tr>
            <tr>
              <td>Tags: </td>
              <td colspan="4"><input type="text" size="75" id="tags" name="tags" /></td>
            </tr>
            <tr>
              <td>Summary: </td>
              <td colspan="4"><input type="text" size="75" id="postSummary" name="postSummary" /></td>
            </tr>
         </table>
                <textarea name="postContent" id="postContent" rows="30" 
                          cols="100">
                </textarea><BR />
              <input type="submit" value="Submit" />
              <input type="button" onClick="makePreview()" value="Preview" />
       </form>
     </div>
    </div>
  </body>
</html>
