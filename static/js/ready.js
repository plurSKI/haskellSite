var clearedField = {};

jQuery().ready(function(){
        $('.slideshow').cycle({fx: 'fade',random: 1});
	jQuery('#navigation').accordion({
		active: false,
		header: '.head',
		event: 'mouseover',
		fillSpace: true,
		animated: 'easeslide'
	});
	$("a[rel='imagePop']").colorbox({height:"900px", width:"900px"});
});

function clearField(x)
{
  if(!clearedField[x])
  { 
    $("#" + x).val("");
    $("#" + x).html("");
    $("#" + x).css("color", "#000000");
    clearedField[x] = true;
  }
}

function postComment()
{
  var name = $("#name").val();
  var email = $("#email").val();
  var post = $("#postId").html();
  var captcha = $("#captcha").val();
  var timestamp = $("#timestamp").html();
  var timestampG = $("#timestampGiven").html();
  var comment = $("#comment").val();
  

  $.post("/addComment", {post: post, name: name, email: email, captcha: captcha, timestamp: timestamp, timestampG: timestampG, comment: comment },
    function(data){
      var newHtml = '<b>' + name + ' said:</b>' 
                    + '<div class="userComment">' + data + '</div>';
      $("#commentArea").html(newHtml);
  });

  $("#unHREF").html("");
  //$("#commentArea").html(newHtml);
}

function commentExpand()
{
    var commentForm = '\
         <BR /><table border="0">  \
         <tr> \
           <td>Name:</td> \
           <td><input size="25" id="name" onFocus="clearField(\'name\')" value="Required" style="color: #FF0000;"></td> \
           <td>E-mail:</td> \
           <td><input size="25" id="email" onFocus="clearField(\'email\')" value="Optional (Will not be shown)" style="color: #888888;"></td> \
         </tr><tr> \
           <td valign=top>Comment:</td> \
           <td colspan="3"><textarea rows="10" cols="100" id="comment" style="color: #FF0000;" onFocus="clearField(\'comment\')">Required</textarea></td> \
         </tr><tr> \
           <td></td> \
           <td><BR /><input type="button" value="Post Comment" onClick="postComment()" /></td> \
         </tr></table>';

    $("#commentArea").html(commentForm);
    $("#unHREF").html("- Add a comment");
}

function pageNav( x )
{
  var newUrl = $("#pageSel" + x).val();
  window.location = newUrl;
}
