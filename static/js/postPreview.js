function makePreview()
{
  var newHtml = "url: " + $("#postId").val() + "<BR>";
  newHtml += "<img src=\"" + $("#postImage").val() + "\" />";
  newHtml += "<H1>" + $("#postTitle").val() + "</H1>";
  newHtml += $("#postContent").val();
  $("#postPreview").html(newHtml);
}
