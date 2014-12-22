<script language="javascript">
//**************************************************
//作者：云凤生(hainanyun@hotmail.com)
//创建日期：2005-3-23
//最后修改日期：2005-4-2
//功能：在页面中提交查询动作后，出现进度条，直到
//  新的页面的数据全部load出来。
//**************************************************
 function onSubmit() {
  var waitingInfo = document．getElementById(getNetuiTagName("waitingInfo"));
  waitingInfo.style.display = ""; //show the ProgressBar
  progress_update();    //begin the progressbar
  //waiting for several seconds,you would delete in your case
  for(i=0;i<1000000;i++){
   j=i+i;
  }
  location.href="progressbar.jsp"; //config your action page in here
 }


 // Build the netui_names table to map the tagId attributes
 // to the real id written into the HTML
 if (netui_names == null)
  var netui_names = new Object();
 netui_names.selectButton="portlet_15_1selectButton"
 // method which will return a real id for a tagId
 function getNetuiTagName(id) {
  return netui_names[id];
 }

 // method which will return a real id for a tagId,
 // the tag parameter will be used to find the scopeId for
 // containers that may scope their ids
 function getNetuiTagName(id, tag) {
  var scopeId = getScopeId(tag);
  if (scopeId == "")
     return netui_names[id];
  else
     return netui_names[scopeId  + "__" + id];
 }

 // method which get a tag will find any scopeId that,
 // was inserted by the containers
 function getScopeId(tag) {
    if (tag == null)
       return "";
    if (tag.getAttribute) { 
       if (tag.getAttribute('scopeId') != null)
          return tag.getAttribute('scopeId');
    } 
    if (tag.scopeId != null)
       return tag.scopeId;
    return getScopeId(tag.parentNode);
 }

 // Build the netui_names table to map the tagId attributes
 // to the real id written into the HTML
 if (netui_names == null)
    var netui_names = new Object();
 netui_names.waitingInfo="waitingInfo"

 var progressEnd = 15;  // set to number of progress <span>'s.
 var progressColor = 'green'; // set to progress bar color
 var progressInterval = 200; // set to time between updates (milli-seconds)

 var progressAt = progressEnd;
 var progressTimer;
 function progress_clear() {
  for (var i = 1; i <= progressEnd; i++)  
  document．getElementById('progress'+i).style.backgroundColor = 'transparent';
  progressAt = 0;
 }
 function progress_update() {
  progressAt++;
  if (progressAt > progressEnd) progress_clear();
  else document．getElementById('progress'+progressAt).style.backgroundColor = progressColor;
  progressTimer = setTimeout('progress_update()',progressInterval);
 }
 function progress_stop() {
  clearTimeout(progressTimer);
  progress_clear();
 }
</script>



<span id="waitingInfo" style="display:none">
<table align="center"><tr><td>
正在处理数据, 请稍候......
<div style="font-size:2pt;padding:2px;border:solid black 1px">
<span id="progress1">   </span>
<span id="progress2">   </span>
<span id="progress3">   </span>
<span id="progress4">   </span>
<span id="progress5">   </span>
<span id="progress6">   </span>
<span id="progress7">   </span>
<span id="progress8">   </span>
<span id="progress9">   </span>
<span id="progress10">   </span>
<span id="progress11">   </span>
<span id="progress12">   </span>
<span id="progress13">   </span>
<span id="progress14">   </span>
<span id="progress15">   </span>
</div>
</td></tr></table>
</span>

<center>
点击按钮试试：
<input name="button" type="button" value="查询" onClick="javascript：return onSubmit();"> 
</center>

