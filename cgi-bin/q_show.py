#!/usr/local/bin/python
# -*- coding: utf-8 -*-
from common import *
from dao import *
from auth import *

check_user(user_id)

subject_view = '''<tr><td colspan='2' align='center' bgcolor='#DFEBFF'>%s</td></tr>'''

content_view = '''<tr bgcolor='#E4E4E4'><td>%s<br>%s</td><td>%s</td></tr>'''
content_edit = '''<form method='post' action='q_add.py'>\
<tr><td>%(author)s<br>%(openDate)s</td>
<td><textarea name='content' cols='85%%' rows='20' wrap='soft' style=font-style:normal;font-size:12pt>
%(content)s</textarea></td></tr>
<tr><td colspan='2' align='center' bgcolor='#E4E4E4'>&nbsp;%(toolbar)s
<input type='hidden' name='cmd' value='edit'>
<input type='hidden' name='qid' value='%(qid)s'>
<input type='hidden' name='typeid' value='%(typeid)s'>
<input type='hidden' name='code' value='%(code)s'>
<input type='hidden' name='start' value='%(start)s'>
</td></tr></form>'''
content_edit_cmd = '''<input type='submit' value='save'>&nbsp;&nbsp;<a href='q_close.py?qid=%s&cmd=view'>close</a>'''

answer_view = '''<tr bgcolor='#F4EAD7'><td>%s<br>%s</td><td>%s</td></tr>'''
answer_edit = '''<form action='a_add.py'><tr><td>My answer</td><td>
	<textarea name='content' cols='80' rows='20' wrap='soft'>%(content)s</textarea></td></tr>
	<tr><td colspan='2' align='center' bgcolor='#E4E4E4'>
	<input type='hidden' name='qid' value='%(qid)s'>
	<input type='hidden' name='typeid' value='%(typeid)s'>
	<input type='hidden' name='code' value='%(code)s'>
    <input type='hidden' name='start' value='%(start)s'>
	<input type='hidden' name='aid' value='%(aid)s'>%(toolbar)s</td></tr></form>'''
answer_edit_cmd = \
	'''<input type='submit' value='answer'>&nbsp;&nbsp;<input type='button' value='Back' onclick="window.history.back();">'''

qid = form.getvalue('qid')
q = Question()
qdata = q.load(qid)[0]
a = Answer()
answers = a.query(qid)

print2('''<html><head>%(head)s<title>%(title)s</title></head>''' % {'head':head_ctype,"title":qa_title})
print('''<body><table width='100%%' align='center' border='1' cellpadding='0' cellspacing='0'>''')
print subject_view % qdata[q_f_subject]
content = escape(qdata[q_f_content])
if qdata[q_f_state] == 'open':
	q_edit_cmd = content_edit_cmd % qid
	a_edit_cmd = answer_edit_cmd
else:		
	q_edit_cmd = INFO_CLOSED
	a_edit_cmd = INFO_CLOSED
		
if user_id == qdata[q_f_author]:
	print( content_edit % {"author":qdata[q_f_author], "openDate":qdata[q_f_open_date], \
		"content":content, "toolbar":q_edit_cmd, "qid":qid, \
		"typeid":qdata[q_f_typeid], "code":qdata[q_f_code], "start":form.getvalue("start")} )
	for answer in answers:
		a_content = escape(answer[a_f_content]).replace('\n', '<br>')
		print(answer_view % (answer[a_f_author], answer[a_f_answer_date], a_content))
else:
	content = content.replace('\n', '<br>')
	print(content_view % (qdata[q_f_author], qdata[q_f_open_date], content))
	answer = filter(lambda x: x[a_f_author] == user_id, answers)
	if len(answer) > 0:
		a = answer[0]
		print(answer_edit % {"content":a[a_f_content], "qid":'', "aid":a[a_f_aid], \
			"toolbar":a_edit_cmd, "typeid":qdata[q_f_typeid], "code":qdata[q_f_code], "start":form.getvalue("start")})
	else:
		print(answer_edit % {"content":'', "qid":qid, "aid":'', \
			"toolbar":a_edit_cmd, "typeid":qdata[q_f_typeid], "code":qdata[q_f_code], "start":form.getvalue("start")})
print('''</table></body></html>''')
