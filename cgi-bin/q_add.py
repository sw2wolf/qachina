#!/usr/local/bin/python
# -*- coding: utf-8 -*-
from common import *
from dao import *
from auth import *

#check_user(user_id)

(q_typeid, q_code) = get_q_type()

view_template = '''<html><head>%(head)s</head><body><form method='POST' action='q_add.py'>
<input type='hidden' name='cmd' value='add'>
<input type='hidden' name='typeid' value=%(typeid)s>
<input type='hidden' name='start' value=%(start)s>
<table width='100%%' align='center' border='0' cellpadding='0' cellspacing='0'>
<tr><td align='right'>Category</td><td>%(code)s</td></tr>
<tr><td align='right'>Subject</td><td><input type='text' name='subject' maxlength='60' size='60'></td></tr>
<tr><td align='right'>Due Days</td><td><input type='text' name='due_day' maxlength='10' size='10' value='100'></td></tr>
<tr><td align='right'>Quoted Price</td><td><input type='text' name='price' maxlength='10' size='10' value='100'>Yuan
	(<font color='red'>must be greater than 10</font>)</td></tr>
<tr><td align='right'>Top it</td><td><input type='checkbox' name='is_top' checked=true></td></tr>
<tr><td align='right'>Content</td>
    <td><textarea name='content' cols='85%%' rows='19' wrap='soft' style=font-style:normal;font-size:12pt></textarea>
	</td></tr>
<tr><td colspan='2'>&nbsp;</td></tr>
<tr><td colspan='2' align='center'>
<input type='submit' value='Add'>&nbsp;&nbsp;<input type='button' value='Back' onclick="window.history.back();">
</td></tr></table></form></body></html>'''
	
cmd = form.getvalue("cmd")

if cmd != 'edit':
	option_1 = '''<option value='%s' selected>%s</option>'''
	option_2 = '''<option value='%s'>%s</option>'''

	code_select = '''<select name='code'>'''
	for c in q_type[q_typeid]:
		if c[0] == q_code:
			code_select += option_1 % (c[0], c[1])
		else:
			code_select += option_2 % (c[0], c[1])
	code_select += '</select>'	

from dao import Question
qs = Question()
if cmd == 'view':
	print2( view_template % {"head":head_ctype, "typeid":q_typeid, "code":code_select,"start":form.getvalue("start")} )
elif cmd == 'add':
	try:
		price = int( form.getvalue("price") )
	except:
		show_info("Must be greater than 10")
	if price < 10:
		show_info("Must be greater than 10")
	
	try:
		due_day = int( form.getvalue("due_day") )
	except:
		show_info("Due days must be an integer")
		
	subject = my_escape( form.getvalue("subject")[0:50] )
	content = my_escape( form.getvalue("content") )
	if subject is None or content is None:
		show_info("Subject/Content mustnot be empty")
		
	if form.getvalue('is_top'):
		is_top = 1
	else:
		is_top = 0
	sql = qs.add((q_typeid, form.getvalue('code'), subject, content, \
		user_id, now(), now_inc(due_day), 'open', price, is_top))
	show_info( postsave_view % (INFO_SAVE_OK, q_typeid, q_code, int(form.getvalue("start"))) )
elif cmd == 'edit':
	if form.getvalue('content') is None:
		show_info("content mustnot be empty")
	content = my_escape( form.getvalue('content') )
	qs.update(form.getvalue('qid'), content)
	show_info( postsave_view % (INFO_SAVE_OK, q_typeid, q_code, int(form.getvalue("start")) ) )

