#!/usr/local/bin/python
# -*- coding: utf-8 -*-
from common import *
from auth import *
from dao import *

find_view = """<form method='POST' action='q_find.py'>
	<input type='hidden' name='cmd' value='go'>
	<input type='hidden' name='typeid' value=%s>
	<input type='hidden' name='code' value=%s>
	<table width='100%%' align='center' border='0' cellpadding='0' cellspacing='0'>
		<tr><td align='right'>subject contains</td><td><input type='text' name='subject' maxlength='30' size='30'></td></tr>
		<tr><td align='right'>quoted price beyond</td><td><input type='text' name='price' maxlength='10' size='10'>Yuan</td></tr>
		<tr><td align='right'>status</td><td>
			<select name='state'><option value='open'>open</option>
			<option value='close'>close</option></select>
		</td></tr>
		<tr><td colspan='2' align='center'>
			<input type='submit' value='Go'>&nbsp;<input type='button' value='Back' onclick="window.history.back();">
		</td></tr>
	</table></form>"""

(q_typeid, q_code) = get_q_type()

cmd = form.getvalue('cmd')
if cmd == 'go':
	subject = form.getvalue("subject")
	try:
		price = int( form.getvalue("price") )
	except:
		price = 0
	state = form.getvalue("state")
	qobj = Question()
	data = qobj.findByData(q_typeid, q_code, subject, price, state)

	from q_common import *
	showQList(data, q_typeid, q_code, "")
else:
	print2(find_view % (q_typeid, q_code))
