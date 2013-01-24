#!/usr/local/bin/python
# -*- coding: utf-8 -*-
from common import *
from dao import *
from auth import *

login_view = '''<html><body><form method='POST' action='login.py'>
	<table width='30%%' align='center' border='0' cellpadding='0' cellspacing='0'>
	<tr><td align='right'>Name</td><td><input type='text' name='userid' value='%s' maxlength='20' size='20'></td></tr>
	<tr><td align='right'>Password</td><td><input type='password' name='userpwd' value='%s' maxlength='20' size='20'></td></tr>
	<tr><td colspan='2'>&nbsp;</td></tr>
	<tr><td colspan='2' align='center'>
	<input type='submit' value='Login'>&nbsp;&nbsp;<input type='button' value='Back'>
	<input type='hidden' name='cmd' value='in'>
	</td></tr> </table></form></body></html>'''

refresh_view = """<html><head>
	<script>setTimeout("this.parent.location.replace('../index.htm')", 0);</script> 
	</head><body></body></html>"""
	
def mk_cookie(uid, passwd):
	import Cookie
	C = Cookie.SimpleCookie()
	C['userid'] = uid
	C['userid']['max-age'] = '72000';
	C['userpwd'] = passwd
	C['userpwd']['max-age'] = '72000';
	return C.output()

def rm_cookie(uid):
	import Cookie
	C = Cookie.SimpleCookie()
	C['userid'] = uid
	C['userid']['max-age'] = time.time() - 100;
	return C.output()

cmd = form.getvalue("cmd")
if cmd == 'in':
	uid = form.getvalue('userid')
	passwd = form.getvalue('userpwd')
	user = User()
	if user.auth(uid, passwd):
		print mk_cookie(uid, passwd)
		print2(refresh_view)
	else:
		show_info(ERR_USER)
else:
	if user_id is None:
		print2(login_view % ('midas', '123'))
	else:
		print2(login_view % (user_id, user_pwd))
