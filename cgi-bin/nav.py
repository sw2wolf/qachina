#!/usr/local/bin/python
# -*- coding: utf-8 -*- 
from common import *
from auth import *

login_view = """<a href='login.py?cmd=view' target='main'>登录</a>"""
logout_view = """
	<a href='user.py?cmd=edit&uid=%s' target='main'>帐户管理</a>&nbsp;
	<a href='my_tx.py' target='main'>交易明细</a>&nbsp;
	<a href='../deposit.htm' target='main'>充值</a>""" % user_id

nav_view = """<html><head>%(head)s <title>%(title)s</title> </head><body bgcolor='#E4E4E4'>
	<table width='100%%' align='center' border='0' cellpadding='0' cellspacing='0'>
	<tr><td>
		<a href='user.py?cmd=add' target='main'>开户</a>&nbsp; %(view)s
	</td>
	<td><a href='../help.htm' target='main'>帮助</td></tr>
	<tr><td>&nbsp;</td></tr>
	<tr><td colspan='2' align='right'>Copyright ? 2005 ZSoft Inc. All Rights Reserved</td></tr>
	</table></body></html>"""

if user_id is None:
	print2(nav_view % {'head':head_ctype, 'title':qa_title, 'view':login_view})
else:
	print2(nav_view % {'head':head_ctype, 'title':qa_title, 'view':logout_view})
