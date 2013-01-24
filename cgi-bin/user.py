#!/usr/local/bin/python
# -*- coding: utf-8 -*-
from common import *
from dao import *

cmds = ('add', 'edit', 'save')
cmd = form.getvalue('cmd')
if cmd not in cmds:
	raise 'invalidate command:%s' % cmd
	
uid_add_view = """<input type='text' name="uid" maxlength='10' size='10' value='%s'>"""
uid_edit_view = """<input type='text' name="uid" maxlength='10' size='10' value='%s' readonly>"""
user_view = '''<form method='POST' action='user.py'>
	<input type='hidden' name='cmd' value='save'>
	<table width='100%%' align='center' border='0' cellpadding='0' cellspacing='2'>
	<tr><th align='center' colspan='2'>%s</th></tr>
	<tr><td align='right'>帐号</td><td>%s</td></tr>
	<tr><td align='right'>呢称</td><td><input type='text' name='uname' maxlength='20' size='20' value='%s'></td></tr>
	<tr><td align='right'>口令</td><td><input type='password' name='pwd1' maxlength='10' size='10' value='%s'></td></tr>
	<tr><td align='right'>&nbsp;</td><td><input type='password' name='pwd2' maxlength='10' size='10' value='%s'></td></tr>
	<tr><td align='right'>邮件</td><td><input type='text' name='email' maxlength='30' size='30' value='%s'></td></tr>
	<tr><td align='right'>电话</td><td><input type='text' name='phone' maxlength='20' size='20' value='%s'></td></tr>
	<tr><td align='right'>手机</td><td><input type='text' name='mobile' maxlength='20' size='20' value='%s'></td></tr>
	<tr bgcolor='#E4E4E4'><td colspan=2 align='center'>
		<input type='submit' value='保存'>&nbsp;<input type='button' value='返回' onclick="history.back()">
	</td></tr> </table> </form> '''

u = User()
if cmd == 'add':
	print2( user_view % ('用户开户', uid_add_view % "", '', '', '', '', '', '') )
elif cmd == 'edit':
	user = u.query(form.getvalue('uid'))
	if len(user) <= 0:
		print2('此用户不存在')
	else:
		u = user[0]
		print2(user_view % ('帐户管理', uid_edit_view % u[u_f_uid], \
			u[u_f_uname], u[u_f_passwd], u[u_f_passwd], \
			u[u_f_email], u[u_f_phone], u[u_f_mobile]))
elif cmd == 'save':
	(uid, uname, email) = (form.getvalue('uid'), form.getvalue('uname'), form.getvalue('email'))
	(pwd1, pwd2) = (form.getvalue('pwd1'), form.getvalue('pwd2'))
	if uid is None or uname is None or email is None:
		print2('帐号，呢称，邮件不能为空')
	elif pwd1 is None or pwd1 != pwd2:
		print2('用户密码错')
	else:
		import md5
		spwd = md5.md5(pwd1).digest()
		user = (uid, uname, now(), '', spwd, email, \
			form.getvalue('phone'), form.getvalue('mobile'), '0.0'
		)
		if len(u.query(uid)) > 0:
			u.update(user)
		else:
			u.add(user)
		show_info('帐户保存成功')
