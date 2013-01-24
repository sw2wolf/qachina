#!/usr/local/bin/python
# -*- coding: utf-8 -*-
from common import *
from dao import *
from auth import *

check_user(user_id)

q_view = """<tr><td colspan='2' align='center' bgcolor='#DFEBFF'>%s</td></tr>
	<tr bgcolor='#E4E4E4'><td width='20%%'>%s<br>%s<br><font color='red'>总值: %s元</font></td><td>%s</td></tr>"""

a_view = """<tr bgcolor='#F4EAD7'>
	<td>%s<br>%s<br>获利：<input type='text' name='%s' maxlength='10' size='10'>元</td>
	<td>%s</td></tr>"""

cmd_view = """<tr bgcolor='#E4E4E4'><td colspan='2' align='center'>
	<input type='submit' value='结算'>&nbsp;&nbsp;
	<input type='button' value='返回' onclick='window.history.back();'></td></tr>"""

close_view = """<table width='100%%' align='center' border='1' cellpadding='0' cellspacing='0'>%(qview)s
	<form method="POST" action="q_close.py">%(aview)s
	<input type="hidden" name="qid" value="%(qid)s">
	<input type="hidden" name="cmd" value="close">%(cview)s</form></table>"""

log_view = """<tr><td>%s</td><td align='right'><font color='red'>%s</font></td></tr>"""
ok_view = """<html><body>结算成功, 明细如下<br>
	<table width='50%%' border='1' align='center'>
	<tr bgcolor='yellow'><th>答问人</th><th>利润</th></tr>%(logs)s
	<tr><td colspan='2' align='center'><a href='q_list.py?typeid=%(typeid)d&code=%(code)d'>返回问题列表</a></td></tr>
	</table></body></html>"""

qid = form.getvalue('qid')
q = Question()
qs = q.load(qid)[0]
a = Answer()
answers = a.query(qid)

content = escape(qs[q_f_content]).replace("\n", "<br>")
price = int( qs[q_f_price] * (1 - percent) )
cmd = form.getvalue('cmd')
if cmd == 'view':
	qview = q_view % (qs[q_f_subject], qs[q_f_author], qs[q_f_open_date], price, content)
	aview = ""
	for answer in answers:
		a_content = escape(answer[a_f_content]).replace('\n', '<br>')
		aview += a_view % (answer[a_f_author], answer[a_f_answer_date], answer[a_f_aid], a_content)
	print2( close_view % {"qview":qview, "aview":aview, "cview":cmd_view, "qid":qid} )
elif cmd == 'close':
	if len(answers) == 0:
		q.update_state(qid, "close")
		print2(ok_view % {"logs":"<tr><td colspan='2'>此问没有答案, 被关闭</td></tr>", \
			"typeid":qs[q_f_typeid], "code":qs[q_f_code]})
		exit(0)
		
	tlog = TxLog()
	balance = tlog.balance(user_id)
	if balance < price:
		show_info("你的帐户余额不够, 不能结帐")
	
	logs = []; sum = 0;
	tx_keys = tx_type.keys()
	logs.append((qs[q_f_author], tx_keys[3], tx_keys[0], price, now(), qid))
	for a in answers:
		aid = a[a_f_aid]
		profit = int( form.getvalue(aid) )
		if profit <= 0:
			continue
		sum += profit
		logs.append((a[a_f_author], tx_keys[0], tx_keys[2], profit, now(), qid))
	if price != sum:
		show_info("问题总价必须等于相应答案利润之和")
	
	tlog.add(logs)
	q.update_state(qid, "close")
	logstr = ""
	for log in logs[1:]:
		logstr += log_view % (log[0], log[3])
	print2(ok_view % {"logs":logstr, "typeid":qs[q_f_typeid], "code":qs[q_f_code]})
