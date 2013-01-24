#!/usr/local/bin/python
# -*- coding: utf-8 -*-
from common import *
from dao import *
from auth import *

check_user(user_id)

txlog_view = """<tr><td>%(txDate)s</td><td>%(txDesc)s</td><td align='right'>%(value)d</td><td>%(memo)s</td>"""

q_view = """<a href="q_show.py?qid=%s">%s</a>"""

mytx_view = """<table width='100%%' align='center' border='1' cellpadding='0' cellspacing='0'>
	<tr bgcolor='yellow'><th width='10%%'>交易时间</th><th width='50%%'>描述</th>
	<th width='5%%'>金额</th><th>备注</th></tr>
	%(txLog)s
	<tr><td colspan="4" align='right'><font color='red'>余额:%(balance)s元</font></td></table>"""
	
qs = Question()

def refQid(qid):
	q = qa.load(qid)
	return q_view % (qid, q[0][q_f_subject])

txlog = TxLog()
balance = txlog.balance(user_id)
logs = txlog.findByUser(user_id)
log_str = ""
for log in logs:
	txDesc = "错误交易记录"
	memo = "&nbsp;"
	if log[txlog_debit] == "001":
		if log[txlog_credit] == "002":
			txDesc = "充值"
		elif log[txlog_credit] == "004":
			txDesc = "答问收益"
			memo = refQid(log[txlog_memo])
	elif log[txlog_credit] == "001":
		if log[txlog_debit] == "003":
			txDesc = "取款"
		elif log[txlog_credit] == "005":
			txDesc = "提问支出"
			memo = refQid(log[txlog_memo])
		
	log_str += txlog_view % {"txDate":log[txlog_date], "txDesc":txDesc, "value":log[txlog_value], "memo":memo}
	
print2(mytx_view % {"txLog":log_str, "balance":balance})
