#!/usr/local/bin/python
# -*- coding: utf-8 -*-
from common import *
from dao import *

menu_bar = """<a href='q_list.py?typeid=%s&code=%s'><font color="%s">[%s]</font></a>"""

title_view = """<table width='100%%' align='center' border='1' cellpadding='0' cellspacing='0'>
	<tr bgcolor='yellow'><th>%s</th><th>%s</th><th>%s</th><th>%s</th><th>%s</th><th>%s</th></tr>"""

q_view = """<tr><td width='40%%'><a href='q_show.py?qid=%(qid)s&start=%(start)d'>%(subject)s</a></td> \
<td>%(author)s</td><td>%(open_date)s</td><td>%(due_date)s</td><td>%(state)s</td><td align='right'>%(price)s$</td></tr>"""

prevBtnView = '''<a href='q_list.py?typeid=%(typeid)s&code=%(code)s&start=%(pstart)d'>&lt;previous</a>&nbsp;'''
nextBtnView = '''<a href='q_list.py?typeid=%(typeid)s&code=%(code)s&start=%(nstart)d'>next&gt;</a>&nbsp;&nbsp;'''
tool_bar = '''<tr><td colspan='6' align='center' bgcolor='#E4E4E4'>
		%(prevBtn)s %(nextBtn)s
		<a href='q_add.py?cmd=view&typeid=%(typeid)s&code=%(code)s&start=%(start)d'>add</a>&nbsp;
		<a href='q_find.py?typeid=%(typeid)s&code=%(code)s&start=%(start)d'>find</a></td></tr></table>'''

def showQList(qlist, typeid, code, tbar, qstart):
	print2('''<html><head>>%(head)s<title>%(title)s</title></head>''' % {'head':head_ctype,"title":qa_title})
	for type in q_type[typeid]:
		if type[0] == code:
			print(menu_bar % (typeid, type[0], "red", type[1]))
		else:
			print(menu_bar % (typeid, type[0], "blue", type[1]))
	print( title_view % ('subject', 'author', 'created date', 'out of date', 'status', 'quoted price') )

	for q in qlist:
		print( q_view % { 'qid':q[q_f_qid], 'subject':q[q_f_subject],
			'author':q[q_f_author], 'open_date':q[q_f_open_date], 'due_date':q[q_f_expire_date], 
			'state':q[q_f_state], 'price':q[q_f_price],'start': qstart}
		)
	print tbar
	print('</html>')

