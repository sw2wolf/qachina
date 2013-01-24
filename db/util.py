#!/usr/bin/env python
# -*- coding: utf-8 -*-
import sqlite3 as sqlite 
import os

page_size = 20

class AbstractTable:
	_conn = None
	_cursor = None

	def __init__(self):
		if self._cursor is None:
			self._conn = sqlite.connect('/media/E/www/qachina/db/qachina.db')
			self._conn.text_factory = str
			self._cursor = self._conn.cursor()
	
	def queryBySQL(self, sql):
		self._cursor.execute(sql)
		return self._cursor.fetchall()

	def changeBySQL(self, sql):
		self._cursor.execute(sql)
		self._conn.commit()

#
# Question
#
[q_f_qid, q_f_typeid, q_f_code, q_f_subject, q_f_content, \
	q_f_author, q_f_open_date, q_f_expire_date, q_f_state, q_f_price, q_f_is_top] = range(11)

class Question(AbstractTable):

	def __init__(self):
		AbstractTable.__init__(self)

	def load(self, qid):
		return self.queryBySQL('select * from question where qid=%s' % qid)
	
	def load_all(self):
		return self.queryBySQL('select * from question')

	def find(self, typeid, code, start, count=page_size):
		sql = 'select * from question where typeid=%s and code=%s and state != "expire" '
		sql += 'order by is_top desc, open_date desc limit %d, %d'
		return self.queryBySQL(sql % (typeid, code, start, count))
		
	def findByData(self, typeid, code, subject, price, state):
		wc = "where typeid=%s and code=%s and price >= %d and state = '%s'"
		if len(subject) > 0:
			wc += " and instr(subject,'%s')" % subject
		sql = "select * from question " + wc + " order by open_date desc limit %d"
		return self.queryBySQL(sql % (typeid, code, price, state, page_size))
	
	def add(self, question):
		sql = 'insert into question values (NULL, %(typeid)s, %(code)s, "%(subject)s", \
				"%(content)s", "%(user_id)s", "%(openDate)s", "%(dueDay)s", "%(state)s", %(price)s, %(is_top)s)'
		
		self.changeBySQL(sql % {"typeid":question[0], "code":question[1], \
			"subject":question[2], "content":question[3], \
			"user_id":question[4], "openDate":question[5], \
			"dueDay":question[6], "state":question[7], "price":question[8], 'is_top':question[9]})

	def update_content(self, qid, content):
		sql = 'update question set content = "%s" where qid = %s' % (content, qid)
		self.changeBySQL(sql)

	def update_state(self, qid, state):
		sql = '''update question set state = "%s" where qid = %s''' % (state, qid)
		self.changeBySQL(sql)

qid=356
qs = Question()
q = qs.load(qid)
sql = 'update question set subject="%s", content="%s" where qid = %s' % (
    unicode(q[q_f_subject],"GB2312").encode("UTF8"),
    unicode(q[q_f_content],"GB2312").encode("UTF8"),
    qid)
qs.changeBySQL(sql)

'''
qlist = qs.load_all()

for q in qlist:
    print(q[q_f_qid])
    sql = 'update question set subject="%s", content="%s" where qid = %s' % (
        unicode(q[q_f_subject],"GBK").encode("UTF8"),
        unicode(q[q_f_content],"GBK").encode("UTF8"),
        q[q_f_qid])
    qs.changeBySQL(sql)

print("finish")
'''
