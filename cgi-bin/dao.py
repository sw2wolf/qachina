#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import sqlite3 as sqlite 
# from pysqlite2 import dbapi2 as sqlite
import os
import cgi; escape = cgi.escape 
from common import page_size, show_info, print2

q_state = ('open', 'close', 'expire')
q_type = {
	'11': [('1','ASM/C'), ('2','Lisp/Prolog/...'), ('3','XML/JS...')],
	'12': [('1','MySQL'), ('2','PostgreSQL'), ('3','SQLite/Berkely DB')],
	'13': [('1','xBSD/xLinux'), ('2','QNX/VxWorks...'), ('3', 'Windows')],
	'14': [('1','editor'), ('2','network'), ('3','VM')],
	'15': [('1','x86'), ('2','arm/blackfin...'), ('3', 'dragon chip'), ('4','integrated circuit...')],
	'21': [('1','health'), ('2','fist Tao'), ('3','game Tao'), ('4','eat'),('5','philosophy')],
	'22': [('1','securities'), ('2','fund...'), ('3', 'warrant'), ('4','stock skill'), ('5','success story'), ('6','my investment'), ('7','financial analysis')],
	'23': [('1','math'), ('2','physics'), ('3','chemistry'), ('4','language'),('5','knowledge...')],
	'24': [('1','Buddhism'), ('2','Taoism'), ('3','horoscope...'), ('9', 'great person')]
}

tx_type = {
	'001':'cash', '002':'QA deposit', '003':'QA draw', '004':'QA income', '005':'QA expense'
}

class AbstractTable:
    _conn = None
    _cursor = None

    def __init__(self):
        if self._cursor is None:
            #self._conn = sqlite.connect('d:/qachina/db/qachina.db')
            self._conn = sqlite.connect('/media/D/qachina/db/qachina.db')
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

    def update(self, qid, content):
        sql = '''update question set content = "%s" where qid = %s''' % (content, qid)
        #print (content)
        self.changeBySQL(sql)

    def update_state(self, qid, state):
        sql = '''update question set state = "%s" where qid = %s''' % (state, qid)
        self.changeBySQL(sql)

#
# Answer
#
[a_f_aid, a_f_qid, a_f_content, a_f_author, a_f_answer_date, a_f_profit] = range(6)

class Answer(AbstractTable):

	def __init__(self):
		AbstractTable.__init__(self)

	def query(self, qid):
		return self.queryBySQL("select * from answer where qid=%s" % qid)
		
	def add(self, answer):
		sql = "insert into answer values (NULL, '%s', '%s', '%s', '%s')"
		self.changeBySQL(sql % answer)

	def update(self, aid, content):
		sql = "update answer set content = '%s' where aid = %s " % (content, aid)
		self.changeBySQL(sql)
		

#
# User
#
[u_f_uid, u_f_uname, u_f_open_date, u_f_close_date, u_f_passwd, u_f_email, \
u_f_phone, u_f_mobile, u_f_balance] = range(9)

class User(AbstractTable):

	def __init__(self):
		AbstractTable.__init__(self)

	def auth(self, uid, passwd):
		import md5
		u = self.queryBySQL("select * from user where uid='%s' " % uid)
		if len(u) > 0:
			if u[0][u_f_passwd] == md5.md5(passwd).digest():
				return 1
		return 0
	
	def query(self, uid):
		return self.queryBySQL("select * from user where uid='%s' " % uid)
	
	def add(self, user):
		sql = "insert into user values ('%s','%s','%s','%s','%s','%s','%s','%s',%s)"
		self.changeBySQL(sql % user)

	def update(self, user):
		sql = "update user set uname='%s', open_date='%s', close_date='%s',	\
				passwd='%s', email='%s', phone='%s', mobile='%s' where uid='%s' "
		self.changeBySQL(sql % (user[1:8] + (user[0],)))

(txlog_txid, txlog_uid, txlog_debit, txlog_credit, txlog_value, txlog_date, txlog_memo) = range(7)

class TxLog(AbstractTable):

	def __init__(self):
		AbstractTable.__init__(self)

	def add(self, logs):
		sql = """insert into txlog values (null, '%s','%s','%s',%d,'%s','%s')"""
		for log in logs:
			self.changeBySQL(sql % log[0:6])

	def findByUser(self, uid):
		return self.queryBySQL("select * from txlog where who='%s' " % uid)
	
	def balance(self, uid):
		in_sql = """select sum(value) from txlog where (who='%s') and (debit='001')"""
		out_sql = """select sum(value) from txlog where (who='%s') and (credit='001')"""
		
		cash_in = self.queryBySQL(in_sql % uid)[0][0]
		if cash_in is None: cash_in = 0
		
		cash_out = self.queryBySQL(out_sql % uid)[0][0]
		if cash_out is None: cash_out = 0

		if cash_in < cash_out:
			show_info("%s balance error, please audit" % uid)
		return cash_in - cash_out
