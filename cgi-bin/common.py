#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import sys, os, time
import cgitb; cgitb.enable()
import cgi; escape = cgi.escape

sys.stderr = sys.stdout
exit = sys.exit
env = os.environ

#
#	globale variables
#
form = cgi.FieldStorage()
qa_css = '''<link href='../qa.css' type='text/css'> '''
percent = 0.1
page_size = 20
MY_EMAIL = "z_axis@163.com"
postsave_view = """<font color="#00FF00">%s</font><br><a href="q_list.py?typeid=%s&code=%s&start=%d">back</a>"""
info_view = """<html><head><script>
	function go_login() {
		document.location = "login.py?cmd=view";
	}
</script></head><body onload="%(init)s">%(info)s</body></html>"""

qa_title = 'China QA Network'
INFO_SAVE_OK = """<font color='blue'>save ok</font>"""
INFO_CLOSED = """<font color='red'>the question has been closed</font>"""

ERR_NOT_LOGIN = 1
ERR_USER = """<font color="red">please re-login</font>"""
head_ctype = '''<META http-equiv=Content-Type content="text/html; charset=utf8">'''
head_ctype_gb = '''<META http-equiv=Content-Type content="text/html; charset=gb2312">'''


def print2(data):
    print "Content-type: text/html\n\n"
    print data
	
def get_q_type():
	q_typeid = form.getvalue('typeid')
	q_code = form.getvalue('code')
	if q_code == None or q_code <= 0:
		q_code = '1'
	return (q_typeid, q_code)
	
def my_escape(data):
	return data.replace('"', '\'')		# for SQLite
	
def now():
	return "%04d-%02d-%02d %02d:%02d:%02d" % time.localtime()[0:6]

def now_inc(days):
	n = list( time.localtime() )
	n[2] = n[2] + days
	return "%04d-%02d-%02d %02d:%02d:%02d" % time.localtime( time.mktime(n) )[0:6]

def show_info(info, code=0):
	info = "<font color='red'>%s</font>" % info
	init = ""
	if code == ERR_NOT_LOGIN:
		init = "go_login()"
	print2(info_view % {"init":init, "info":info})
	
def check_user(uid):
	if uid is None:
		show_info("not login", ERR_NOT_LOGIN)
