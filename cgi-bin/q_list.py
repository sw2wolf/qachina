#!/usr/local/bin/python
# -*- coding: utf-8 -*-
from common import *
from auth import *
from dao import *
from q_common import *

check_user(user_id)
(q_typeid, q_code) = get_q_type()
try:
    q_start = int( form.getvalue('start') )
except:
    q_start = 0
qs = Question()
qlist = qs.find(q_typeid, q_code, q_start)

prevS, nextS = q_start - page_size, q_start + page_size
if prevS <= 0: prevS = 0
prev_btn, next_btn = '', ''
if q_start > 0:
	prev_btn = prevBtnView % {"typeid":q_typeid, "code":q_code, "pstart":prevS}
if len(qlist) > 0:
	next_btn = nextBtnView % {"typeid":q_typeid, "code":q_code, "nstart":nextS}
	
tbar = tool_bar % {"prevBtn":prev_btn, "nextBtn":next_btn, "typeid":q_typeid, "code":q_code, "start":q_start}

showQList(qlist, q_typeid, q_code, tbar, q_start)
