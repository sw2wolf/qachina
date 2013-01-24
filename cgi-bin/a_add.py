#!/usr/local/bin/python
# -*- coding: utf-8 -*-
from common import *
from auth import *
from dao import Answer

check_user(user_id)

(q_typeid, q_code) = get_q_type()
qid = form.getvalue('qid')
aid = form.getvalue('aid')

a = Answer()

if qid == None:
	a.update(aid, form.getvalue('content'))
elif aid == None:
	a.add((qid, form.getvalue('content'), user_id, now()))
print2( postsave_view % (INFO_SAVE_OK, q_typeid, q_code) )
