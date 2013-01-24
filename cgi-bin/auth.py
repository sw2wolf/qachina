#!/usr/local/bin/python
# -*- coding: utf-8 -*-
from common import *

user_id = None
user_pwd = None
if env.has_key('HTTP_COOKIE'):
	import Cookie
	C = Cookie.SimpleCookie()
	C.load(env['HTTP_COOKIE'])
	if C.has_key('userid'):
		user_id = C['userid'].value
