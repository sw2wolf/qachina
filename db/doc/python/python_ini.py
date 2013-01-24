#-*-coding:utf-8-*-  
import readline, rlcompleter; readline.parse_and_bind("tab: complete")

import os, sys
import datetime

SXF = 0.0015 #手续费
YHS = 0.001  #印花费
GHF = 1.0   #过户费

#计算股票盈利
def win(Qty,Pb,Ps):
	return Qty * Ps * (1 -SXF - YHS) - 2 * GHF - Qty * Pb * (1 + SXF)

#算权证盈利
def winQ(Qty,Pb,Ps):
	return Qty * Ps * (1 - SXF) - 2 * GHF - Qty * Pb * (1 + SXF)

RATIO = [0.236, 0.382, 0.5, 0.618, 0.809]
#黄金分割
def div618P(priceLo, priceHi):
    for i in xrange(len(ratio)-1, -1, -1):
        price = priceLo + (priceHi - priceLo) * RATIO[i]
        print "-------%.3f   %.3f-------\n" % (RATIO[i], price)

DT = datetime.datetime
NO_TRADE_DAYS = [DT(2009,1,1), DT(2009,5,1), DT(2009,5,2), DT(2009,5,3),
                 DT(2009,10,1), DT(2009,10,2), DT(2009,10,3)
                ]
oneDay = datetime.timedelta(days=1)

#将字符串转换成datetime类型  
def str2date(dateStr,format):      
    return DT.strptime(dateStr, format)

def is_trade_day(theDate):
    WoD = theDate.weekday()
    if (WoD in (5,6)) or (theDate in NO_TRADE_DAYS):
        return False
    return True

def date_by_ntday(dateStr, ntday):
    format = "%Y-%m-%d"
    day1 = str2date(dateStr, format)
    count = 0
    while count != ntday:
        day1 += oneDay
        if is_trade_day(day1):
            count += 1
    print str(day1)[0:10]

def trade_days(dateStr1, dateStr2):
    format = "%Y-%m-%d"
    day1 = str2date(dateStr1, format)
    day2 = str2date(dateStr2, format) + oneDay
    count = 0
    while day1 != day2:
        if is_trade_day(day1): count += 1
        day1 += oneDay
    print "The number of trading days is %d\n" % count

if __name__=="__main__":
    print "Welcome to Python world\n"
