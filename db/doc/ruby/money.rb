require "date.rb"

def pickNums(fromLst, count)
    result = []
    while result.size < count do
        num = fromLst[rand fromLst.size]
        result.push num if !result.include? num
    end
    result
end

def save2File(fp, dataLst)
    if fp
        resStr = (dataLst.join " ") + "\n"
        fp.print resStr 
        print resStr
    end
end

#
#            产生双色球号码
#   count:   注数
#   noFst:   不要的红区号列表
#   noSnd:   不要的蓝区号列表
#
def winSSQ(count, noRed, noBlue)
    #yesRed = (1..33).to_a.reject {|n| noRed.include? n}
    yesRed = good_red.reject {|n| noRed.include? n}
    yesBlue = (1..16).to_a.reject {|n| noBlue.include? n}
    srand
    begin
        f = open('ssqNum.txt', 'w')
        count.times {
            result = pickNums(yesRed,6) + pickNums(yesBlue,1)
            save2File(f, result)
        }
    ensure
        f.close if f
    end
end

def good_red
    sumRedNum = {}; (1..33).to_a.each {|n| sumRedNum[n] = 0}
    File.foreach("ssqHitNum.txt") { |line|
        num = (line.split " ")[1..7].map {|x| x.to_i}
        numRed = num.slice(0,6); numRed.each {|n| sumRedNum[n] += 1}
    }
    ((sumRedNum.sort_by{|k,v| v}.reverse).collect {|k,v| k}).slice(0,16)
end

def hitSSQ(no,hitNs)
    def hitDesc(redNum, blueNum)
        case [redNum, blueNum]
        when [6,1]; return "1st"
        when [6,0]; return "2nd"
        when [5,1]; return "3rd(3000)"
        when [5,0], [4,1]; return "4th(200)"
        when [4,0], [3,1]; return "5th(10)"
        when [2,1], [1,1], [0,1]; return "6th(5)"
        else return "X"
        end
    end

    File.open("ssqHitNum.txt", "a") do |fp|
        fp.write(no + " " + (hitNs.join " ") + "\n")
    end

    hitRed, hitBlue = hitNs.slice(0,6), hitNs.slice(6,1)
    print "No\tRed\tBlue\tResult\n"
    print "=============================================\n"
    File.foreach("ssqNum.txt") { |line|
        num = (line.split " ").map {|x| x.to_i}
        hitRedNum = num.slice(0,6).inject(0) {|s,x| (hitRed.include? x) ? s+=1 : s}
        hitBlueNum = num.slice(6,1).inject(0) {|s,x| (hitBlue.include? x) ? s+=1 : s}
        printf "%15s---> %2s %2s %s\n",line.chomp,hitRedNum,hitBlueNum,hitDesc(hitRedNum,hitBlueNum)
    }
end

#
#   stock
#
$SXF = 0.0015 #手续费
$YHS = 0.001  #印花费
$GHF = 1.0   #过户费
$RATIO = [0.236, 0.382, 0.5, 0.618, 0.809]

#计算股票盈利
def winG(qty,priceB,priceS)
	return qty * priceS * (1 -$SXF - $YHS) - 2 * $GHF - qty * priceB * (1 + $SXF)
end

#算权证盈利
def winQ(qty,priceB,priceS)
	return qty * priceS * (1 - $SXF) - 2 * $GHF - qty * priceB * (1 + $SXF)
end

def div618(priceLo, priceHi)
    ($RATIO.size-1).downto(0) {|i|
        price = priceLo + (priceHi - priceLo) * $RATIO[i]
        printf "-------%.3f   %.3f-------\n", $RATIO[i], price
    }
end

$NO_TRADE_DAYS = [
    Date.new(2010,1,1), Date.new(2010,1,2), Date.new(2010,1,3), #元旦
    Date.new(2010,2,13), Date.new(2010,2,14), Date.new(2010,2,15), Date.new(2010,2,16), Date.new(2010,2,17), #春节
    Date.new(2010,2,18), Date.new(2010,2,19), Date.new(2010,2,20), Date.new(2010,2,21),
    Date.new(2010,4,3), Date.new(2010,4,4), Date.new(2010,4,5), #清明
    Date.new(2010,5,1), Date.new(2010,5,2), Date.new(2010,5,3), #劳动
    Date.new(2010,6,12), Date.new(2010,6,13), Date.new(2010,6,14), Date.new(2010,6,15), Date.new(2010,6,16),  #端午
    Date.new(2010,9,22), Date.new(2010,9,23), Date.new(2010,9,24), Date.new(2010,9,25), Date.new(2010,9,26),  #中秋
    Date.new(2010,10,1), Date.new(2010,10,2), Date.new(2010,10,3), Date.new(2010,10,4), Date.new(2010,10,5),  #国庆
    Date.new(2010,10,6), Date.new(2010,10,7), Date.new(2010,10,9)
]

def is_trade_day(day1)
    if [6,0].include?(day1.wday) or $NO_TRADE_DAYS.include?(day1)
        return false
    else
        return true
    end
end

def str2date(dayStr)
    return Date.strptime(str=dayStr,fmt='%F')
end

def dateByNTday(dateStr, ntday)
    day1 = str2date(dateStr)
    count = 0
    while count < ntday
        day1 += 1
        count += 1 if is_trade_day(day1)
    end
    print day1
end
