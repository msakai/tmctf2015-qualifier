require 'socket'
require 'roman-numerals' # https://rubygems.org/gems/roman-numerals/

def subst_english(s)
  s.gsub(/([a-zA-Z]+)(\s+[a-zA-Z]+)*/){|ws|
    ws = ws.split(/\s+/)
    
    result = 0
    x = 0
    ws.each{|w|
      case w
      when 'zero'
        x += 0
      when 'one'
        x += 1
      when 'two'
        x += 2
      when 'three'
        x += 3
      when 'four'
        x += 4
      when 'five'
        x += 5
      when 'six'
        x += 6
      when 'seven'
        x += 7
      when 'eight'
        x += 8
      when 'nine'
        x += 9
      when 'ten'
        x += 10
      when 'eleven'
        x += 11
      when 'twelve'
        x += 12
      when 'thirteen'
        x += 13
      when 'fourteen'
        x += 14
      when 'fifteen'
        x += 15
      when 'sixteen'
        x += 16
      when 'seventeen'
        x += 17
      when 'eighteen'
        x += 18
      when 'nineteen'
        x += 19
      when 'twenty'
        x += 20
      when 'thirty'
        x += 30
      when 'forty'
        x += 40
      when 'fifty'
        x += 50
      when 'sixty'
        x += 60
      when 'seventy'
        x += 70
      when 'eighty'
        x += 80
      when 'ninety'
        x += 90
      when 'hundred'
        x *= 100
      when 'thousand'
        result += x * 1000
        x = 0
      when 'million'
        result += x * 1000000
        x = 0
      when 'billion'
        result += x * 1000000000
        x = 0
      when 'trillion'
        result += x * 1000000000000
        x = 0
      else
        raise "unknown word #{w}"
      end      
    }
    result += x unless x == 0
    result.to_s
  }
end

#p subst_english("three").to_i==3
#p subst_english("six").to_i==6
#p subst_english("seven").to_i==7
#p subst_english("forty five").to_i==45
#p subst_english("twenty three").to_i==23
#p subst_english("nine thousand five hundred thirty").to_i==9530
#p subst_english("twenty five thousand three hundred seventy two").to_i==25372
#p subst_english("ninety two").to_i==92
#p subst_english("nine hundred seventeen thousand four hundred thirteen").to_i==917413
#p subst_english("nine hundred forty four thousand nine hundred fifteen").to_i==944915
#p subst_english("fifty three million nine hundred eight thousand three hundred ninety four").to_i==53908394
#__END__

sock = TCPSocket.open("ctfquest.trendmicro.co.jp", 51740)
while s = sock.readpartial(1024)
  puts("server> " + s)
  s.gsub!(/=.*$/, '')
  s.gsub!(/,/, '')
  begin
    s.gsub!(/[MCDLIVX]+/){|roman| RomanNumerals.to_decimal(roman) }
    s = subst_english(s)
    puts("eval> " + s)
    ans = eval(s)
  rescue Exception => e
    p e
    print("human> ")
    ans = gets
  end  
  puts("ans> " + ans.to_s)
  sock.write(ans.to_s + "\n")
end
s.close
