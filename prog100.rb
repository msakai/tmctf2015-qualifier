require 'open-uri'
require 'chunky_png'

uri = "http://ctfquest.trendmicro.co.jp:43210/click_on_the_different_color"
while true
  puts uri
  html = open(uri){|f| f.read }

  %r!<img src="/(img/.*\.png)" onClick="clicked\(event\)">! =~ html
  img_uri = "http://ctfquest.trendmicro.co.jp:43210/#{$1}"
  puts img_uri
  png = open(img_uri){|f| f.read }
  ds = ChunkyPNG::Datastream.from_blob(png)
  png = ChunkyPNG::Image.from_datastream(ds)

  table = {}
  (0...png.height).each{|y|
    (0...png.width).each{|x|
      color = png[x,y] & 0xFFFFFF00
      if color & 0xFFFFFF00 != 0xFFFFFF00
        (table[color] ||= []) << [x,y]
      end
    }
  }

  table.each_pair{|k,v|
    puts "#{k} : #{v.length}"
  }
  foo = table.to_a.map{|(color,points)| [color, points.length, points[0]] }.sort_by{|(color, num, pt)| num }
  p foo
  x, y = foo[0][2]

  /window\.location\.href='\/(.*)\?/ =~ html  
  uri = "http://ctfquest.trendmicro.co.jp:43210/#{$1}?x=#{x}&y=#{y}"
end

# <html>
# <head>
# <title>Choose the different color</title>
# <script type=text/javascript src="/js/jquery.min.js"></script>
# <script type=text/javascript>
# <!--
# function clicked(e)
# {
#   var x=e.layerX-e.target.x;
#   var y=e.layerY-e.target.y;
#   window.location.href='/0e61c0e10df0be7c16ea343d03f9a0508f680858553e?x='+x+'&y='+y;
# }
# // -->
# </script>
# </head>
# <body>
#   <img src="/img/0e61c0e10df0be7c16ea343d03f9a0508f680858553e.png" onClick="clicked(event)">
# </body>
# </html>
