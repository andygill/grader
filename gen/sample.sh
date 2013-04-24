
. gen/config-$1.sh

convert pages/$page1 \
  -gravity center -extent 1350x2000 \
  -gravity none \
  -font Bookman-DemiItalic -pointsize 72 -stroke red -fill red \
  -draw "text 500,1000 '$student'" \
  -draw "text 800,1200 '$result'" \
  tmp/$1-output-0.png

convert pages/$page2 \
  -gravity center -extent 1350x2000 \
  -gravity none \
  -font Bookman-DemiItalic -pointsize 44 -stroke red -fill red \
  -draw "text 60,320 '$q1a'" \
  -draw "text 60,375 '$q1b'" \
  -draw "text 60,430 '$q1c'" \
  -draw "text 60,485 '$q1d'" \
  -draw "text 60,540 '$q1e'" \
  -draw "text 60,595 '$q1f'" \
  -draw "text 60,650 '$q1g'" \
  -draw "text 60,1000 '$q2a'" \
  -draw "text 60,1250 '$q2b'" \
  -draw "text 60,1550 '$q2c'" \
  -draw "text 60,1800 '$q2d'" \
  tmp/$1-output-1.png

convert pages/$page3 \
  -gravity center -extent 1350x2000 \
  -gravity none \
  -font Bookman-DemiItalic -pointsize 72 -stroke red -fill red \
  -draw "text 1200,1000 '$q31'" \
  -draw "text 1200,1600 '$q32'" \
  tmp/$1-output-2.png



convert pages/$page4 \
  -gravity center -extent 1350x2000 \
  -gravity none \
  -font Bookman-DemiItalic -pointsize 72 -stroke red -fill red \
  -draw "text 1200,700 '$q4a'" \
  -draw "text 1200,1100 '$q4b'" \
  -draw "text 1200,1600 '$q4c'" \
  tmp/$1-output-3.png

convert pages/$page5 \
  -gravity center -extent 1350x2000 \
  -gravity none \
  -font Bookman-DemiItalic -pointsize 72 -stroke red -fill red \
  -draw "text 1200,1100 '$q51'" \
  -draw "text 1200,1600 '$q52'" \
  tmp/$1-output-4.png

convert pages/$page6 \
  -gravity center -extent 1350x2000 \
  -gravity none \
  -font Bookman-DemiItalic -pointsize 72 -stroke red -fill red \
  -draw "text 1200,400 '$q6a'" \
  -draw "text 1200,800 '$q6b'" \
  -draw "text 1200,1250 '$q6c'" \
  -draw "text 1200,1650 '$q6d'" \
  tmp/$1-output-5.png
#
convert +append tmp/$1-output-0.png tmp/$1-output-1.png tmp/$1-output-2.png tmp/$1-output-012.png
convert +append tmp/$1-output-3.png tmp/$1-output-4.png tmp/$1-output-5.png tmp/$1-output-345.png
convert -quality 100 tmp/$1-output-012.png tmp/$1-output-345.png final/$target

