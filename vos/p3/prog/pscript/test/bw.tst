%!PS-Adobe-2.0
%%Creator: VICAR Program PSCRIPT
%%Title: bw
%%CreationDate: Mon Mar 22 11:21:47 1993
%%Pages: 1
%%DocumentFonts: Times-Roman
%%EndComments

 /inch { 72 mul} def
 /height     6.000 def
 /width     6.000 def
 /xPos     1.250 def
 /yPos     2.500 def
 /fontpt 15 def
 /picstr 10 string def
 /vicarimage
 { 10 10 8 [ 10 0 0 -10 0 10 ] 
     {currentfile picstr readhexstring pop}
     image
     }  def
 gsave
 xPos inch yPos inch translate
 width inch height inch scale
 vicarimage
00010203040506070809
0102030405060708090a
02030405060708090a0b
030405060708090a0b0c
0405060708090a0b0c0d
05060708090a0b0c0d0e
060708090a0b0c0d0e0f
0708090a0b0c0d0e0f10
08090a0b0c0d0e0f1011
090a0b0c0d0e0f101112
 grestore
  /Times-Roman findfont fontpt scalefont
  setfont
  % The following centers title below image:
  (Black & White Test Image)
 dup stringwidth pop 1 fontpt mul     0.000 inch add
 yPos inch exch sub exch % Y-Position on page
 width inch sub 2 div % Width of image in inches
 xPos inch exch sub exch moveto show % X-position
 grestore  % Restore former parameters
 showpage  % Command to print out page.
