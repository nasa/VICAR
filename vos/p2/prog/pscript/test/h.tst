%!PS-Adobe-2.0
%%Creator: VICAR Program PSCRIPT
%%Title: h
%%CreationDate: Wed Feb 28 11:08:00 2001
%%Pages: 1
%%DocumentFonts: Times-Roman
%%EndComments

 /inch { 72 mul} def
 /height    10.167 def
 /width     5.083 def
 /xPos     1.708 def
 /yPos     0.417 def
 /fontpt 15 def
 /picstr 5 string def
 /vicarimage
 { 5 10 8 [ 5 0 0 -10 0 10 ] 
     {currentfile picstr readhexstring pop}
     image
     }  def
 gsave
 xPos inch yPos inch translate
 width inch height inch scale
 vicarimage
0002040608
020406080a
0406080a0c
06080a0c0e
080a0c0e10
0a0c0e1012
0c0e101214
0e10121416
1012141618
121416181a
 grestore
  /Times-Roman findfont fontpt scalefont
  setfont
  % The following centers title below image:
  (Subsampled)
 dup stringwidth pop 1 fontpt mul     0.000 inch add
 yPos inch exch sub exch % Y-Position on page
 width inch sub 2 div % Width of image in inches
 xPos inch exch sub exch moveto show % X-position
  (Halfword)
 dup stringwidth pop 2 fontpt mul     0.000 inch add
 yPos inch exch sub exch % Y-Position on page
 width inch sub 2 div % Width of image in inches
 xPos inch exch sub exch moveto show % X-position
  (with)
 dup stringwidth pop 3 fontpt mul     0.000 inch add
 yPos inch exch sub exch % Y-Position on page
 width inch sub 2 div % Width of image in inches
 xPos inch exch sub exch moveto show % X-position
  (Titles)
 dup stringwidth pop 4 fontpt mul     0.000 inch add
 yPos inch exch sub exch % Y-Position on page
 width inch sub 2 div % Width of image in inches
 xPos inch exch sub exch moveto show % X-position
 grestore  % Restore former parameters
 showpage  % Command to print out page.
