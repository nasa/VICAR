%!PS-Adobe-2.0
%%Creator: VICAR Program PSCRIPT
%%Title: bw
%%CreationDate: Wed Feb 28 11:07:59 2001
%%Pages: 1
%%DocumentFonts: Times-Roman
%%EndComments

 /inch { 72 mul} def
 /height     0.120 def
 /width     6.000 def
 /xPos     1.250 def
 /yPos     5.440 def
 /fontpt 15 def
 /picstr 50 string def
 /vicarimage
 { 50 1 8 [ 50 0 0 -1 0 1 ] 
     {currentfile picstr readhexstring pop}
     image
     }  def
 gsave
 xPos inch yPos inch translate
 width inch height inch scale
 vicarimage
000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d
1e1f202122232425262728292a2b2c2d2e2f3031
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
