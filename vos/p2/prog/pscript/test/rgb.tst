%!PS-Adobe-2.0
%%Creator: VICAR Program PSCRIPT
%%Title: r
%%CreationDate: Wed Feb 28 11:08:00 2001
%%Pages: 1
%%DocumentFonts: Times-Roman
%%EndComments

 /inch { 72 mul} def
 /height     4.800 def
 /width     6.000 def
 /xPos     1.250 def
 /yPos     3.100 def
 /fontpt 15 def
 /Rstr 10 string def
 /Gstr 10 string def
 /Bstr 10 string def
 /vicarimage
 { 10 8 8 [ 10 0 0 -8 0 8 ] 
     {currentfile Rstr readhexstring pop}
     {currentfile Gstr readhexstring pop}
     {currentfile Bstr readhexstring pop}
     true 3
     colorimage
     }  def
 gsave
 xPos inch yPos inch translate
 width inch height inch scale
 vicarimage
0019324b647d96afc8e1
0019324b647d96afc8e1
00000000000000000000
19324b647d96afc8e1fa
0019324b647d96afc8e1
19191919191919191919
324b647d96afc8e1fa13
0019324b647d96afc8e1
32323232323232323232
4b647d96afc8e1fa132c
0019324b647d96afc8e1
4b4b4b4b4b4b4b4b4b4b
647d96afc8e1fa132c45
0019324b647d96afc8e1
64646464646464646464
7d96afc8e1fa132c455e
0019324b647d96afc8e1
7d7d7d7d7d7d7d7d7d7d
96afc8e1fa132c455e77
0019324b647d96afc8e1
96969696969696969696
afc8e1fa132c455e7790
0019324b647d96afc8e1
afafafafafafafafafaf
 grestore
  /Times-Roman findfont fontpt scalefont
  setfont
  % The following centers title below image:
  (Color Test Image)
 dup stringwidth pop 1 fontpt mul     0.000 inch add
 yPos inch exch sub exch % Y-Position on page
 width inch sub 2 div % Width of image in inches
 xPos inch exch sub exch moveto show % X-position
 grestore  % Restore former parameters
 showpage  % Command to print out page.
