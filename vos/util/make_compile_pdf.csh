#!/bin/csh
unalias cd
echo procedure
echo body
echo enable-force_lower
echo 'let _onfail="continue"'
cd source
foreach pdf_file (*.pdf)
	echo "COMPILE $pdf_file"
end
echo end-proc
