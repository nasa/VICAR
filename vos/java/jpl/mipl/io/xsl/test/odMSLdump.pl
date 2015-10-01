#!/usr/bin/perl

# odMSL.pl

# ~/bin/odMSLdump.pl
$argc = scalar(@ARGV);
# print "argc = $argc \n";
if ($argc < 5) {
	print "odMSLdump.pl file start_byte recsize print_ct rec_ct \n";
	print "file =- file name to dump\n";
	print "start_byte - the (decimal) byte to start reading/dumping \n";
	print "recsize - record size. \n";
	print "print_ct - how many short values to print for the record \n";
	print "rec_ct - how many records (recsize) to print \n";
	exit;
}
	
$file = $ARGV[0];
$start_byte = $ARGV[1];
$recsize = $ARGV[2];
$print_ct = $ARGV[3];
$rec_ct = $ARGV[4];



# od -t c -A d -v CR0_349646445EDR_F0000000001015808M1.IMG > CR0_349646445EDR_F0000000001015808M1.IMG.od3 
# ~/bin/odMSLdump.pl CR0_349646445EDR_F0000000001015808M1.VIC 2144 2144 16 5  

# start_byte = 17 * 2144 = 36448
# ~/bin/odMSLdump.pl CR0_349646445EDR_F0000000001015808M1.IMG 36448 2144 16 5  

# decimal addresses -A d
# -j skip
# -t x2 hex shorts
# -v write all data
# -N length - number of bytes to output
print "start_byte=$start_byte rec_size=$recsize print_ct=$print_ct rec_ct=$rec_ct \n\n";

for ($rec = 0 ; $rec < $rec_ct ; $rec++) {
	$start = $start_byte + ($recsize * $rec) ;
	$cmd = sprintf("od -A d -t x2 -v -N %d  -j %d %s ", $print_ct, $start, $file);
	# print "cmd=$cmd \n";

	open DUMP,"$cmd |";

	while (<DUMP>) {
		chomp;
		$line = $_;
		print "$line \n";
	}
}

