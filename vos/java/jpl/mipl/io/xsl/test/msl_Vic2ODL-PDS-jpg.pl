#!/usr/bin/perl

# start in the directory with all the files
$argc = @ARGV;          # get the number of arguments
if ($argc == 0 ) {
	print "msl_Vic2ODL-PDDS-jpg.pl dir multi vic2odl_xsl  odl2pds_xsl max \n";
	print "prints a set of transcoder (jConvertIIO) commands to test .DAT and .VIC files \n";
	print "Output goes to STDOUT. Redirect to a file to keep the output as a script or arg file.\n";
	print "There are no command switches, just put the values on the command line. Order is important. \n";
	print " dir - the input directory \n";
	print " multi - {multi/single] create standalone java commands or a set to be executed from a file \n";
	print "vic2odl_xsl - full path to the xsl script to convert vicar files to ODL (VicarToPDSmsl_Blob_ODL2.xsl)\n";
	print "odl2pds_xsl - full path to the xsl script to convert ODL files to PDS (PDSToPDSmsl_Blob_ODL2PDS_4.xsl)\n";
	print "max - the maximum number fo files to process. \n";
	print "      (Optional argument) Useful to limit the number of comands generated for a large input directory\n";
	print " If the multi option is used, redirect the output to a file. Use this file as the input file for the \n";
	print " command: java -Xmx256m jpl.mipl.io.jConvertIIO  arg=multi.txt \n";
}
$outdir = $ARGV[0];
$multi = $ARGV[1];

$vic2odl_xsl = $ARGV[2];
$odl2pds_xsl = $ARGV[3];

if ($argc > 4) {
	$max = $ARGV[4]; # maximum number of files used 
} else {
	$max = 1000;
}



# cd /Volumes/bigraid/MSL_data/Sample_EDRS/5-9-2011/clean_data
#
# cp *.VIC ../out_multi
# cp *.VIC ../out_single

# cd /Volumes/bigraid/MSL_data/Sample_EDRS/
# ~/bin/msl_Vic2ODL-PDS-jpg.pl /Volumes/bigraid/MSL_data/Sample_EDRS/test-5-11-11 multi 11 > multi11.txt
# ~/bin/msl_Vic2ODL-PDS-jpg.pl /Volumes/bigraid/MSL_data/Sample_EDRS/test-5-11-11 single 11 > single11.txt
# ~/bin/msl_Vic2ODL-PDS-jpg.pl /Volumes/bigraid/MSL_data/Sample_EDRS/5-9-2011/out_single single 1 > single1.txt
# capture output to a file. execute each
#
# sh single1.txt
# sh single.txt
#
# java -Xmx256m jpl.mipl.io.jConvertIIO  arg=multi.txt

# time sh single.txt
#
# time java -Xmx256m jpl.mipl.io.jConvertIIO  ARGFILE=multi.txt
# 152.015u 30.504s 3:06.07 98.0%	0+0k 236+326io 918pf+0w
#
# time sh single100.txt
# 617.869u 87.299s 15:35.90 75.3%	0+0k 1+1171io 3267pf+0w


chdir $outdir;
print "#!/bin/sh \n";
$cwd = `pwd`;
print "# $cwd \n";

# $debug = " debug=true xml=true ";
$debug = "  ";

if ($multi =~ /^m/) {
	$java = "";
} else {
	
	$java = "java -Xmx256m jpl.mipl.io.jConvertIIO ";
}

$ct = 0;

while (<*.DAT>) {
	$file  = $_;
	($base,$ext) = split(/[.]/,$file);
	
	$pds = sprintf ( "%s.%s", $base, "LBL");
	
	
	# java -Xmx256m jpl.mipl.io.jConvertIIO   inp=/Volumes/bigraid/MSL_data/Sample_EDRS/test-5-11-11/CC0_353400938EIN_F0000000001036288M1.DAT out=/Volumes/bigraid/MSL_data/Sample_EDRS/test-5-11-11/CC0_353400938EIN_F0000000001036288M1.lbl PDS_DETACHED_ONLY=true   format=PDS PDS_LABEL_TYPE=PDS3 xsl=/eclipse_3.6_workspace/MSL_xsl/PDSToPDSmsl_Blob_ODL2PDS_2.xsl
	$xsl = "/eclipse_3.6_workspace/MSL_xsl/PDSToPDSmsl_Blob_ODL2PDS_4.xsl";
	$xsl = $odl2pds_xsl;
	# other options: RI PDS_FILENAME_IN_LABEL=true
	$cmd = sprintf("%s inp=%s/%s out=%s/%s %s format=PDS PDS_DETACHED_ONLY=true PDS_LABEL_TYPE=PDS3 xsl=%s ",
	$java, $outdir, $file, $outdir, $pds, $debug, $xsl);
	
	print "$cmd \n";
}

$ct=0;
while (<*.VIC>) {

$file  = $_;
$vic = $file;
	
($base,$ext) = split(/[.]/,$file);
$odl = sprintf ( "%s.%s", $base, "IMG");
$pds = sprintf ( "%s.%s", $base, "LBL");
$vic_jpg = sprintf ( "%s_vic.%s", $base, "jpg");
$odl_tif = sprintf ( "%s_odl.%s", $base, "tif");

# VIC to ODL	
#	java -Xmx256m jpl.mipl.io.jConvertIIO  inp=/Users/slevoe/MSL/Sample_EDRS/RRA_353184709EDR_T0000000001036288M1.VIC out=/Users/slevoe/MSL/Sample_EDRS/out_ODL/RRA_353184709EDR_T0000000001036288M1.ODL format=PDS ADD_BLOB=true RI EMBED PDS_LABEL_TYPE=ODL3 xsl=/eclipse_3.6_workspace/MSL_xsl/VicarToPDSmsl_Blob_ODL1.xsl

$xsl = "/eclipse_3.6_workspace/MSL_xsl/VicarToPDSmsl_Blob_ODL2.xsl";
$xsl = $vic2odl_xsl;
	
$cmd = sprintf("%s inp=%s/%s out=%s/%s %s format=PDS ADD_BLOB=true RI EMBED PDS_LABEL_TYPE=ODL3 xsl=%s ",
	$java, $outdir, $vic, $outdir, $odl, $debug, $xsl);

   print "$cmd \n";
   # system($cmd);
	
	# $cmd = sprintf("~/bin/getPdsLabel.pl %s/%s ", $outdir, $odl);	
	# print "$cmd \n";
	
	# ODL - PDS detached

	
	# java -Xmx256m jpl.mipl.io.jConvertIIO  inp=/Users/slevoe/MSL/Sample_EDRS/out-ODL-PDS_Libs/CC0_353400938EIN_F0000000001036288M1.DAT out=/Users/slevoe/MSL/Sample_EDRS/out-ODL-PDS_Libs/CC0_353400938EIN_F0000000001036288M1.PDS PDS_FILENAME_IN_LABEL=true xml=true debug=true PDS_DETACHED_ONLY=true format=pds PDS_LABEL_TYPE=PDS3 xsl=/eclipse_3.6_workspace/MSL_xsl/PDSToPDSmsl_Blob_ODL2PDS_2.xsl

	
	$xsl = "/eclipse_3.6_workspace/MSL_xsl/PDSToPDSmsl_Blob_ODL2PDS_4.xsl";
	$xsl = $odl2pds_xsl;
	# other options: RI PDS_FILENAME_IN_LABEL=true
	$cmd = sprintf("%s inp=%s/%s out=%s/%s %s format=PDS PDS_DETACHED_ONLY=true PDS_LABEL_TYPE=PDS3 xsl=%s ",
	$java, $outdir, $odl, $outdir, $pds,  $debug,  $xsl);
	
	print "$cmd \n";
	
	
	

	
	#	ODL to tif
	#	java -Xmx256m jpl.mipl.io.jConvertIIO  inp=/Users/slevoe/MSL/Sample_EDRS/RRA_353184709EDR_T0000000001036288M1.ODL out=/Users/slevoe/MSL/Sample_EDRS/RRA_353184709EDR_T0000000001036288M1.jpg 2RGB format=jpg RI OFORM=byte
	$cmd = sprintf("%s inp=%s/%s out=%s/%s %s  format=tif RI OFORM=byte ",
	$java, $outdir, $odl, $outdir, $odl_tif, $debug);
	
	print "$cmd \n";
	#	Vicar to jpg
	#	java -Xmx256m jpl.mipl.io.jConvertIIO  inp=/Users/slevoe/MSL/Sample_EDRS/RRA_353184709EDR_T0000000001036288M1.VIC out=/Users/slevoe/MSL/Sample_EDRS/RRA_353184709EDR_T0000000001036288M1.jpg format=tif RI OFORM=byte
	$cmd = sprintf("%s inp=%s/%s out=%s/%s %s format=jpg RI 2RGB OFORM=byte ",
	$java, $outdir, $vic, $outdir, $vic_jpg, $debug);
	print "$cmd \n\n";

	$ct++;
	if($ct >= $max) { 
		exit;
	}
	
}

# 

