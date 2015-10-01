#! /usr/bin/perl
#	This is a perl script designed to take a list of files with full
# path information from CCC and generate the imake_bld
# files.  The advantage of this script will be that it will only require
# 1 easily generated file from CCC rather than 2 pain-in-the-butt ones.
# Also note that the CCC file is the same file used for the Imake file
# generation.
# Note though that if seperate files were generated per link group
# (such as p2 or gui), you will still need to have them generated seperately.
#
# NAME
#	create_bld.perl creates the file, <sysname>.bld
#
# SYPNOPSIS
#	create_bld.perl <input file> <output file> [filter]
#
# OPTIONS
#	<input file> is the name of the CCC generated file.
#
#	<output file> is the name of the .bld file to generate.  The
#	.bld should be included in this name.
#
#	[filter] is an optional parameter mostly for gui and p2, where you
#	can specify to only keep 1 given link group.  A special case of this
#	filter is ending it with a "/", which insures that the matching
#	string is not a substring of another.  I.e. "foo/" will match "foo"
#	but will not match "foobar".
#
#	[name extention] is an optional parameter mostly for gui and p2
#	for when you want to override the default name extentions.
#
#
# NOTES
#	Note that if the group's CCC path is not listed below in the global
#	lookup tables, this perl script won't know what to do, so it will skip
#	them.  Since this was written to handle a large list including inc and
#	makefiles (i.e. files you wouldn't expect it to process), it WILL NOT
#	OUTPUT AN ERROR MESSAGE if it can't identify the group.  It will
#	simply skip it quietly.
#

# ***********************************************************************
#		initialize global lookup tables and variables
# ***********************************************************************
# The following maps the path name into the object list
# note that if the group is just a "-", it will be skipped
# in both the obj and target file.
# The following code is used to parse each line of the link group table:
# 		($pre_bld,$post_bld) = $build_macro =~ /(.*)#####(.*)/;
# 		$build_line = $pre_bld . $im_name . $im_ext . $post_bld ."\n";
# 	OR:
# 		($pre_bld,$post_bld) = $build_macro =~ /(.*)##NO#(.*)/;
# 		$build_line = $pre_bld . $im_name . $post_bld ."\n";
# 
# Note this is considerably simpler than the Imake file generation in that
# we just substitute the file name in for the ##### or ##NO#.  The difference
# between these two is that ##### indicates we want the file extention
# included in the build line, and ##NO# indicates we wish to skip it.

# link_group is a list of all the link groups available and a list of
# templates for a build line.
%link_group = (
"/gui/prog",			"\$ \@gui\$source:##### system",
"/gui/sub/gui",			"\$ \@gui\$sub:[gui]##### system",

"/p1/sub",			"\$ \@p1\$sub:##NO# sys",

"/p2/port-prog",		"\$ \@p2\$source:##### system",
"/p2/unport-prog",		"\$ \@v2\$util:build_unport p2\$source:#####",
"/p2/unport-inc",		"\$ \@p2\$top:incs2 #####",
"/p2/port-sub",			"\$ \@p2\$sub:##### system",
"/p2/unport-sub",		"\$ \@s2\$source:#####",
"/p2/unport-inc",		"\$ \@p2\$top:incs2 #####",

"/p3/port-prog",		"\$ \@p3\$source:##### system",
"/p3/unport-prog",		"\$ \@v2\$util:build_unport p3\$source:#####",
"/p3/port-sub",			"\$ \@s3\$source:#####",
"/p3/unport-sub",		"\$ \@s3\$source:#####",

"/rtl/source",			"\$ \@v2\$rtltop:comprtl #####",
"/rtl/source/unix",		"-",
"/rtl/source/vaxvmsmar",	"\$ if (f\$getsyi(\"cpu\").ne.128) then \@v2\$rtltop:marrtl #####",
"/rtl/source/vmsmar",		"\$ \@v2\$rtltop:marrtl #####",
"/rtl/source/arch_alliant",	"-",
"/rtl/source/arch_mac_mpw",	"-",
"/rtl/inc/includes",		"\$ \@v2\$rtltop:incrtl #####",
"/rtl/inc/main",		"-",
"/rtl/inc/unixinc",		"-",
"/rtl/inc/vmsinc",		"\$ \@v2\$rtltop:incrtl #####",

"/shell_vicar/src/com",		"\$ \@shvic\$top:compshvic #####",
"/shell_vicar/src/comftn",	"\$ \@shvic\$top:compshvic #####",
"/shell_vicar/src/vms",		"\$ \@shvic\$top:comp_fort_shvic #####",

"/stae/src/com",		"\$ \@stae\$top:compstae #####",
"/stae/src/vms",		"\$ \@stae\$top:compstae #####",

"/vids/source/source",		"\$ \@vids\$top:compvids #####",
"/vids/source/pdf",		"\$ copy vids\$source:#####  vids\$lib",
"/vids/source/source_mar",	"\$ \@vids\$top:marvids #####",
"/vids/source/source_vms",	"\$ \@vids\$top:compvids #####",

"/vrdi/source/common",		"\$ \@vrdi\$top:compvrdi #####",
"/vrdi/source/common_vrdi_tae", "\$ \@vrdi\$top:compvrdi #####",
"/vrdi/source/common_vms",	"\$ \@vrdi\$top:compvrdi #####",
"/vrdi/source/common_vms_vrdi_tae","\$ \@vrdi\$top:compvrdi #####",
"/vrdi/source/comftn",		"\$ \@vrdi\$top:compvrdi #####",
"/vrdi/source/comftn_vrdi_tae",	"\$ \@vrdi\$top:compvrdi #####",

"/vrdi/source/adage/adage",	"\$ \@vrdi\$top:compvrdi #####",
"/vrdi/source/dummy/dummy",	"\$ \@vrdi\$top:compvrdi #####",
"/vrdi/source/ip85hi/ip85hi",	"\$ \@vrdi\$top:compvrdi #####",
"/vrdi/source/ip85lo/ip85lo",	"\$ \@vrdi\$top:compvrdi #####",
"/vrdi/source/ivas/ivas",	"\$ \@vrdi\$top:compvrdi #####",
"/vrdi/source/jup/jup",		"\$ \@vrdi\$top:compvrdi #####",
"/vrdi/source/ramtek/ramtek",	"\$ \@vrdi\$top:compvrdi #####",
"/vrdi/source/tek/tek",		"\$ \@vrdi\$top:compvrdi #####",
"/vrdi/inc/includes",		"\$ \@vrdi\$top:incvrdi #####",
"/vrdi/inc/inc_vrdi_tae",	"\$ \@vrdi\$top:incvrdi #####",

"/vrdi/source/xdisplay/xdisplay","\$ \@vrdi\$top:compvrdi #####",
"/vrdi/source/xdisplay/xdisplay_sub","\$ \@vrdi\$top:compvrdix #####",

);

# ***********************************************************************
#			parse the command line
# ***********************************************************************
$link_filter = "";
$nameext = "";
$create_targ = 0;
($input_file,$output_file,$link_filter) = @ARGV;

# ***********************************************************************
#			open up the files
#	If there are any problems with the files, abort the perl script.
# ***********************************************************************
open(IMAKE_SOURCE,$input_file) ||
	die "ERROR: create_bld.perl couldn't open " . $input_file;
open(BLD_FILE,">" . $output_file ) ||
	die "ERROR: create_bld.perl couldn't open " . $output_file;
print(BLD_FILE "\$ set noon\n");

# ***********************************************************************
#		parse CCC file into a form more usable for
#		obj and targ files.  Write targ file and
#		sort data for obj file.
# ***********************************************************************

($last_char) = $link_filter =~ /(.)$/;
foreach (sort(<IMAKE_SOURCE>)) {
	# *** parse each filename ***
	chop();
	($im_path,$im_name,$im_ext) = /(.*)\/([^\/.]*)(\.[^.]*)$/;
	@tmp_im_path = split('/',$im_path);
	$build_macro = "";
	while ((@tmp_im_path) && ($build_macro eq "")) {
		$build_macro = $link_group{join('/',@tmp_im_path)};
		pop(@tmp_im_path );
	}
	if (($build_macro eq "") || ($build_macro eq "-")) {
		next;
	}

	# Check against link group filters if any.
	($subsystem, $link_path) = $im_path =~ /(\/[^\/]*)(\/.*)/;
	if ($last_char eq "/") {
		$link_path = $link_path . "/";
	}
	if (($link_path !~ /$link_filter/)		# doesn't match filter
			&& ($link_filter ne "") ) {	# and filter exits
		next;
	}

	if ($build_macro =~ /#####/) {
		($pre_bld,$post_bld) = $build_macro =~ /(.*)#####(.*)/;
		$build_line = $pre_bld . $im_name . $im_ext . $post_bld ."\n";
	} elsif ($build_macro =~ /##NO#/) {
		($pre_bld,$post_bld) = $build_macro =~ /(.*)##NO#(.*)/;
		$build_line = $pre_bld . $im_name . $post_bld ."\n";
	}
	print (BLD_FILE $build_line );
}

# ***********************************************************************
#			close all files
# ***********************************************************************
close(IMAKE_SOURCE);
close(BLD_FILE);

