#! /usr/bin/perl
#	This is a perl script designed to take a list of files with full
# path information from CCC and generate the imake_obj and imake_targ
# files.  The advantage of this script will be that it will only require
# 1 easily generated file from CCC rather than 2 pain-in-the-butt ones.
# Note though that if seperate files were generated per link group
# (such as p2 or gui), you will still need to have them generated seperately.
#
# NAME
#	create_Imake.perl creates the 2 files, Imake_obj.* and Imake_targ.*
#
# SYPNOPSIS
#	create_Imake.perl <system name> [filter] [name extention]
#
# OPTIONS
#	<system name> is a required parameter which specifies which system
#	 (i.e. rtl, shvic, vrdi, etc.) is to be used.  This name is used in
#	 the Imake filenames.
#
#	[filter] is an optional parameter mostly for gui and p2, where you
#	can specify to only keep 1 given link group
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
#	($obj_group,$compile_macro,$make_app_targ,$obj_directory)
#		 = split(/:/,$link_group{$im_path});
# 
#		$obj_group is the name you want assigned to the object
#	group in the Imakefile_obj.* file.
#		$compile_macro is the name of the compile macro you want
#	listed in the Imakefile_targ.* file.  Note you do not provide
#	parenthesis.  make_app is an acceptable macro for this.
#		$make_app_targ is the target directory if the macro make_app
#	is chosen.  If it is not chosen, this field still needs to be
#	present- but it should just be empty.
#		$obj_directory is the directory where the modules are
#	compiled.  Note that if your location involves system variables, you
#	must remember that you are enterring this from a perl script and must
#	take any perl peculiarities into account.  For example, if you were
#	in the rtl system and wanted to specify VICCPU you would use:
#		'\\\$(VICCPU)'
#	Trust me, you really need to precede the $ with 3 backslashes.
#

# link_group is a list of all the link groups available and a list of
# several parameters needed in the compilation in a colan delimited list,
# much like an /etc/passwd file.
%link_group = (
"/cplt/src","-:::",

"/gui/prog","APP_MOTIFAPP:make_app:GUILIB:\\\$(GUILIB)",
"/gui/sub/gui","SUBS:make_app:GUILIB:.",

"math77/source","MATH77OBJS:compile_math77::\\\$(VICCPU)",

"/mdms/source/cppobjs","-:::",
"/mdms/source/dbview","-:::",
"/mdms/source/dbq","-:::",
"/mdms/source/fei","-:::",
"/mdms/source/glblib","-:::",
"/mdms/source/lcllib","-:::",
"/mdms/source/pwdserver","-:::",
"/mdms/source/dblib","-:::",
"/mdms/source/dbwindows","-:::",

"/mars/src/sub/general","-:::",
"/mars/src/sub/private","-:::",
"/mars/src/prog/general","-:::",
"/mars/src/prog/private","-:::",
"/mars/src/sub/general","-:::",
"/mars/src/sub/private","-:::",
"/mars/src/prog/general","-:::",
"/mars/src/prog/private","-:::",

"/p1/prog","-:::",
"/p1/sub","SUBS:make_app:R1LIB:.",

"/p2/port-prog/dtr","APP_DTR:make_app:R2LIB:\\\$(R2LIB)",
"/p2/port-prog/easy","APP_EASY:make_app:R2LIB:\\\$(R2LIB)",
"/p2/port-prog/nimscal_spice","APP_NIMSCAL_SPICE:make_app:R2LIB:\\\$(R2LIB)",
"/p2/port-prog/pds_label","APP_PDS_LABEL:make_app:R2LIB:\\\$(R2LIB)",
"/p2/port-prog/rt_shr","APP_RT_SHR:make_app:R2LIB:\\\$(R2LIB)",
"/p2/port-prog/spice","APP_SPICE:make_app:R2LIB:\\\$(R2LIB)",
"/p2/port-prog/sybase","APP_SYBASE:make_app:R2LIB:\\\$(R2LIB)",
"/p2/port-prog/candela","APP_CANDELA:make_app:R2LIB:\\\$(R2LIB)",
"/p2/unport-prog/clips","APP_CLIPS:make_app:R2LIB:\\\$(R2LIB)",
"/p2/unport-prog/dtr","APP_DTR:make_app:R2LIB:\\\$(R2LIB)",
"/p2/unport-prog/dtr_fps","APP_DTR_FPS:make_app:R2LIB:\\\$(R2LIB)",
"/p2/unport-prog/dtr_rt_shr","APP_DTR_RT_SHR:make_app:R2LIB:\\\$(R2LIB)",
"/p2/unport-prog/dtr_shr_img","APP_DTR_SHR_IMG:make_app:R2LIB:\\\$(R2LIB)",
"/p2/unport-prog/dtr_spice","APP_DTR_SPICE:make_app:R2LIB:\\\$(R2LIB)",
"/p2/unport-prog/easy","APP_EASY:make_app:R2LIB:\\\$(R2LIB)",
"/p2/unport-prog/fps","APP_FPS:make_app:R2LIB:\\\$(R2LIB)",
"/p2/unport-prog/fps_rt_shr","APP_FPS_RT_SHR:make_app:R2LIB:\\\$(R2LIB)",
"/p2/unport-prog/fps_spice","APP_FPS_SPICE:make_app:R2LIB:\\\$(R2LIB)",
"/p2/unport-prog/idm_shr","APP_IDM_SHR:make_app:R2LIB:\\\$(R2LIB)",
"/p2/unport-prog/nimscal_spice","APP_NIMSCAL_SPICE:make_app:R2LIB:\\\$(R2LIB)",
"/p2/unport-prog/pics","APP_PICS:make_app:R2LIB:\\\$(R2LIB)",
"/p2/unport-prog/rdm","APP_RDM:make_app:R2LIB:\\\$(R2LIB)",
"/p2/unport-prog/rdm_shr_img","APP_RDM_SHR_IMG:make_app:R2LIB:\\\$(R2LIB)",
"/p2/unport-prog/rt_shr_img","APP_RT_SHR_IMG:make_app:R2LIB:\\\$(R2LIB)",
"/p2/unport-prog/rt_shr","APP_RT_SHR:make_app:R2LIB:\\\$(R2LIB)",
"/p2/unport-prog/spice","APP_SPICE:make_app:R2LIB:\\\$(R2LIB)",
"/p2/port-sub","SUBS:make_app:R2LIB:.",
"/p2/unport-sub","SUBS:make_app:R2LIB:.",

"/p3/port-prog/ccsi","APP_CCSI:make_app:R3LIB:\\\$(R3LIB)",
"/p3/port-prog/dtr","APP_DTR:make_app:R3LIB:\\\$(R3LIB)",
"/p3/port-prog/easy","APP_EASY:make_app:R3LIB:\\\$(R3LIB)",
"/p3/port-prog/fps","APP_FPS:make_app:R3LIB:\\\$(R3LIB)",
"/p3/port-prog/spice","APP_SPICE:make_app:R3LIB:\\\$(R3LIB)",
"/p3/port-prog/pics","APP_PICS:make_app:R3LIB:\\\$(R3LIB)",
"/p3/unport-prog/ccsi","APP_CCSI:make_app:R3LIB:\\\$(R3LIB)",
"/p3/unport-prog/dtr","APP_DTR:make_app:R3LIB:\\\$(R3LIB)",
"/p3/unport-prog/easy","APP_EASY:make_app:R3LIB:\\\$(R3LIB)",
"/p3/unport-prog/fps","APP_FPS:make_app:R3LIB:\\\$(R3LIB)",
"/p3/unport-prog/spice","APP_SPICE:make_app:R3LIB:\\\$(R3LIB)",
"/p3/unport-prog/pics","APP_PICS:make_app:R3LIB:\\\$(R3LIB)",
"/p3/port-sub","-:::",
"/p3/unport-sub","-:::",

"/rtl/source/arch_alliant","ALLIANTOBJS:compile::\\\$(VICCPU)",
"/rtl/source/arch_mac_mpw","MAC_MPWOBJS:compile::\\\$(VICCPU)",
"/rtl/source/comftn","FTNOBJS:compile_ftn::\\\$(VICCPU)",
"/rtl/source/common","COMOBJS:compile::\\\$(VICCPU)",
"/rtl/source/unix","UNIXOBJS:compile::\\\$(VICCPU)",
"/rtl/source/vms","-:compile::\\\$(VICCPU)",
"/rtl/source/vaxvmsmar","-:compile::\\\$(VICCPU)",
"/rtl/source/vmsmar","-:compile::\\\$(VICCPU)",

"/shell_vicar/src/com","COMOBJS:compile_shvic::\\\$(VICCPU)",
"/shell_vicar/src/unix","COMOBJS:compile_shvic::\\\$(VICCPU)",
"/shell_vicar/src/comftn","COMOBJS:compile_shvic_cftn::\\\$(VICCPU)",
"/shell_vicar/src/fortran","COMOBJS:compile_shvic_ftn::\\\$(VICCPU)",
"/shell_vicar/src/vms","-:compile_shvic::",

"/stae/src/com","COMOBJS:compile_stae::\\\$(VICCPU)",
"/stae/src/unix","COMOBJS:compile_stae::\\\$(VICCPU)",
"/stae/src/vms","-:compile_stae::",
"/stae/src/fortran","COMOBJS:compile_stae_fort::\\\$(VICCPU)",
"/stae/src/staec","COBJS:compile_staec::\\\$(VICCPU)",

"/vids/source/source","OBJS:compile_vids::\\\$(VICCPU)",
"/vids/source/source_unix","OBJS:compile_vids::\\\$(VICCPU)",
"/vids/source/source_mar","-:??::",
"/vids/source/source_vms","-:??::",

"/vrdi/source/common","COMOBJS:compile_vrdi::\\\$(VICCPU)",
"/vrdi/source/common_vrdi_tae","COMOBJS:compile_vrdi::\\\$(VICCPU)",
"/vrdi/source/common_vms","-:compile_vrdi::\\\$(VICCPU)",
"/vrdi/source/common_vms_vrdi_tae","-:compile_vrdi::\\\$(VICCPU)",
"/vrdi/source/common_unix","COMOBJS:compile_vrdi::\\\$(VICCPU)",
"/vrdi/source/comftn","FTNOBJS:compile_vrdi_ftn::\\\$(VICCPU)",
"/vrdi/source/comftn_vrdi_tae","FTNOBJS:compile_vrdi_ftn::\\\$(VICCPU)",

"/vrdi/source/adage/adage_dummy","OBJS:compile_vrdi::\\\$(VICCPU)",
"/vrdi/source/dummy/dummy","OBJS:compile_vrdi::\\\$(VICCPU)",
"/vrdi/source/ip85hi/ip85hi_dummy","OBJS:compile_vrdi::\\\$(VICCPU)",
"/vrdi/source/ip85lo/ip85lo_dummy","OBJS:compile_vrdi::\\\$(VICCPU)",
"/vrdi/source/ivas/ivas_dummy","OBJS:compile_vrdi::\\\$(VICCPU)",
"/vrdi/source/jup/jup_dummy","OBJS:compile_vrdi::\\\$(VICCPU)",
"/vrdi/source/ramtek/ramtek_dummy","OBJS:compile_vrdi::\\\$(VICCPU)",
"/vrdi/source/tek/tek_dummy","OBJS:compile_vrdi::\\\$(VICCPU)",

"/vrdi/source/xdisplay/xdisplay","LIBOBJS:compile_vrdix::\\\$(VICCPU)",
"/vrdi/source/xdisplay/xdisplay_sub","DEVOBJS:compile_vrdixd::\\\$(VICCPU)",

);

# ***********************************************************************
#			parse the command line
# ***********************************************************************
$link_filter = "";
$nameext = "";
$create_targ = 0;
($input_file,$output_file,$link_filter) = @ARGV;
$create_obj = ($output_file =~ /Imakefile_obj/);
$create_targ = ($output_file =~ /Imakefile_targ/);

# ***********************************************************************
#			open up the files
#	If there are any problems with the files, abort the perl script.
# ***********************************************************************
open(IMAKE_SOURCE,$input_file) ||
	die "ERROR: create_Imake.perl couldn't open " . $input_file;
if ($create_targ) {
	open(IMAKE_TARG,">" . $output_file ) ||
	die "ERROR: create_Imake.perl couldn't open " . $output_file;
}
if ($create_obj) {
	open(IMAKE_OBJ,">" . $output_file ) ||
	die "ERROR: create_Imake.perl couldn't open " . $output_file;
}

# ***********************************************************************
#		parse CCC file into a form more usable for
#		obj and targ files.  Write targ file and
#		sort data for obj file.
# ***********************************************************************
if ($create_obj) {
foreach (sort(<IMAKE_SOURCE>)) {
	# *** parse each filename ***
	chop();
	($im_path,$im_name,$im_ext) = /(.*)\/([^\/]*)(\.[^.]*)$/;
	($obj_group,$compile_macro,$make_app_targ,$obj_directory)
					= split(/:/,$link_group{$im_path});

	# Check against link group filters if any.  Also, don't call for
	# unportable programs in UNIX.
	($subsystem, $link_path) = $im_path =~ /(\/[^\/]*)(\/.*)/;
	if (($link_path !~ /$link_filter/)		# doesn't match filter
			&& ($link_filter ne "") ) {	# and filter exits
		next;
	}
	if ($link_path =~ /unport/ )	{		# supposed to be UNIX
		next;
	}

	$targ_ext = ($compile_macro eq "make_app") ? "" : ".o";

	# *** Put together object lists and output target file ***
	if (($obj_group ne "-") && ($obj_group ne "")) {
		#
		# Get ready for obj stuff
		#
		if (grep(/$obj_group/,@obj_macros) == 0) {
			@obj_macros = (@obj_macros,$obj_group);
		}
		$add_to_list = "\@list_" . $obj_group .
			"= (\@list_" . $obj_group . "," .  '"';
		if ($obj_directory ne ".") {
			$add_to_list = $add_to_list . $obj_directory . "/";
		}
		$add_to_list = $add_to_list . $im_name .
				$targ_ext . '"' . ");\n";
		eval $add_to_list;
	}
}
}


if ($create_targ) {
foreach (sort(<IMAKE_SOURCE>)) {
	# *** parse each filename ***
	chop();
	($im_path,$im_name,$im_ext) = /(.*)\/([^\/]*)(\.[^.]*)$/;
	($obj_group,$compile_macro,$make_app_targ,$obj_directory)
					= split(/:/,$link_group{$im_path});

	# Check against link group filters if any.  Also, don't call for
	# unportable programs in UNIX.
	($subsystem, $link_path) = $im_path =~ /(\/[^\/]*)(\/.*)/;
	if (($link_path !~ /$link_filter/)		# doesn't match filter
			&& ($link_filter ne "") ) {	# and filter exits
		next;
	}
	if ($link_path =~ /unport/ )	{		# supposed to be UNIX
		next;
	}

	$targ_ext = ($compile_macro eq "make_app") ? "" : ".o";

	# *** Put together object lists and output target file ***
	if (($obj_group ne "-") && ($obj_group ne "")) {
		#
		# set up for targ list
		#
		$targ_dir = "";
		if ($obj_directory ne ".") {
			eval ("\$targ_dir = " . '"' . $obj_directory . '"');
			$targ_dir = $targ_dir . "/";
		}

		print (IMAKE_TARG $targ_dir ,$im_name, $targ_ext . ": ");
		if (($compile_macro ne "make_app") || ($im_path !~ /sub/)) {
			print (IMAKE_TARG $im_name, $im_ext);
		}
		print (IMAKE_TARG "\n");
		print (IMAKE_TARG "\t",$compile_macro,"(", $im_name);
		if ($compile_macro eq "make_app") {
			print(IMAKE_TARG ",",$make_app_targ);
		}
		print (IMAKE_TARG ")\n");
	}
}
}

# ***********************************************************************
#			Write out the obj file
# ***********************************************************************
# First, make a list of all object macros:
#	note that "-" just meant "no macro" so we should filter those out
# once we filter them out, then we are guaranteed that the curr_obj_macro
# will _not_ match "-".
if ($create_obj) {			# Well, only do this if we want obj
	$last_obj = "-";
	foreach $curr_obj_macro (sort(@obj_macros)) {
		if ($curr_obj_macro eq $last_obj) {	# skip duplicates
			next;
		}
		$last_obj = $curr_obj_macro;
		eval ("\@curr_obj_list = \@list_" . $curr_obj_macro) ;
		print (IMAKE_OBJ "\n\n");
		print (IMAKE_OBJ $curr_obj_macro . " =");
		foreach $obj_filename ( sort(@curr_obj_list)) {
			print ( IMAKE_OBJ " \\\n\t" .$obj_filename );
		}
	}
	print IMAKE_OBJ "\n\n";
}

# ***********************************************************************
#			close all files
# ***********************************************************************
close(IMAKE_SOURCE);
if ($create_obj) {
	close(IMAKE_OBJ);
}
if ($create_targ) {
	close(IMAGE_TARG);
}

