#!/usr/bin/perl

# usage:  make_pdf_cache <directory> <pdf file> <machine>
# 		Defaults: . vicar.dict vicarproc
#
# Usage:  make_pdf_db <directory> <cache file>
# This perl script is designed to scan and convert
# each PDF in the target directory into a dictionary file entry
# to be interpreted by GEM during dictionary build.

use Cwd;
use Config;

if ( $#ARGV < 0 )  {
  $ARGV[0] = ".";		# get current working directory
}
else {
  $ARGV[0] = glob($ARGV[0]);	# expand dirname
}

if ( $#ARGV < 1 )  {
  $ARGV[1] = "vicar.dict";	# default filename
}

if ( $#ARGV < 2 )  {
  $ARGV[2] = "vicarproc";	# default machine name
}

opendir (INP_DIR, "$ARGV[0]") || die "Can't open input dir $ARGV[0]: $!\n";

if ( $Config{'osname'} eq 'VMS' ) {
  @pdf_files = grep /\.pdf;.*$/, readdir(INP_DIR);
} else {
  @pdf_files = grep /\.pdf$/, readdir(INP_DIR);
}

# open file for output

open (OUT_PDF, ">$ARGV[1]") || die "Can't open output file: $!\n";

foreach $pdf_file (sort (@pdf_files))  {

  if ( $Config{'osname'} eq 'VMS' ) {
    open ( INP_PDF, "$ARGV[0]$pdf_file") || die "Can't open input file: $ARGV[0]$pdf_file!\n";
  } else {
    open ( INP_PDF, "$ARGV[0]/$pdf_file") || die "Can't open input file: $ARGV[0]/$pdf_file!\n";
  }

  $name = $pdf_file;
  $name =~ s/.pdf//s;

  $prompt = $name;
  $prompt =~ tr/a-z/A-Z/;

  @annots = ();
  @parms = ();

  while (<INP_PDF>)  {	# for each line, do the following:
    chomp;		#  get rid of ending new-line
    tr/\t/ /s;		#  replace any number of tabs with a space
    s/^\s*//;		#  delete any leading white-space

    while (/\+\s*$/) {			# continuation line
      s/\+\s*$//;			# remove continuation char
      if ($newline = <INP_PDF>) {	# get new line
        if (/^!#/) {
	  $newline =~ s/^!#//;	# if spcl comment, remove !# on contuation line
        }
        $_ = $_ . $newline;		# append new line
        chomp;				# preprocess again
        tr/\t/ /s;
      }
    }

    if (/^\s*$/) { 	# skip the line if it is empty
      next;
    }

    elsif (/^Procedure/i) {
      next;
    }
    elsif (/^Process/i)	{
      next;
    }

    # read PARAM names

    elsif (/^PARM /i)  {
      tr/A-Z/a-z/;
      @parmdef = split ' ',$_;
      @parms = (@parms,$parmdef[1]);
      next;
    }

    # read SUBCMD

    elsif (/^SUBCMD /i)  {
      @parms = (@parms,"SUBCMD");
      next;
    }

    # read annotations (lines start with '!# ANNOT')

    elsif (/^!#\s*ANNOT\s/i) {
      s/!#\s*ANNOT\s\s*//i;
      @annots = (@annots,$_);
    }

    # read parameter-specific annotations (lines start with '!# PARM')

    elsif (/^!#\s*PARM\s/i) {
      s/!#\s*PARM\s\s*//i;
      if (/^([\w(){}\-]+)\s+(.*)$/) {	# else it's invalid
	$parm_name = $1;
	$annot_vals = $2;

	# Convert (n-m) to __n_m in name
	$parm_name =~ s/\((\d+)-(\d+)\)/__\1_\2/;
	# Now try simple (n) to __n
	$parm_name =~ s/\((\d+)\)/__\1/;

	while ($annot_vals) {

	  # Strip off one value, delimited by a semicolon... but not a semi
	  # that is in quotes!
	  $annot_vals =~ s/^((?:(?:\"[^"]*\")|[^";])*);?//;
	  $one_annot = $1;
	  if ($one_annot =~ s/^\s*(\w+)\s*=\s*(.*)$//) {
	    $key = $1;
	    $value = $2;
	    @annots = (@annots, $key . "." . $parm_name . "=" . $value);
	  }
	}
      }
    }

    # read .TITLE description line

    elsif ($readtitle ne "") {
      s/^\"(.*)\"$/\1/;			# remove quotes, if any
      tr/\"/\'/;			# convert to single quotes
      $temp = "description=" . "\"" . $_ . "\"";
      @annots = (@annots,$temp);
      $readtitle = "";
      next;
    }
    elsif (/^\s*\.title/i)  {
      $readtitle = "True";
      next;
    }
  }

  close INP_PDF;

  $parmlist = join (',', @parms);
  $parmstr = "parameters=\(" . $parmlist . "\)";

  $system = "system=vicar";

  $annotlist = join (';', @annots, $parmstr, $system);

  $outline = join (':', $name, $prompt, $ARGV[2], "", $annotlist);

  print OUT_PDF "compute_module $outline\n\n";
}

close OUT_PDF;

