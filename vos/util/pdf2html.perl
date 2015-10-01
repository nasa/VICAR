#!/usr/bin/perl
# print out usage statement if command line arguments are less than the
# required one argument

if ($#ARGV < 0) {
   print "USAGE:\n\n";
   print "pdf2html directory_name index_name link_prefix pdf_file_name\n";
   print "\n";
   print "   This perl script is used to generate HTML help documents\n";
   print "   from TAE-formatted PDF files.  The script generates two .html\n";
   print "   files for each PDF: name.html has the main help and a link and\n";
   print "   level 1 help for each parameter, and name_level2.html for the\n";
   print "   level 2 help.  Both files are written to the current directory.\n";
   print "   In addition, an index file is created with links to all of the\n";
   print "   programs (including their titles).\n";
   print "\n";
   print "directory name: The directory to use to look for PDFs.\n";
   print "\n";
   print "index_name: The name of the index file (defaults to index.html\n";
   print "\n";
   print "link_prefix: The URL prefix to use for links to params or other\n";
   print "   pdfs.  Omit or leave empty for relative links in the same dir.\n";
   print "\n";
   print "pdf_file_name: The name of the PDF to generate.  Omit to generate\n";
   print "   help for all pdf's in the directory.\n";
   print "\n";
   exit;
} 

$index_file = "index.html";
$crosslink_prefix = "";
$file_to_translate = "";
$directory = $ARGV[0];
if ($#ARGV > 0) {
   $index_file = $ARGV[1];
}
if ($#ARGV > 1) {
   $crosslink_prefix = $ARGV[2];
}
if ($#ARGV > 2) {
   $file_to_translate = $ARGV[3];
}

$destdir = "./";

# This will create a list of the PDF names to be used for text substitutions.
# In order to create a link for each word in the text that matches a PDF name,
# uncomment this section, and the eval's that use pdf_translations.

# opendir(PDF_DIR,"$directory")|| die "Can't open $directory: $!\n";
# 
# @pdf_files = grep(/(.*)pdf$/,readdir(PDF_DIR));
# @sorted_pdf_files = sort(@pdf_files);
#
# # create list of translations to link references to other programs within
# # the pdf of a particular program
# for $pdf_file (@sorted_pdf_files){
#    ($pn,$junk) = split(/\./,$pdf_file);
#    $pdf_translations .= "s/\\b$pn\\b/\<A HREF\=\"$crosslink_prefix${pn}.html\"\>$pn\<\\/A\>/\;\n";
#    }

# Create index file

open(INDEX_FILE, ">$index_file")|| die "Can't open $index_file: $!\n";
print INDEX_FILE "<HEAD><title>VICAR Help</title></HEAD>\n";
print INDEX_FILE "<BODY>\n";
print INDEX_FILE "<H1>VICAR Help</H1>\n";

if ($file_to_translate eq "") {
   $file_to_translate = ".*\.pdf";
}

opendir (INP_DIR, "$directory") || die "Can't open input dir $directory: $!\n";

@pdf_files = grep /$file_to_translate/, readdir(INP_DIR);
@pdf_files = sort(@pdf_files);

foreach $pdf_file (@pdf_files) {
   print "Processing $pdf_file\n";
   $pdf_file_spec = "$directory/$pdf_file";

   open(PDF_FILE, "$pdf_file_spec")|| die "Can't open $pdf_file_spec: $!\n";

   ($progname,$pdf) = split(/\./,$pdf_file);
   $progname =~ tr/A-Z/a-z/;
   $title_pn = $progname;
   $title_pn =~ tr/a-z/A-Z/;

   $v1 = 0;
   $v2 = 0;
   $d1 = 0;
   $d2 = 0;
   $suf1 = "_level1.html";
   $suf2 = ".html";
   $suf3 = "_level2.html";
   $file1 = "$progname$suf1";
   $file1a = "$destdir$file1";
   $file2 = "$progname$suf2";
   $file2a = "$destdir$file2";
   $file3 = "$progname$suf3";
   $file3a = "$destdir$file3";
   $helpstar = 0;
   $levelone = 0;
   $leveltwo = 0;
   $l1_var = 0;
   $l2_var = 0;
   $end = 0;
   $index_printed = 0;

   $parm_translations = "";
   %par_names = ();

   open(OUT1, ">$file1a") || die "Can't open $file1 $!\n";
   open(OUT2, ">$file2a") || die "Can't open $file2 $!\n";
   open(OUT3, ">$file3a") || die "Can't open $file3 $!\n";

   print OUT2 "<HTML>\n";
   print OUT2 "<HEAD><title>HTML Help for $title_pn</title></HEAD>\n";
   print OUT3 "<HTML>\n";
   print OUT3 "<HEAD><title>HTML Level 2 Help for $title_pn</title></HEAD>\n";

   print OUT2 "<BODY>\n";
   print OUT3 "<BODY>\n";

   print OUT2 "<H1>Help for $title_pn</H1>\n<pre>\n";
   print OUT3 "<H1>Level 2 Help for <A HREF=\"$crosslink_prefix$file2\">$title_pn</A></H1>\n";
   
   print OUT1 "<H2>PARAMETERS:</H2>\n";

   while(<PDF_FILE>){
      chomp;
      while (/\+\s*$/) {		# continuation lines
         s/\+\s*$/ /;
         $_ = $_ . <PDF_FILE>;
         chomp;
      }

      s/\</\&lt/;
      s/\>/\&gt/;
      tr/a-z/A-Z/ if (/^\.(.*)/);

      if (/^.PAGE(.*)/) {
         next;
      }

   # Look for PARM statements

      elsif (/^\s*PARM(.*)/){
         s/^\s+//;
         ($junk,$par_name,$rest) = split(/\s+/,$_ ,3);
         $par_name =~ tr/A-Z/a-z/;
         $rest =~ s/\s+/ /g;
         $rest =~ s/^\s//;
         $rest =~ s/(\w+ *= *(("[^"]*")|(\([^)]*\))|([^\s"(][^\s]*))) /\1<p>\n/g;
         $rest_of_parm{$par_name} = $rest;
         if ($par_names{$par_name} != 1) {	# avoid dups
            $par_names{$par_name}=1;
            $parm_translations .= "s/\\b($par_name)\\b/\<A HREF\=\"$crosslink_prefix${progname}_level2.html\#$par_name\"\>\$1\<\\/A\>/i\;\n";
         }
         next;
      }    

   # Look for TITLE, and add the name to the index

      elsif (/^\.TITLE/) {
         print INDEX_FILE "<A HREF=\"$crosslink_prefix$progname.html\">$title_pn</A> - ";
         $title = <PDF_FILE>;
         print INDEX_FILE $title;
         print INDEX_FILE "<p>\n";
         $index_printed = 1;
      }

   # Look for start of Help, or being in the Help section already

      elsif ((/^\.HELP(.*)/) || ($helpstar)){
         $helpstar = 1;      
         next if (/^.HELP(.*)/);
         if (/^.LEVEL1(.*)/) {
            $helpstar = 0;
            $levelone = 1;
            next;
         }
         elsif (/^\s*EXAM(.*)/){
            print OUT2 qq(<A NAME="examples">$_</A>\n);
            $examples_flag = 1;
         }
         elsif ((/^(.*)ognizant(.*)/)||(/^(.*)OGNIZANT(.*)/)){
            print OUT2 qq(<A NAME="CogProg">$_</A>\n);
            $cogprog_flag = 1;
         }
         else {
###            eval $pdf_translations;
            eval $parm_translations;
            print OUT2 "$_\n";
         }
      }

   # We're processing a Level 1 variable's help

      elsif ($l1_var){
         if (/^\.LEVEL2(.*)/) {
            $levelone = 0;
            $l1_var = 0;
            $leveltwo = 1;
            next;
         }      

         # Done with one, start another

         elsif(/^\.VAR(.*)/){
            $l1_var = 1;
            $d1 = 1;
            ($junk, $l1varname[++$v1,$d1]) = split(/\s+/,$_);
            $lc = $l1varname[++$v1,$d1];
            $lc =~ tr/A-Z/a-z/;
            print OUT1 qq(<HR><A HREF="$crosslink_prefix$file3#$lc"><H3>$l1varname[$v1,1]</H3></A>\n);
            next;
         }
         else{
###            eval $pdf_translations;
            eval $parm_translations;
            $l1varname[$v1,++$d1] = $_;
            print OUT1 "$l1varname[$v1,$d1]\n";
            next;
         }      
      }

   # We're processing a Level 2 variable's help

      elsif ($l2_var){
         if (/^\.END.*/) {
            print OUT3 "<p>\n<p></pre>\n$rest_of_parm{$l2varname[$v2,1]}\n";
            $leveltwo = 0;
            $l2_var = 0;
            $end = 1;
            next;
         }      
         elsif(/^\.VAR(.*)/){
            print OUT3 "<p>\n<p></pre>\n$rest_of_parm{$l2varname[$v2,1]}\n";
            $l2_var = 1;
            $d2 = 1;
            ($junk, $l2varname[++$v2,$d2]) = split(/\s+/,$_);
            $lc = $l2varname[$v2,1];
            $lc =~ tr/A-Z/a-z/;
            print OUT3 qq(\n<hr>\n<A NAME="$lc"><H3>$l2varname[$v2,1]</H3></A><pre>\n);
            next;
         }
         else{
###            eval $pdf_translations;
            eval $parm_translations;
            $l2varname[$v2,++$d2] = $_;
            print OUT3 "$l2varname[$v2,$d2]\n";
            next;
         }      
      }

   # We're in level 1 but not in any specific variable

      elsif ($levelone){
         if (/^\.LEVEL2(.*)/) {
            $levelone = 0;
            $l1_var = 0;
            $leveltwo = 1;
            next;
         }      
         elsif(/^\.VAR(.*)/){
            $l1_var = 1;
            $d1 = 1;
            ($junk, $l1varname[$v1,$d1]) = split(/\s+/,$_);
            $lc = $l1varname[$v1,$d1];
            $lc =~ tr/A-Z/a-z/;
            print OUT1 qq(<HR><A HREF="$crosslink_prefix$file3#$lc"><H3>$l1varname[$v1,1]</H3></A>\n);
            next;
         }
      }

   # We're in level 2 but not in any specific variable

      elsif ($leveltwo){
         if(/^\.VAR(.*)/){
            $l2_var = 1;
            $d2 = 1;
            ($junk, $l2varname[$v2,$d2]) = split(/\s+/,$_);
            $lc = $l2varname[$v2,$d2];
            $lc =~ tr/A-Z/a-z/;
            print OUT3 qq(<A NAME="$lc"><H3>$l2varname[$v2,1]</H3></A>\n<pre>\n);
            next;
         }
      }
      else {
         next;
      }
   }

   if ($examples_flag) {
      print OUT1 qq(<HR><A HREF="$crosslink_prefix$file2#examples"><H4>See Examples:</H4></A>\n);
   }

   if ($cogprog_flag) {
      print OUT1 qq(<HR><A HREF="$crosslink_prefix$file2#CogProg"><H4>Cognizant Programmer:</H4></A>\n);
   }

   close OUT1;
   print OUT2 "</pre>\n<hr>\n";

   if ($end == 0 && $d2 != 0) {		# means we didn't find a .end
      print OUT3 "<p>\n<p></pre>\n$rest_of_parm{$l2varname[$v2,1]}\n";
   }

   if ($index_printed == 0) {		# didn't find .TITLE
      print INDEX_FILE "<A HREF=\"$crosslink_prefix$progname.html\">$title_pn</A>";
      print INDEX_FILE "<p>\n";
   }

   # Append the parameter list to the end of the main help file

   open(OUT1, "<$file1a") || die "Can't open $file1 $!\n";
   while (<OUT1>) {
      print OUT2 $_;
   }
   close OUT1;
   unlink $file1a;

   print OUT2  "</BODY>\n";
   print OUT3  "</BODY>\n";
   print OUT2  "</HTML>\n";
   print OUT3  "</HTML>\n";
   close OUT2;
   close OUT3;

}

print INDEX_FILE "</BODY>\n</HTML>\n";
close INDEX_FILE;

