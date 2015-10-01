#!/usr/bin/perl

# mslKeywords2RemoveXSL.pl msl_removed_keywordsV1.0.txt
# /Users/slevoe/MSL/java_development/
# ~/bin/mslKeywords2RemoveXSL.pl msl_removed_keywordsV1.0.txt

$file = $ARGV[0];


print "<!-- file $file --> \n";
open(FILE, $file) or die "can't open $file \n";

# open the file
# read thru one line at a time
$lineCt = 0;
# print "read a line after opening $file \n";

while (<FILE>) {
	chomp();
	$line = $_;
	$in = trim( $line);
	# print "$lineCt) $line - $in - \n";
	$lineCt++;
	
	if ($in =~ /#/) 
	   {
		print " ";
		# do nothing
		}
	else {
		printf	("	<xsl:when test=\"\@key='%s'\"> \n", $in);
		# printf	("		<xsl:attribute name=\"key\">%s</xsl:attribute> \n",  $out);
		printf	("	</xsl:when> \n\n");
	}
}


sub trim($)
{
	my $string = shift;
	$string =~ s/^\s+//;
	$string =~ s/\s+$//;
	return $string;
}