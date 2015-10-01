#!/usr/bin/perl

# mslKeywords2XSL.pl msl_keywordsV1.2.txt
# /Users/slevoe/MSL/java_development/
# ~/bin/mslKeywords2XSL.pl msl_keywordsV1.2.txt

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
	($in, $out) = split(" ", $line);
	#	print "$lineCt) $line - $in - $out - \n";
	$lineCt++;
	
	# printf	("	<xsl:when test=\"\@key='%s'\"> \n", $in);
	# printf	("		<xsl:attribute name=\"key\">%s</xsl:attribute> \n",  $out);
	# printf	("	</xsl:when> \n\n");
	if ($in =~ /#/) 
	   {		
		# do nothing
		}
else {
	
printf	("  <xsl:when test=\"\@key='%s'\"> \n", $in);
printf	("   <xsl:element name=\"item\"> \n");
printf	("    <xsl:attribute name=\"key\">%s</xsl:attribute> \n",  $out);
printf	("    <xsl:attribute name=\"quoted\"><xsl:value-of select=\"\@quoted\"/></xsl:attribute> \n");
printf	("    <xsl:if test=\"normalize-space(\@units)\">  \n");
printf	("       <xsl:attribute name=\"units\"><xsl:value-of select=\"\@units\"/></xsl:attribute> \n");
printf	("    </xsl:if> \n");
printf	("    <xsl:value-of select=\".\"/>  \n");
printf	("   </xsl:element> \n");
printf	("  </xsl:when>  \n\n");
}
			
}
