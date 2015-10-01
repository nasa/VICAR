package jpl.mipl.io.vicar;

import java.io.*;
import java.util.*;

/**
 * This class parses and returns keyword/value pairs from a VICAR label
 * string (incorporated into a stream).  The end of the stream should
 * correspond to the end of the LBLSIZE-allocated area so EOL can be
 * detected properly in all cases (i.e. you shouldn't simply pass in the
 * file's stream directly).
 */

public class VicarLabelParser
{

    protected Reader _label_stream;
    protected StreamTokenizer _tokenizer;

/***********************************************************************
 * Creates a VicarLabelParser from the given input stream.  Simply wraps
 * the stream in an InputStreamReader and BufferedReader.
 */
    public VicarLabelParser(InputStream s)
    {
	this(new InputStreamReader(s));
//	this(new BufferedReader(new InputStreamReader(s)));
    }

/***********************************************************************
 * Creates a VicarLabelParser from the given Reader class.
 */
    public VicarLabelParser(Reader s)
    {
	_label_stream = s;

	_tokenizer = new StreamTokenizer(_label_stream);

	// Set up the tokenizer table for VICAR labels.  We disallow non-ASCII
	// characters in words to be consistent with old RTL.  Only spaces
	// are valid whitespace, and only single quotes work.

	_tokenizer.resetSyntax();
	_tokenizer.wordChars('A', 'Z');
	_tokenizer.wordChars('a', 'z');
	_tokenizer.wordChars('0', '9');
	_tokenizer.wordChars('_', '_');
	_tokenizer.wordChars('.', '.');
	_tokenizer.wordChars('-', '-');
	_tokenizer.wordChars('+', '+');
	_tokenizer.whitespaceChars(' ', ' ');
	_tokenizer.quoteChar('\'');
    }

/***********************************************************************
 * Parses the next keyword/value pair from the input stream and returns
 * the keyword and value in a VicarLabelItem object.  All whitespace and
 * punctuation (outside of quotes) such as = ( , ) are removed, as are
 * the quotes themselves.  Doubled quotes are replaced by a single quote
 * embedded in the string.  Multivalued label items are split into their
 * component elements in the VicarLabelItem object.
 * <p>
 * @return A VicarLabelItem object, or <code>null</code> if the there are
 * no more pairs.
 * @throws IOException if the stream does.
 * @throws VicarLabelSyntaxException if the label syntax is invalid.
 */
    public VicarLabelItem getNextLabel()
	throws IOException, VicarLabelSyntaxException
    {
	int ttype;
	String keyword;

	// Check for keyword.  If nothing there, we're at the end.
	// 0 is EOL before buffer ends (label end is defined as the first
	// 0 byte, or the end of the LBLSIZE area, which should have been
	// handled by the caller).

	ttype = _tokenizer.nextToken();
	if (ttype == StreamTokenizer.TT_EOF || ttype == 0) {
	    return null;
	}

	if (ttype != StreamTokenizer.TT_WORD)
	    throw new VicarLabelSyntaxException("Label keyword expected");

	keyword = _tokenizer.sval;

	// Check for =.

	ttype = _tokenizer.nextToken();
	if (ttype != '=')
	    throw new VicarLabelSyntaxException(
			"'=' expected after label keyword '"
					+ keyword + "'");

	// Get the value.  Can be either a string, a word token, or a (.

	ttype = _tokenizer.nextToken();
	if (ttype != StreamTokenizer.TT_WORD && ttype != '\'' && ttype != '(')
	    throw new VicarLabelSyntaxException(
			"Invalid label value for keyword '"
					+ keyword + "'");

	if (ttype == StreamTokenizer.TT_WORD || ttype == '\'') {
	    return new VicarLabelItem(keyword, getStringValue(), ttype=='\'');
	}

	// Must be multivalued

	List values_vec = new ArrayList();
	boolean quote = false;

	while (ttype != ')') {
	    // Next token must be a value: string or word.
	    ttype = _tokenizer.nextToken();
	    if (ttype != StreamTokenizer.TT_WORD && ttype != '\'')
		throw new VicarLabelSyntaxException(
			"Invalid label value for multivalued keyword '"
					+ keyword + "'");

	    values_vec.add(getStringValue());
	    if (ttype == '\'')
		quote = true;

	    // Next must be comma, or the ending paren
	    ttype = _tokenizer.nextToken();
	    if (ttype != ',' && ttype != ')')
		throw new VicarLabelSyntaxException(
			"Missing comma or end paren for multivalued keyword '"
					+ keyword + "'");

	    // Loop test will check for ending paren
	}
	return new VicarLabelItem(keyword, values_vec, quote);
    }

/***********************************************************************
 * Returns the string value from the tokenizer.  If it's a quoted string,
 * peek at the next token to see if it is <em>also</em> a quote, in which
 * case we concatenate the strings together with a ' in between (doubled
 * quotes represent a single quote inside the string).
 * <p>
 * White space is temporarily disabled during this peek-ahead since the
 * quotes must be truly doubled.  If we *do* get a space, we don't push it
 * back since it's not really a token to anyone else.
 * <p>
 * It is assumed here that the current token is either a ' quoted string,
 * or a word token.
 */
    protected String getStringValue() throws IOException
    {
	// Check for the simple case - not a quoted string

	if (_tokenizer.ttype != '\'')
	    return _tokenizer.sval;

	// It's a quote... save sval and check for another

	String str = _tokenizer.sval;

	while (_tokenizer.ttype == '\'') {
	    _tokenizer.ordinaryChar(' ');
	    int ttype = _tokenizer.nextToken();
	    _tokenizer.whitespaceChars(' ', ' ');

	    if (ttype == '\'') {	// another quote!
		str = str + '\'' + _tokenizer.sval;
	    }
	    else {			// Nope, put everything back
		if (ttype != ' ') {
		    _tokenizer.pushBack();
		}
	    }
	}

	return str;
    }

}

