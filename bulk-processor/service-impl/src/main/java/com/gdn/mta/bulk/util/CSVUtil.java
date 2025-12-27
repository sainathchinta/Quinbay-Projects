package com.gdn.mta.bulk.util;

/*
 * Write files in comma separated value format.
 * Copyright (C) 2001-2003 Stephen Ostermiller
 * http://ostermiller.org/contact.pl?regarding=Java+Utilities
 * Copyright (C) 2003 Pierre Dittgen <pierre dot dittgen at pass-tech dot fr>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * See COPYING.TXT for details.
 */


import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.Writer;

/**
 * Print values as a comma separated list.
 * More information about this class is available from <a target="_top" href=
 * "http://ostermiller.org/utils/CSV.html">ostermiller.org</a>.
 *
 * @author Stephen Ostermiller http://ostermiller.org/contact.pl?regarding=Java+Utilities
 * @author Pierre Dittgen <pierre dot dittgen at pass-tech dot fr>
 * @since ostermillerutils 1.00.00
 */
public class CSVUtil {

	/**
	 * Delimiter character written.
	 *
	 * @since ostermillerutils 1.02.18
	 */
	protected char delimiterChar = ',';

	/**
	 * Quoting character written.
	 *
	 * @since ostermillerutils 1.02.18
	 */
	protected char quoteChar = '"';

	/**
	 * The place that the values get written.
	 *
	 * @since ostermillerutils 1.00.00
	 */
	protected PrintWriter out;

	/**
	 * True iff we just began a new line.
	 *
	 * @since ostermillerutils 1.00.00
	 */
	protected boolean newLine = true;

	/**
	 * Character used to start comments. (Default is '#')
	 *
	 * @since ostermillerutils 1.00.00
	 */
	protected char commentStart = '#';

	/**
	 * Create a printer that will print values to the given
	 * stream.	 Character to byte conversion is done using
	 * the default character encoding.	Comments will be
	 * written using the default comment character '#'.
	 *
	 * @param out stream to which to print.
	 *
	 * @since ostermillerutils 1.00.00
	 */
	public CSVUtil(OutputStream out){
		this.out = new PrintWriter(out);
	}

	/**
	 * Create a printer that will print values to the given
	 * stream.	Comments will be
	 * written using the default comment character '#'.
	 *
	 * @param out stream to which to print.
	 *
	 * @since ostermillerutils 1.00.00
	 */
	public CSVUtil(Writer out){
		if (out instanceof PrintWriter){
			this.out = (PrintWriter)out;
		} else {
			this.out = new PrintWriter(out);
		}
	}

	/**
	 * Print the string as the last value on the line.	The value
	 * will be quoted if needed. If value is null, an empty value is printed.
	 *
	 * @param value value to be outputted.
	 *
	 * @since ostermillerutils 1.00.00
	 */
	public void println(String value){
		print(value);
		out.println();
		out.flush();
		newLine = true;
	}

	/**
	 * Start a new line.
	 *
	 * @since ostermillerutils 1.00.00
	 */
	public void println(){
		out.println();
		out.flush();
		newLine = true;
	}

	/**
	 * Print a single line of comma separated values.
	 * The values will be quoted if needed.  Quotes and
	 * newLine characters will be escaped.
	 *
	 * @param values values to be outputted.
	 * @throws NullPointerException if values is null.
	 *
	 * @since ostermillerutils 1.00.00
	 */
	public void println(String[] values){
		for (int i=0; i<values.length; i++){
			print(values[i]);
		}
		out.println();
		out.flush();
		newLine = true;
	}

	/**
	 * Print several lines of comma separated values.
	 * The values will be quoted if needed.  Quotes and
	 * newLine characters will be escaped.
	 *
	 * @param values values to be outputted.
	 * @throws NullPointerException if values is null.
	 *
	 * @since ostermillerutils 1.00.00
	 */
	public void println(String[][] values){
		for (int i=0; i<values.length; i++){
			println(values[i]);
		}
		if (values.length == 0){
			out.println();
		}
		out.flush();
		newLine = true;
	}

	/**
	 * Put a comment among the comma separated values.
	 * Comments will always begin on a new line and occupy a
	 * least one full line. The character specified to star
	 * comments and a space will be inserted at the beginning of
	 * each new line in the comment.  If the comment is null,
	 * an empty comment is outputted.
	 *
	 * @param comment the comment to output.
	 *
	 * @since ostermillerutils 1.00.00
	 */
	public void printlnComment(String comment){
		if (comment==null) comment = "";
		if (!newLine){
			out.println();
		}
		out.print(commentStart);
		out.print(' ');
		for (int i=0; i<comment.length(); i++){
			char c = comment.charAt(i);
			switch (c){
				case '\r': {
					if (i+1 < comment.length() && comment.charAt(i+1) == '\n'){
						i++;
					}
				}
				case '\n': {
					out.println();
					out.print(commentStart);
					out.print(' ');
				} break;
				default: {
					out.print(c);
				} break;
			}
		}
		out.println();
		out.flush();
		newLine = true;
	}

	/**
	 * Print the string as the next value on the line.	The value
	 * will be quoted if needed. If value is null, an empty value is printed.
	 *
	 * @param value value to be outputted.
	 *
	 * @since ostermillerutils 1.00.00
	 */
	public void print(String value){
		if (value == null) value = "";
		boolean quote = false;
		if (value.length() > 0){
			char c = value.charAt(0);
			if (newLine && (c<'0' || (c>'9' && c<'A') || (c>'Z' && c<'a') || (c>'z'))){
				quote = true;
			}
			if (c==' ' || c=='\f' || c=='\t'){
				quote = true;
			}
			for (int i=0; i<value.length(); i++){
				c = value.charAt(i);
				if (c==quoteChar || c==delimiterChar || c=='\n' || c=='\r'){
					quote = true;
				}
			}
			if (c==' ' || c=='\f' || c=='\t'){
				quote = true;
			}
		} else if (newLine) {
			quote = true;
		}
		if (newLine){
			newLine = false;
		} else {
			out.print(delimiterChar);
		}
		if (quote){
			out.print(escapeAndQuote(value));
		} else {
			out.print(value);
		}
		out.flush();
	}

	/**
	 * Enclose the value in quotes and escape the quote
	 * and comma characters that are inside.
	 *
	 * @param value needs to be escaped and quoted
	 * @return the value, escaped and quoted.
	 *
	 * @since ostermillerutils 1.00.00
	 */
	private String escapeAndQuote(String value){
		int count = 2;
		for (int i=0; i<value.length(); i++){
			char c = value.charAt(i);
			switch(c){
				case '\n': case '\r': case '\\': {
					count ++;
				} break;
				default: {
					if (c == quoteChar){
						count++;
					}
				} break;
			}
		}
		StringBuilder sb = new StringBuilder(value.length() + count);
		sb.append(quoteChar);
		for (int i=0; i<value.length(); i++){
			char c = value.charAt(i);
			switch(c){
				case '\n': {
					sb.append("\\n");
				} break;
				case '\r': {
					sb.append("\\r");
				} break;
				case '\\': {
					sb.append("\\\\");
				} break;
				default: {
					if (c == quoteChar){
						sb.append("\"" + quoteChar);
					} else {
						sb.append(c);
					}
				}
			}
		}
		sb.append(quoteChar);
		return (sb.toString());
	}
}
