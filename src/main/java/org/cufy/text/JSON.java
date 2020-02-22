/*
 * Copyright (c) 2019, LSafer, All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * -You can edit this file (except the header).
 * -If you have change anything in this file. You
 *   shall mention that this file has been edited.
 *   By adding a new header (at the bottom of this header)
 *   with the word "Editor" on top of it.
 */
package org.cufy.text;

import cufy.beans.StaticMethod;
import cufy.lang.Global;
import cufy.lang.Recurse;
import cufy.lang.Type;
import cufy.text.Format;
import cufy.text.FormatException;
import cufy.text.ParseException;
import cufy.util.Array$;
import cufy.util.Reader$;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.text.NumberFormat;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;

/**
 * A formatter/parser for JSON.
 *
 * <ul>
 *     <font color="orange" size="4"><b>Dynamic Methods:</b></font>
 *     <li>
 *         <font color="yellow">{@link Collection Array}</font>
 *         <ul>
 *             	<li>{@link #formatArray format}</li>
 *         		<li>{@link #isArray classify}</li>
 *         		<li>{@link #parseArray parse}</li>
 *         </ul>
 *     </li>
 *     <li>
 *         <font color="yellow">{@link Boolean}</font>
 *     		<ul>
 *     		 	<li>{@link #formatBoolean format}</li>
 *     			<li>{@link #isBoolean classify}</li>
 *     			<li>{@link #parseBoolean parse}</li>
 *     		</ul>
 *     </li>
 *     <li>
 *         <font color="yellow">{@link Number}</font>
 *         <ul>
 *             	<li>{@link #formatNumber format}</li>
 *             	<li>{@link #isNumber classify}</li>
 *             	<li>{@link #parseNumber parse}</li>
 *         </ul>
 *     </li>
 *     <li>
 *         <font color="yellow">{@link Map Object}</font>
 *         <ul>
 *            	<li>{@link #formatObject format}</li>
 *            	<li>{@link #isObject classify}</li>
 *            	<li>{@link #parseObject parse}</li>
 *         </ul>
 *     </li>
 *     <li>
 *         <font color="yellow">{@link Recurse}</font>
 *         <ul>
 *             <li>{@link #formatRecurse format}</li>
 *             <li>{@link #isRecursive classify}</li>
 *             <li>{@link #parseRecurse parse}</li>
 *         </ul>
 *     </li>
 *     <li>
 *         <font color="yellow">{@link CharSequence String}</font>
 *         <ul>
 *             	<li>{@link #formatString format}</li>
 *             	<li>{@link #isString classify}</li>
 *             	<li>{@link #parseString parse}</li>
 *         </ul>
 *     </li>
 *     <li>
 *         <font color="yellow">{@link Object Else}</font>
 *         <ul>
 *             <li>{@link #formatElse format}</li>
 *         </ul>
 *     </li>
 *     <li>
 *         <font color="yellow">{@link Void Null}</font>
 *         <ul>
 *         		<li>{@link #formatNull foramt}</li>
 *         		<li>{@link #isNull classify}</li>
 *         		<li>{@link #parseNull parse}</li>
 *         </ul>
 *     </li>
 * </ul>
 *
 * @author LSaferSE
 * @version 12 release (02-Feb-2020)
 * @apiNote you can change the syntax of this (by inheriting this)
 * @see <a href="http://www.json.org/">json.org</a> for more about JSON
 * @since 09-Jul-19
 */
public class JSON extends Format implements Global {
	/**
	 * The global instance to avoid unnecessary instancing.
	 */
	final public static JSON global = JSON.newInstance();

	/**
	 * The expected number of members on objects or arrays.
	 */
	protected int DEFAULT_MEMBERS_COUNT;
	/**
	 * The expected depth for nested arrays and object.
	 */
	protected int DEFAULT_NESTING_DEPTH;
	/**
	 * The number of characters expected for values.
	 */
	protected int DEFAULT_VALUE_LENGTH;
	/**
	 * The number of whitespaces characters expected to be read continuously.
	 *
	 * @implNote larger number will effect the RAM. Lower number will effect the performance
	 */
	protected int DEFAULT_WHITE_SPACE_LENGTH;

	/**
	 * The format instance to format/parse numbers with.
	 */
	protected NumberFormat NUMBER_FORMAT;
	/**
	 * The relationships between strings and theirs escapes.
	 */
	protected Map<String, String> STRING_ESCAPABLES;
	/**
	 * The symbols of this.
	 */
	protected Syntax SYNTAX;
	/**
	 * The literal symbols relationships for {@link SyntaxTracker}.
	 *
	 * @apiNote final after initialization
	 */
	protected Map<String, String> SYNTAX_LITERAL;
	/**
	 * The nestable symbols relationships for {@link SyntaxTracker}.
	 *
	 * @apiNote final after initialization
	 */
	protected Map<String, String> SYNTAX_NESTABLE;

	/**
	 * Get a new {@link JSON} instance, With the default syntax.
	 *
	 * @return a new JSON instance with the default syntax
	 */
	public static JSON newInstance() {
		JSON json = new JSON();

		json.DEBUGGING = false;
		json.DEFAULT_MEMBERS_COUNT = 10;
		json.DEFAULT_NESTING_DEPTH = 5;
		json.DEFAULT_VALUE_LENGTH = 20;
		json.DEFAULT_WHITE_SPACE_LENGTH = 20;

		json.NUMBER_FORMAT = NumberFormat.getInstance(Locale.ENGLISH);

		json.STRING_ESCAPABLES = new HashMap<>();
		json.STRING_ESCAPABLES.put("\\", "\\\\");
		json.STRING_ESCAPABLES.put("\"", "\\\"");
		json.STRING_ESCAPABLES.put("\n", "\\\n");
		json.STRING_ESCAPABLES.put("\r", "\\\r");
		json.STRING_ESCAPABLES.put("\t", "\\\t");

		json.SYNTAX = new Syntax();

		json.SYNTAX_NESTABLE = new HashMap<>();
		json.SYNTAX_NESTABLE.put(json.SYNTAX.OBJECT_START, json.SYNTAX.OBJECT_END);
		json.SYNTAX_NESTABLE.put(json.SYNTAX.ARRAY_START, json.SYNTAX.ARRAY_END);

		json.SYNTAX_LITERAL = new HashMap<>();
		json.SYNTAX_LITERAL.put(json.SYNTAX.STRING_START, json.SYNTAX.STRING_END);
		json.SYNTAX_LITERAL.put(json.SYNTAX.COMMENT_START, json.SYNTAX.COMMENT_END);
		json.SYNTAX_LITERAL.put(json.SYNTAX.LINE_COMMENT_START, json.SYNTAX.LINE_COMMENT_END);

		return json;
	}

	@Override
	@StaticMethod
	protected void formatElse(Writer writer, Object object, Format.FormatPosition position) throws IOException {
		if (DEBUGGING) {
			Objects.requireNonNull(writer, "writer");
			Objects.requireNonNull(position, "position");
		}

		this.formatString(writer, object.toString());
	}

	@Override
	@StaticMethod
	protected JSONFormatPosition newFormatPosition() {
		return new JSONFormatPosition();
	}
	@Override
	@StaticMethod
	protected JSONParsePosition newParsePosition() {
		return new JSONParsePosition();
	}

	/**
	 * Format the given {@link Collection Array}. To a {@link JSON} text. Then {@link Writer#append} it to the given {@link Writer}.
	 *
	 * @param array    to be formatted
	 * @param writer   to append to
	 * @param position to format depending on
	 * @throws FormatException          when any formatting errors occurs
	 * @throws IOException              when any I/O exception occurs
	 * @throws NullPointerException     if any of the given parameters is null
	 * @throws IllegalArgumentException if the given 'array' is neither a collection nor an array
	 */
	@FormatMethod(
			@Type(
					subin = {Collection.class,
							 Object[].class,
					},
					in = {boolean[].class,
						  byte[].class,
						  char[].class,
						  double[].class,
						  float[].class,
						  int[].class,
						  long[].class,
						  short[].class
					}))
	protected void formatArray(Writer writer, Object array, JSONFormatPosition position) throws IOException {
		if (DEBUGGING) {
			Objects.requireNonNull(array, "array");
			Objects.requireNonNull(writer, "writer");
			Objects.requireNonNull(position, "position");

			if (!(array instanceof Collection) && !array.getClass().isArray())
				throw new IllegalArgumentException(array + " neither a collection nor an array");
		}

		Iterator<?> iterator = array instanceof Collection ? ((Collection) array).iterator() : Array$.iterator0(array);

		if (!iterator.hasNext()) {
			writer.append(SYNTAX.ARRAY_START)
					.append(SYNTAX.LINE)
					.append(position.tab)
					.append(SYNTAX.ARRAY_END);
		} else {
			writer.append(SYNTAX.ARRAY_START)
					.append(SYNTAX.LINE)
					.append(position.shift);

			while (true) {
				position.format(writer, iterator.next(), null, null, array, null, null);

				if (!iterator.hasNext()) {
					writer.append(SYNTAX.LINE)
							.append(position.tab)
							.append(SYNTAX.ARRAY_END);
					return;
				}

				writer.append(SYNTAX.MEMBER_END)
						.append(SYNTAX.LINE)
						.append(position.shift);
			}
		}
	}
	/**
	 * Format the given {@link Boolean}. To a {@link JSON} text. Then {@link Writer#append} it to the given {@link Writer}.
	 *
	 * @param bool   to be formatted
	 * @param writer to append to
	 * @throws FormatException      when any formatting errors occurs
	 * @throws IOException          when any I/O exception occurs
	 * @throws NullPointerException if any of the given parameters is null
	 */
	@FormatMethod(
			@Type(
					in = {Boolean.class,
						  boolean.class
					}))
	protected void formatBoolean(Writer writer, Boolean bool) throws IOException {
		if (DEBUGGING) {
			Objects.requireNonNull(bool, "bool");
			Objects.requireNonNull(writer, "writer");
		}

		writer.append(bool ? SYNTAX.TRUE : SYNTAX.FALSE);
	}
	/**
	 * Append null to the given writer.
	 *
	 * @param writer to append to
	 * @throws FormatException      when any formatting errors occurs
	 * @throws IOException          when any I/O exception occurs
	 * @throws NullPointerException if any of the given parameters is null
	 */
	@FormatMethod(
			@Type(
					in = Void.class
			))
	protected void formatNull(Writer writer) throws IOException {
		if (DEBUGGING) {
			Objects.requireNonNull(writer, "writer");
		}

		writer.append(SYNTAX.NULL);
	}
	/**
	 * Format the given {@link Number}. To a {@link JSON} text. Then {@link Writer#append} it to the given {@link Writer}.
	 *
	 * @param number to be formatted
	 * @param writer to append to
	 * @throws FormatException      when any formatting errors occurs
	 * @throws IOException          when any I/O exception occurs
	 * @throws NullPointerException if any of the given parameters is null
	 */
	@FormatMethod(
			@Type(
					subin = Number.class,
					in = {
							double.class,
							float.class,
							int.class,
							long.class,
							short.class
					}))
	protected void formatNumber(Writer writer, Number number) throws IOException {
		if (DEBUGGING) {
			Objects.requireNonNull(number, "number");
			Objects.requireNonNull(writer, "writer");
		}

		writer.append(NUMBER_FORMAT.format(number));
	}
	/**
	 * Format the given {@link Map Object}. To a {@link JSON} text. Then {@link Writer#append} it to the given {@link Writer}.
	 *
	 * @param object   to be formatted
	 * @param writer   to append to
	 * @param position to format depending on
	 * @throws FormatException      when any formatting errors occurs
	 * @throws IOException          when any I/O exception occurs
	 * @throws NullPointerException if any of the given parameters is null
	 */
	@FormatMethod(
			@Type(
					subin = Map.class
			))
	protected void formatObject(Writer writer, Map<?, ?> object, JSONFormatPosition position) throws IOException {
		if (DEBUGGING) {
			Objects.requireNonNull(object, "object");
			Objects.requireNonNull(writer, "writer");
			Objects.requireNonNull(position, "position");
		}

		Iterator<? extends Map.Entry<?, ?>> iterator = object.entrySet().iterator();

		if (!iterator.hasNext()) {
			writer.append(SYNTAX.OBJECT_START)
					.append(SYNTAX.LINE)
					.append(position.tab)
					.append(SYNTAX.OBJECT_END);
		} else {
			writer.append(SYNTAX.OBJECT_START)
					.append(SYNTAX.LINE)
					.append(position.shift);

			while (true) {
				Map.Entry<?, ?> entry = iterator.next();

				position.format(writer, entry.getKey(), null, null, object, null, null);
				writer.append(SYNTAX.DECLARATION);
				position.format(writer, entry.getValue(), null, null, object, null, null);

				if (!iterator.hasNext()) {
					writer.append(SYNTAX.LINE)
							.append(position.tab)
							.append(SYNTAX.OBJECT_END);
					return;
				}

				writer.append(SYNTAX.MEMBER_END)
						.append(SYNTAX.LINE)
						.append(position.shift);
			}
		}
	}
	/**
	 * Format the given {@link Recurse}. To a {@link JSON} text. Then {@link Writer#append} it to the given {@link Writer}.
	 *
	 * @param recurse  to be formatted
	 * @param writer   to append to
	 * @param position to format depending on
	 * @throws FormatException      when any formatting errors occurs
	 * @throws IOException          when any I/O exception occurs
	 * @throws NullPointerException if any of the given parameters is null
	 */
	@FormatMethod(
			@Type(
					in = Recurse.class
			))
	protected void formatRecurse(Writer writer, Object recurse, JSONFormatPosition position) throws IOException {
		if (DEBUGGING) {
			Objects.requireNonNull(recurse, "recurse");
			Objects.requireNonNull(writer, "writer");
			Objects.requireNonNull(position, "position");
		}

		int index = position.parents.indexOf(recurse);

		if (index == -1) {
			throw new IllegalArgumentException("Not recurse!: " + recurse);
		} else {
			writer.append(SYNTAX.RECURSE).append(String.valueOf(position.parents.size() - 1 - index));
		}
	}
	/**
	 * Format the given {@link CharSequence String}. To a {@link JSON} text. Then {@link Writer#append} it to the given {@link Writer}.
	 *
	 * @param string to be formatted
	 * @param writer to append to
	 * @throws FormatException      when any formatting errors occurs
	 * @throws IOException          when any I/O exception occurs
	 * @throws NullPointerException if any of the given parameters is null
	 */
	@FormatMethod(
			@Type(
					subin = CharSequence.class
			))
	protected void formatString(Writer writer, CharSequence string) throws IOException {
		if (DEBUGGING) {
			Objects.requireNonNull(string, "string");
			Objects.requireNonNull(writer, "writer");
		}

		String value = string.toString();

		for (Map.Entry<String, String> escapable : STRING_ESCAPABLES.entrySet())
			value = value.replace(escapable.getKey(), escapable.getValue());

		writer.append(SYNTAX.STRING_START)
				.append(value)
				.append(SYNTAX.STRING_END);
	}

	/**
	 * Check if the given string should be parsed as {@link Collection Array} or not.
	 *
	 * @param reader to read from
	 * @return whether the given string should be parsed as {@code array} or not.
	 * @throws ParseException       when any parsing exception occurs
	 * @throws IOException          when any I/O exception occurs
	 * @throws NullPointerException if any of the given parameters is null
	 */
	@ClassifyMethod(Collection.class)
	protected boolean isArray(Reader reader) throws IOException {
		if (DEBUGGING) {
			Objects.requireNonNull(reader, "reader");
		}

		reader.mark(DEFAULT_WHITE_SPACE_LENGTH + SYNTAX.ARRAY_START.length());
		int r = Reader$.isRemainingEquals(reader, true, false, false, SYNTAX.ARRAY_START);
		reader.reset();
		return r != -1;
	}
	/**
	 * Check if the given string should be parsed as {@link Boolean} or not.
	 *
	 * @param reader to read from
	 * @return whether the given string should be parsed as {@code boolean} or not.
	 * @throws ParseException       when any parsing exception occurs
	 * @throws IOException          when any I/O exception occurs
	 * @throws NullPointerException if any of the given parameters is null
	 */
	@ClassifyMethod(Boolean.class)
	protected boolean isBoolean(Reader reader) throws IOException {
		if (DEBUGGING) {
			Objects.requireNonNull(reader, "reader");
		}

		reader.mark(DEFAULT_WHITE_SPACE_LENGTH + Math.max(SYNTAX.TRUE.length(), SYNTAX.FALSE.length()));
		int r = Reader$.isRemainingEquals(reader, true, true, true, SYNTAX.TRUE, SYNTAX.FALSE);
		reader.reset();
		return r != -1;
	}
	/**
	 * Check if the given tracker is in a comment literal or not.
	 *
	 * @param tracker to check for
	 * @return if the given tracker is in comment literal or not
	 * @throws NullPointerException if the given tracker is null
	 */
	@StaticMethod
	protected boolean isInComment(SyntaxTracker tracker) {
		if (DEBUGGING) {
			Objects.requireNonNull(tracker, "tracker");
		}

		String pos = tracker.getUnwrap();
		return pos.equals(SYNTAX.COMMENT_END) || pos.equals(SYNTAX.LINE_COMMENT_END);
	}
	/**
	 * Check if the remaining character on the given reader should be parsed as null or not.
	 *
	 * @param reader to read the remaining characters from
	 * @return if the remaining characters on the given reader should be parsed as null or not.
	 * @throws ParseException       when any parsing exception occurs
	 * @throws IOException          when any I/O exception occurs
	 * @throws NullPointerException if any of the given parameters is null
	 */
	@ClassifyMethod(Void.class)
	protected boolean isNull(Reader reader) throws IOException {
		if (DEBUGGING) {
			Objects.requireNonNull(reader, "reader");
		}

		reader.mark(DEFAULT_WHITE_SPACE_LENGTH + SYNTAX.NULL.length());
		int r = Reader$.isRemainingEquals(reader, true, true, true, SYNTAX.NULL);
		reader.reset();
		return r != -1;
	}
	/**
	 * Check if the given string should be parsed as {@link Number} or not.
	 *
	 * @param reader to read from
	 * @return whether the given string should be parsed as {@code number} or not.
	 * @throws ParseException       when any parsing exception occurs
	 * @throws IOException          when any I/O exception occurs
	 * @throws NullPointerException if any of the given parameters is null
	 */
	@ClassifyMethod(Number.class)
	protected boolean isNumber(Reader reader) throws IOException {
		if (DEBUGGING) {
			Objects.requireNonNull(reader, "reader");
		}

		reader.mark(DEFAULT_WHITE_SPACE_LENGTH + 1);
		int r = Reader$.isRemainingEquals(reader, true, false, false, "0", "1", "2", "3", "4", "5", "6", "7", "8", "9");
		reader.reset();

		return r != -1;
	}
	/**
	 * Check if the given string should be parsed as {@link Map Object} or not.
	 *
	 * @param reader to read from
	 * @return whether the given string should be parsed as {@code object} or not.
	 * @throws ParseException       when any parsing exception occurs
	 * @throws IOException          when any I/O exception occurs
	 * @throws NullPointerException if any of the given parameters is null
	 */
	@ClassifyMethod(Map.class)
	protected boolean isObject(Reader reader) throws IOException {
		if (DEBUGGING) {
			Objects.requireNonNull(reader, "reader");
		}

		reader.mark(DEFAULT_WHITE_SPACE_LENGTH + SYNTAX.OBJECT_START.length());
		int r = Reader$.isRemainingEquals(reader, true, false, false, SYNTAX.OBJECT_START);
		reader.reset();

		return r != -1;
	}
	/**
	 * Check if the given string should be parsed as {@link Recurse} or not.
	 *
	 * @param reader to read from
	 * @return whether the given string should be parsed as {@code recurse} or not.
	 * @throws ParseException       when any parsing exception occurs
	 * @throws IOException          when any I/O exception occurs
	 * @throws NullPointerException if any of the given parameters is null
	 */
	@ClassifyMethod(Recurse.class)
	protected boolean isRecursive(Reader reader) throws IOException {
		if (DEBUGGING) {
			Objects.requireNonNull(reader, "reader");
		}

		reader.mark(DEFAULT_WHITE_SPACE_LENGTH + SYNTAX.RECURSE.length());
		int r = Reader$.isRemainingEquals(reader, true, true, true, SYNTAX.RECURSE);
		reader.reset();
		return r != -1;
	}
	/**
	 * Check if the given string should be parsed as {@link CharSequence String} or not.
	 *
	 * @param reader to read from
	 * @return whether the given string should be parsed as {@code string} or not.
	 * @throws ParseException       when any parsing exception occurs
	 * @throws IOException          when any I/O exception occurs
	 * @throws NullPointerException if any of the given parameters is null
	 */
	@ClassifyMethod(CharSequence.class)
	protected boolean isString(Reader reader) throws IOException {
		if (DEBUGGING) {
			Objects.requireNonNull(reader, "reader");
		}

		reader.mark(DEFAULT_WHITE_SPACE_LENGTH + SYNTAX.STRING_START.length());
		int r = Reader$.isRemainingEquals(reader, true, false, false, SYNTAX.STRING_START);
		reader.reset();

		return r != -1;
	}

	/**
	 * Parse the string from the given reader to an {@link Collection Array}. Then set it to the given {@link AtomicReference buffer}.
	 *
	 * @param reader   to read from
	 * @param buffer   to set the parsed object to
	 * @param position to parse the string depending on
	 * @throws ParseException       when any parsing exception occurs
	 * @throws IOException          when any I/O exception occurs
	 * @throws NullPointerException if any of the given parameters is null
	 */
	@ParseMethod(@Type(Collection.class))
	protected void parseArray(AtomicReference<Collection<Object>> buffer, Reader reader, JSONParsePosition position) throws IOException {
		if (DEBUGGING) {
			Objects.requireNonNull(reader, "reader");
			Objects.requireNonNull(buffer, "buffer");
			Objects.requireNonNull(position, "position");
		}

		if (Reader$.isRemainingEquals(reader, true, false, false, SYNTAX.ARRAY_START) != 0)
			throw new ParseException("array not started");

		if (buffer.get() == null)
			buffer.set(new ArrayList<>(DEFAULT_MEMBERS_COUNT));

		SyntaxTracker tracker = new SyntaxTracker(SYNTAX_NESTABLE, SYNTAX_LITERAL);
		StringBuilder builder = new StringBuilder(DEFAULT_VALUE_LENGTH);
		StringBuilder points = new StringBuilder(DEFAULT_VALUE_LENGTH);
		boolean closed = false;
//		//stands for: delete previous if comment
		boolean dpic = true;
		int i;

		while ((i = reader.read()) != -1) {
			char point = (char) i;

			if (closed)
				if (Character.isWhitespace(i))
					continue;
				else throw new ParseException("array closed before text end");
			if (tracker.length() == 0) {
				String past = points.append(point).toString();

				if ((closed = past.endsWith(SYNTAX.ARRAY_END)) || past.endsWith(SYNTAX.MEMBER_END)) {
					{
						Collection tmpArr = buffer.get();
						Object tmpElm;
						{
							AtomicReference<?> tmpBuf = new AtomicReference<>();
							position.parse(tmpBuf, new StringReader(builder.toString().trim()), null, null, buffer);
							tmpElm = tmpBuf.get();
						}
						tmpArr.add(tmpElm);
					}

					builder = new StringBuilder(DEFAULT_VALUE_LENGTH);
					points = new StringBuilder(DEFAULT_VALUE_LENGTH);
					continue;
				}
			} else if (points.length() != 0) {
				points = new StringBuilder(DEFAULT_VALUE_LENGTH);
			}

			tracker.append(point);

			if (isInComment(tracker)) {
				if (dpic) {
					builder.delete(builder.length() - 1, builder.length());
					dpic = false;
				}
			} else if (dpic) {
				builder.append(point);
			} else {
				dpic = true;
			}
		}

		if (!closed)
			throw new ParseException("Collection not closed");
	}
	/**
	 * Parse the string from the given reader to an {@link Boolean}. Then set it to the given {@link AtomicReference buffer}.
	 *
	 * @param reader to read from
	 * @param buffer to set the parsed object to
	 * @throws ParseException       when any parsing exception occurs
	 * @throws IOException          when any I/O exception occurs
	 * @throws NullPointerException if any of the given parameters is null
	 */
	@ParseMethod(@Type(Boolean.class))
	protected void parseBoolean(AtomicReference<Boolean> buffer, Reader reader) throws IOException {
		if (DEBUGGING) {
			Objects.requireNonNull(reader, "reader");
			Objects.requireNonNull(buffer, "buffer");
		}

		String string = Reader$.getRemaining(reader, Math.max(SYNTAX.TRUE.length(), SYNTAX.FALSE.length())).trim();

		if (SYNTAX.TRUE.equals(string)) {
			buffer.set(true);
		} else if (SYNTAX.FALSE.equals(string)) {
			buffer.set(false);
		} else {
			throw new ParseException("Can't parse \"" + string + "\" as boolean");
		}
	}
	/**
	 * Set null to the given buffer.
	 *
	 * @param reader to read from
	 * @param buffer to set the value to
	 * @throws NullPointerException if any of the given parameters is null
	 * @throws IOException          if any I/O exception occurred
	 */
	@ParseMethod(@Type(Void.class))
	protected void parseNull(Reader reader, AtomicReference<Object> buffer) throws IOException {
		if (DEBUGGING) {
			Objects.requireNonNull(buffer, "buffer");

			String string = Reader$.getRemaining(reader, SYNTAX.NULL.length(), SYNTAX.NULL.length());
			if (!string.equalsIgnoreCase(SYNTAX.NULL))
				throw new ParseException("can't parse \"" + string + "\" as null");
		}

		buffer.set(null);
	}
	/**
	 * Parse the string from the given reader to an {@link Number}. Then set it to the given {@link AtomicReference buffer}.
	 *
	 * @param reader to read from
	 * @param buffer to set the parsed object to
	 * @throws ParseException           when any parsing exception occurs
	 * @throws IOException              when any I/O exception occurs
	 * @throws NullPointerException     if any of the given parameters is null
	 * @throws java.text.ParseException if the number on the string can't be parsed
	 */
	@ParseMethod(@Type(Number.class))
	protected void parseNumber(AtomicReference<Number> buffer, Reader reader) throws IOException, java.text.ParseException {
		if (DEBUGGING) {
			Objects.requireNonNull(reader, "reader");
			Objects.requireNonNull(buffer, "buffer");
		}

		String string = Reader$.getRemaining(reader, DEFAULT_VALUE_LENGTH, DEFAULT_VALUE_LENGTH).trim();
		Number value = NUMBER_FORMAT.parse(string);
		buffer.set(value);
	}
	/**
	 * Parse the string from the given reader to an {@link Map Object}. Then set it to the given {@link AtomicReference buffer}.
	 *
	 * @param reader   to read from
	 * @param buffer   to set the parsed object to
	 * @param position to parse the string depending on
	 * @throws ParseException       when any parsing exception occurs
	 * @throws IOException          when any I/O exception occurs
	 * @throws NullPointerException if any of the given parameters is null
	 */
	@ParseMethod(@Type(Map.class))
	protected void parseObject(AtomicReference<Map<Object, Object>> buffer, Reader reader, JSONParsePosition position) throws IOException {
		if (DEBUGGING) {
			Objects.requireNonNull(reader, "reader");
			Objects.requireNonNull(buffer, "buffer");
			Objects.requireNonNull(position, "position");
		}

		//pre operations
		if (Reader$.isRemainingEquals(reader, true, false, false, SYNTAX.OBJECT_START) != 0)
			throw new ParseException("Object not started");
		if (buffer.get() == null)
			buffer.set(new HashMap<>(DEFAULT_MEMBERS_COUNT));

		//public declarations
		Set keys = new HashSet();
		boolean closed = false;

		//content parsing
		{
			//syntax manager
			SyntaxTracker tracker = new SyntaxTracker(SYNTAX_NESTABLE, SYNTAX_LITERAL);
			//content reading buffer (for key and value)
			StringBuilder builder = new StringBuilder(DEFAULT_VALUE_LENGTH);
			//key holder
			StringBuilder key = null;
			//short backtrace
			StringBuilder backtrace = new StringBuilder(DEFAULT_VALUE_LENGTH);
			//reading integer buffer
			int i;

			//comment mode
			boolean comment = false;

			//foreach character
			while ((i = reader.read()) != -1) {
				//reading character buffer
				char point = (char) i;

				if (closed)
					//only white spaces are allowed when closed
					if (Character.isWhitespace(i))
						continue;
					else throw new ParseException("Object closed before text end");
				if (tracker.length() == 0) {
					//complex inner pre-operations
					backtrace.append(point);

					//represent the short past string
					String past = backtrace.toString();

					if (past.endsWith(SYNTAX.DECLARATION) || past.endsWith(SYNTAX.EQUATION)) {
						//key reading mode is over| value reading mode
						if (key != null)
							throw new ParseException("Two equation symbol");

						//switch reading destination address
						key = builder;

						builder = new StringBuilder(DEFAULT_VALUE_LENGTH);
						backtrace = new StringBuilder(DEFAULT_VALUE_LENGTH);
						continue;
					} else if ((closed = past.endsWith(SYNTAX.OBJECT_END)) || past.endsWith(SYNTAX.MEMBER_END)) {
						//value reading mode is over| key reading mode
						if (key == null)
							throw new ParseException("No equation symbol");

						//resolve member
						{
							Map tmpObj = buffer.get();
							Object tmpKey;
							Object tmpVal;
							Object oldVal;

							//resolve key
							{
								AtomicReference<?> tmpBuf = new AtomicReference<>();
								position.parse(tmpBuf, new StringReader(key.toString().trim()), null, null, buffer);
								tmpKey = tmpBuf.get();
							}

							//value to overwrite (use the same instance) (if possible)
							oldVal = tmpObj.get(tmpKey);

							//resolve value
							{
								AtomicReference<?> tmpBuf = new AtomicReference<>(oldVal);
								position.parse(tmpBuf, new StringReader(builder.toString().trim()), null, null, buffer);
								tmpVal = tmpBuf.get();
							}

							if (tmpVal != oldVal)
								//value instance overwriting is not possible
								tmpObj.put(tmpKey, tmpVal);

							//keys tracking
							if (keys.contains(tmpKey))
								throw new ParseException("duplicated key: " + tmpKey);
							else keys.add(tmpKey);
						}

						//switch reading destination addresses
						key = null;

						builder = new StringBuilder(DEFAULT_VALUE_LENGTH);
						backtrace = new StringBuilder(DEFAULT_VALUE_LENGTH);
						continue;
					}
				} else if (backtrace.length() != 0) {
					//reset short backtrace
					backtrace = new StringBuilder(DEFAULT_VALUE_LENGTH);
				}

				//notify syntax manager
				tracker.append(point);

				if (isInComment(tracker)) {
					//currently in comment mode
					if (!comment) {
						//delete the comment open symbol
						builder.deleteCharAt(builder.length() - 1);
						comment = true;
					}
				} else if (comment) {
					//comment mode is over (ignore the comment close symbol)
					comment = false;
				} else {
					//append the reading content buffer
					builder.append(point);
				}
			}
		}

		//remove missing keys!
		{
			//tmp declarations
			Map tmpObj = buffer.get();
			Set rmvSet = new HashSet();

			for (Object k : tmpObj.keySet())
				if (!keys.contains(k))
					//key is missing from the source
					rmvSet.add(k);

			//remove missing keys
			for (Object k : rmvSet)
				tmpObj.remove(k);
		}

		if (!closed)
			throw new ParseException("Map not closed");
	}
	/**
	 * Parse the string from the given reader to an {@link Recurse}. Then set it to the given {@link AtomicReference buffer}.
	 *
	 * @param reader   to read from
	 * @param buffer   to set the parsed object to
	 * @param position to parse the string depending on
	 * @throws ParseException           when any parsing exception occurs
	 * @throws IOException              when any I/O exception occurs
	 * @throws NullPointerException     if any of the given parameters is null
	 * @throws java.text.ParseException when any parsing exception occurs
	 */
	@ParseMethod(@Type(Recurse.class))
	protected void parseRecurse(AtomicReference<Object> buffer, Reader reader, JSONParsePosition position) throws IOException, java.text.ParseException {
		if (DEBUGGING) {
			Objects.requireNonNull(reader, "reader");
			Objects.requireNonNull(buffer, "buffer");
			Objects.requireNonNull(position, "position");
		}

		int i = DEFAULT_VALUE_LENGTH + SYNTAX.RECURSE.length();
		String string = Reader$.getRemaining(reader, i, i).trim().substring(SYNTAX.RECURSE.length());
		int index = NUMBER_FORMAT.parse(string).intValue();
		AtomicReference<?> value = position.parents.get(position.parents.size() - 1 - index);
		buffer.set(value.get());
	}
	/**
	 * Parse the string from the given reader to an {@link String}. Then set it to the given {@link AtomicReference buffer}.
	 *
	 * @param reader to read from
	 * @param buffer to set the parsed object to\
	 * @throws ParseException       when any parsing exception occurs
	 * @throws IOException          when any I/O exception occurs
	 * @throws NullPointerException if any of the given parameters is null
	 */
	@ParseMethod(@Type(CharSequence.class))
	protected void parseString(AtomicReference<String> buffer, Reader reader) throws IOException {
		if (DEBUGGING) {
			Objects.requireNonNull(reader, "reader");
			Objects.requireNonNull(buffer, "buffer");
		}

		String string = Reader$.getRemaining(reader, DEFAULT_VALUE_LENGTH, DEFAULT_VALUE_LENGTH).trim();
		String value = string.substring(SYNTAX.STRING_START.length(), string.length() - SYNTAX.STRING_END.length());

		for (Map.Entry<String, String> escapable : STRING_ESCAPABLES.entrySet())
			value.replace(escapable.getValue(), escapable.getKey());

		buffer.set(value);
	}

	/**
	 * A structure holding the symbols of a JSON formatter.
	 *
	 * @implSpec all values should be used as final
	 */
	protected static class Syntax {
		/**
		 * Array end char on JSON.
		 */
		public String ARRAY_END = "]";
		/**
		 * Array start char on JSON.
		 */
		public String ARRAY_START = "[";
		/**
		 * Declare that the comment ended.
		 */
		public String COMMENT_END = "*/";
		/**
		 * Declare that further characters are commented. Until the {@link #COMMENT_END} cancel it.
		 */
		public String COMMENT_START = "/*";
		/**
		 * Pair mapping char on JSON.
		 */
		public String DECLARATION = ":";
		/**
		 * Pair equation char on other JSON like formats.
		 */
		public String EQUATION = "=";
		/**
		 * The value false of the type boolean on JSON.
		 */
		public String FALSE = "false";
		/**
		 * A symbol used to shows a line. To make the code more readable.
		 */
		public String LINE = "\n";
		/**
		 * Declare that the line comment ended.
		 */
		public String LINE_COMMENT_END = "\n";
		/**
		 * Declare that the next characters are commented. Until the {@link #LINE_COMMENT_END} cancel it.
		 */
		public String LINE_COMMENT_START = "//";
		/**
		 * Member end char on JSON.
		 */
		public String MEMBER_END = ",";
		/**
		 * The value null on JSON.
		 */
		public String NULL = "null";
		/**
		 * Object end char on JSON.
		 */
		public String OBJECT_END = "}";
		/**
		 * Object start char on JSON.
		 */
		public String OBJECT_START = "{";
		/**
		 * Recurse reference on JSON.
		 */
		public String RECURSE = "this";
		/**
		 * String start char on JSON.
		 */
		public String STRING_END = "\"";
		/**
		 * String end char on JSON.
		 */
		public String STRING_START = "\"";
		/**
		 * A symbol used to show a gap between characters. TO make the code more readable.
		 */
		public String TAB = "\t";
		/**
		 * The value true of the type boolean on JSON.
		 */
		public String TRUE = "true";
	}

	/**
	 * Helps to effect the formatting behavior depending on the formatting position.
	 */
	public class JSONFormatPosition implements Format.FormatPosition {
		/**
		 * Current parents on this position.
		 *
		 * @implSpec don't modify it after the constructor!
		 */
		final public ArrayList<Object> parents = new ArrayList<>(DEFAULT_NESTING_DEPTH);
		/**
		 * Current spacing from the edge of the text to the start of the shifted value.
		 */
		final public String shift;
		/**
		 * Current spacing from the edge of the text to the start of the value.
		 */
		final public String tab;

		/**
		 * New format position.
		 */
		public JSONFormatPosition() {
			this.shift = SYNTAX.TAB;
			this.tab = "";
		}

		/**
		 * New format position with the given params.
		 *
		 * @param parents all parents currently formatting on
		 * @param parent  the direct parent formatting on
		 * @param tab     spacing from the edge of the text to the start of the value
		 * @param shift   spacing from the edge of the text to the start of the shifted value
		 */
		public JSONFormatPosition(ArrayList<Object> parents, Object parent, String tab, String shift) {
			if (DEBUGGING) {
				Objects.requireNonNull(parents, "parents");
				Objects.requireNonNull(parent, "parent");
				Objects.requireNonNull(tab, "tab");
				Objects.requireNonNull(shift, "shift");
			}

			this.parents.addAll(parents);
			this.parents.add(parent);
			this.tab = tab;
			this.shift = shift;
		}

		/**
		 * Format the given object depending on a sub-position of this position using the formatter of this. Then {@link Writer#append} it to the
		 * given {@link Writer}.
		 *
		 * @param object   to format
		 * @param writer   to write the formatted string to
		 * @param position the position to be used (null for a delegate of this)
		 * @param klass    the targeted method parameter type (null for the class of the given object)
		 * @param parent   the direct parent
		 * @param tab      spacing from the edge of the text to the start of the value (null for a plus tab of the tab of this)
		 * @param shift    spacing from the edge of the text to the start of the shifted value (null for a plus shift of the shift of this)
		 * @throws FormatException      when any formatting errors occurs
		 * @throws IOException          if any I/O exception occurs
		 * @throws NullPointerException if the given 'writer' or 'parent' is null
		 */
		public void format(Writer writer, Object object, JSONFormatPosition position, Class<?> klass, Object parent, String tab, String shift) throws IOException {
			if (DEBUGGING) {
				Objects.requireNonNull(writer, "writer");
				Objects.requireNonNull(parent, "parent");
			}

			if (tab == null)
				tab = this.shift;
			if (shift == null)
				shift = this.shift + SYNTAX.TAB;
			if (position == null)
				position = new JSONFormatPosition(this.parents, parent, tab, shift);
			if (klass == null)
				klass = this.parents.contains(object) ? Recurse.class : null;

			JSON.this.format(writer, object, position, klass);
		}
	}

	/**
	 * Helps to effect the parsing behavior depending on the formatting position.
	 */
	public class JSONParsePosition implements Format.ParsePosition {
		/**
		 * Current parents on this position.
		 */
		final public ArrayList<AtomicReference<?>> parents = new ArrayList<>(DEFAULT_NESTING_DEPTH);

		/**
		 * New position.
		 */
		public JSONParsePosition() {
		}

		/**
		 * New position with the given params.
		 *
		 * @param parents all parents currently formatting on
		 * @param parent  direct parent
		 */
		public JSONParsePosition(ArrayList<AtomicReference<?>> parents, AtomicReference<?> parent) {
			if (DEBUGGING) {
				Objects.requireNonNull(parents, "parents");
				Objects.requireNonNull(parent, "parent");
			}

			this.parents.addAll(parents);
			this.parents.add(parent);
		}

		/**
		 * Parse the string from the given reader depending on a sub-position of this. Then set the parsed object to the given {@link AtomicReference
		 * buffer}.
		 *
		 * @param reader   to read the string from
		 * @param buffer   to store the parsed object to while parsing
		 * @param position to parse the given sequence depending on (null for a delegate of this)
		 * @param klass    the targeted method output type (null for classifying the given sequence dynamically)
		 * @param parent   direct parent
		 * @throws ParseException       when any parsing error occurs
		 * @throws IOException          if any I/O exception occurs
		 * @throws NullPointerException if the given 'buffer' or 'reader' or 'parent' is null
		 */
		public void parse(AtomicReference<?> buffer, Reader reader, JSONParsePosition position, Class<?> klass, AtomicReference<?> parent) throws IOException {
			if (DEBUGGING) {
				Objects.requireNonNull(buffer, "buffer");
				Objects.requireNonNull(reader, "reader");
				Objects.requireNonNull(parent, "parent");
			}

			if (position == null)
				position = new JSONParsePosition(this.parents, parent);

			JSON.this.parse(buffer, reader, position, klass);
		}
	}
}
