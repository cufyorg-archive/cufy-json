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
package org.cufy.lang;

import cufy.cnv.BaseConverter;
import cufy.cnv.ConvertArguments;
import cufy.cnv.ConvertMethod;
import cufy.meta.MetaFamily;
import cufy.meta.MetaReference;
import org.cufy.text.JSON;

import java.io.IOError;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Objects;

/**
 * A converter that supports everything in the {@link BaseConverter}. This converter supports the conversion between {@link String} and other {@link
 * Object}s. Using {@link org.cufy.text.JSON}.
 *
 * @author LSaferSE
 * @version 1 release (25-Jan-2020)
 * @since 25-Jan-2020
 */
public class JSONConverter extends BaseConverter {
	/**
	 * The global instance to avoid unnecessary instancing.
	 */
	@MetaReference
	final public static JSONConverter global = new JSONConverter();

	/**
	 * Object => String
	 * <br/>
	 *
	 * Set the {@link ConvertArguments#output} with a new {@link String} that holds the value of the given {@link ConvertArguments#input}. Using
	 * {@link JSON}.
	 *
	 * @param arguments the conversion instance that holds the variables of this conversion
	 * @throws NullPointerException     if the given 'arguments' is null
	 * @throws IllegalArgumentException if the given 'outputClass' is not {@link String}
	 */
	@ConvertMethod(
			input = @MetaFamily(
					subin = Object.class,
					in = {byte.class,
						  boolean.class,
						  char.class,
						  double.class,
						  float.class,
						  int.class,
						  long.class,
						  short.class
					}),
			output = @MetaFamily(
					in = String.class
			))
	protected void object_string(ConvertArguments<Object, String> arguments) {
		try {
			if (DEBUGGING) {
				Objects.requireNonNull(arguments, "arguments");
			}

			arguments.output = JSON.global.format(arguments.input, new StringWriter(), arguments.inputClazz, arguments.outputClazz).toString();
		} catch (IOException e) {
			throw new IOError(e);
		}
	}

	/**
	 * String => Object
	 * <br/>
	 * Try to construct a new object of the value of the given {@link ConvertArguments#input} with type of the {@link ConvertArguments#outputClazz}.
	 * Using {@link JSON}.
	 *
	 * @param arguments the conversion instance that holds the variables of this conversion
	 * @throws NullPointerException     if the given 'arguments' or 'input' is null
	 * @throws IllegalArgumentException if the given 'input' is not a string. Or if the 'inputClass' is not String.class.
	 */
	@ConvertMethod(
			input = @MetaFamily(
					subin = String.class
			),
			output = @MetaFamily(
					subin = Object.class,
					in = {boolean.class,
						  byte.class,
						  char.class,
						  double.class,
						  float.class,
						  int.class,
						  long.class,
						  short.class
					}))
	protected void string_object(ConvertArguments<String, Object> arguments) {
		try {
			if (DEBUGGING) {
				Objects.requireNonNull(arguments, "arguments");
			}

			arguments.output = JSON.global.cparse(new StringReader(arguments.input), arguments.outputClazz);
		} catch (IOException e) {
			throw new IOError(e);
		}
	}
}
