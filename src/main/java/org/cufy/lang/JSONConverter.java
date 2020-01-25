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

import cufy.lang.Global;
import cufy.lang.Type;
import org.cufy.text.JSON;

import java.util.Objects;

/**
 * A converter that supports everything in the {@link BaseConverter}. This converter supports the conversion between {@link String} and other {@link
 * Object}s. Using {@link org.cufy.text.JSON}.
 *
 * @author LSaferSE
 * @version 1 release (25-Jan-2020)
 * @since 25-Jan-2020
 */
public class JSONConverter extends BaseConverter implements Global {
	/**
	 * The global instance to avoid unnecessary instancing.
	 */
	final public static JSONConverter global = new JSONConverter();

	/**
	 * Format the given object to string using {@link JSON}.
	 *
	 * @param object to be formatted
	 * @return a formatted string for the given object
	 */
	@Override
	@ConvertMethod(
			in = @Type(
					subin = Object.class,
					in = {boolean.class,
						  byte.class,
						  char.class,
						  double.class,
						  float.class,
						  int.class,
						  long.class,
						  short.class
					}),
			out = @Type(
					String.class
			))
	protected String objectToString(Object object) {
		return JSON.global.format(object);
	}

	/**
	 * Parse the given string to the given class.
	 *
	 * @return a parsed object from the given string
	 * @throws NullPointerException if any of the given parameters is null
	 */
	@Override
	@ConvertMethod(
			in = @Type(
					in = String.class
			),
			out = @Type(
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
	protected Object stringToObject(String string, Class<?> productClass) {
		if (DEBUGGING) {
			Objects.requireNonNull(string, "string");
			Objects.requireNonNull(productClass, "productClass");
		}

		return this.convert(JSON.global.parse(string), productClass);
	}
}
