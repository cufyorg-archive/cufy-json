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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Map;
import java.util.Objects;

/**
 * Helps to track syntax positioning on a stream.
 *
 * @author LSaferSE
 * @version 5 release (02-Feb-2020)
 * @since 21-Nov-2019
 */
public class SyntaxTracker implements Appendable {
	/**
	 * The map for literal syntax. Such syntax shouldn't contain any meaningful syntax inside it.
	 *
	 * @implSpec should be used as a final
	 */
	final protected Map<String, String> LITERAL;
	/**
	 * The map for nestable syntax. Such syntax can have a nested syntax in it.
	 *
	 * @apiNote no change after initialize
	 */
	final protected Map<String, String> NESTABLE;

	/**
	 * An array for the wraps appended to this.
	 */
	final protected ArrayList<String> wraps = new ArrayList<>();
	/**
	 * If currently receiving literal (non-nestable) text.
	 */
	protected boolean literal = false;
	/**
	 * value expected string to unwrap the last wrap.
	 */
	protected String unwrap = "";
	/**
	 * Value appended before that may match any wrapping string.
	 */
	protected String wrap = "";

	/**
	 * Initialize a new syntax tracker.
	 *
	 * @param nestable the syntax that can have a syntax inside it
	 * @param literal  the syntax that can not have a syntax inside it
	 */
	public SyntaxTracker(Map<String, String> nestable, Map<String, String> literal) {
		this.NESTABLE = nestable;
		this.LITERAL = literal;
	}

	@Override
	public Appendable append(CharSequence csq) throws IOException {
		Objects.requireNonNull(csq, "csq");

		int length = csq.length();

		for (int i = 0; i < length; i++)
			this.append(csq.charAt(i));

		return this;
	}

	@Override
	public Appendable append(CharSequence csq, int start, int end) throws IOException {
		Objects.requireNonNull(csq, "csq");

		int length = csq.length();

		if (start < 0 || end < 0 || length < start || length < end)
			throw new IndexOutOfBoundsException();

		for (int i = start; i < end; i++)
			this.append(csq.charAt(i));

		return this;
	}

	@Override
	public Appendable append(char c) throws IOException {
		if (!this.unwrap(c) && !this.literal)
			this.wrap(c);
		return this;
	}

	/**
	 * Get the unwrapping string that this tracker is currently waiting for.
	 *
	 * @return the unwrapping string
	 */
	public String getUnwrap() {
		return this.wraps.size() == 0 ? "" : this.wraps.get(this.wraps.size() - 1);
	}

	/**
	 * Return if the last wrap is a literal syntax or not.
	 *
	 * @return if the last wrap is a literal syntax or not
	 */
	public boolean isLiteral() {
		return this.literal;
	}

	/**
	 * Get the size of wrapping applied to this.
	 *
	 * @return the size of the wraps list
	 */
	public int length() {
		return this.wraps.size();
	}

	/**
	 * Apply the unwrapping algorithm to the given character.
	 *
	 * @param c the character for the current position
	 * @return true if {@link #unwrap0)} got invoked
	 */
	protected boolean unwrap(char c) {
		if (this.unwrap.length() == 0) {
			return this.unwrap0();
		} else {
			if (this.unwrap.charAt(0) == c) {
				if (this.unwrap.length() == 1) {
					return this.unwrap0();
				} else {
					this.unwrap = this.unwrap.substring(1);
				}
			} else {
				this.resetUnwrap();
			}
		}

		return false;
	}

	/**
	 * Apply the wrapping algorithm to the given character.
	 *
	 * @param c the character for the current position
	 * @return true if {@link #wrap0(String)}
	 */
	protected boolean wrap(char c) {
		if (!this.literal) {
			this.wrap += c;

			boolean startsWith = false;
			for (Map.Entry<String, String> entry : this.NESTABLE.entrySet()) {
				String key = entry.getKey();

				if (key.startsWith(this.wrap)) {
					startsWith = true;

					if (key.length() == this.wrap.length()) {
						return this.wrap0(entry.getValue());
					}
				}
			}
			for (Map.Entry<String, String> entry : this.LITERAL.entrySet()) {
				String key = entry.getKey();

				if (key.startsWith(this.wrap)) {
					startsWith = true;

					if (key.length() == this.wrap.length()) {
						this.literal = true;
						return this.wrap0(entry.getValue());
					}
				}
			}

			if (!startsWith)
				this.resetWrap();
		}

		return false;
	}

	/**
	 * Reset the mode that blocks any further wrappings.
	 */
	private void resetLiteral() {
		this.literal = false;
	}

	/**
	 * Reset the mode that holds the unwrapping condition.
	 */
	private void resetUnwrap() {
		this.unwrap = this.wraps.size() == 0 ? "" : this.wraps.get(this.wraps.size() - 1);
	}

	/**
	 * Reset the mode that holds the wrapping condition.
	 */
	private void resetWrap() {
		this.wrap = "";
	}

	/**
	 * Remove the last wrap (if there is). Then reset the wrapping and unwrapping and literal modes.
	 *
	 * @return if this method has actually unwrapped something
	 */
	private boolean unwrap0() {
		if (this.wraps.size() != 0) {
			this.wraps.remove(this.wraps.size() - 1);

			this.resetWrap();
			this.resetUnwrap();
			this.resetLiteral();
			return true;
		}

		return false;
	}

	/**
	 * Wrap And set the specified string as a unwrapping condition. Then reset the wrapping and unwrapping modes.
	 *
	 * @param unwrapMode the unwrapping string to unwrap
	 * @return true if this method has actually wrapping something
	 */
	private boolean wrap0(String unwrapMode) {
		this.wraps.add(unwrapMode);

		this.resetWrap();
		this.resetUnwrap();
		return true;
	}
}
