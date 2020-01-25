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
import java.util.Objects;

/**
 * Helps to track string wrapping symbols.
 * <ul>
 *     Closeables
 *     <li>Braces: {@link open#BRACE &#123;} {@link close#BRACE &#125;}</li>
 *     <li>Brackets: {@link open#BRACKET [} {@link close#BRACKET ]}</li>
 *     <li>Chevrons: {@link open#CHEVRON ⟨} {@link close#CHEVRON ⟩}</li>
 *     <li>Corner-brackets: {@link open#CORNER_BRACKET ｢} {@link close#CORNER_BRACKET ｣}</li>
 *     <li>Double-brackets: {@link open#DOUBLE_BRACKET ⟦} {@link close#DOUBLE_BRACKET ⟧}</li>
 *     <li>Double-guillemet: {@link open#DOUBLE_GUILLEMET «} {@link close#DOUBLE_GUILLEMET »}</li>
 *     <li>Guillemet: {@link open#GUILLEMET <} {@link close#GUILLEMET >}</li>
 *     <li>Parenthese: {@link open#PARENTHESE (} {@link close#PARENTHESE )}</li>
 *     <li>Tortoise: {@link open#TORTOISE 〔} {@link close#TORTOISE 〕}</li>
 *     <li>Under-brackets: {@link open#UNDER_BRACKET ⸤} {@link close#UNDER_BRACKET ⸥}</li>
 * </ul>
 * <ul>
 *     Marks
 *     <li>Double-quote: {@link mark#DOUBLE_QUOTE "}</li>
 *     <li>Quote: {@link mark#QUOTE '}</li>
 *     <li>Star: {@link mark#STAR *}</li>
 *     <li>Tilde: {@link mark#TILDE ~}</li>
 *     <li>Underscore: {@link mark#UNDER_SCORE _}</li>
 * </ul>
 *
 * @author LSaferSE
 * @version 4 release (26-Jan-2020)
 * @see <a href="https://en.wikipedia.org/wiki/Bracket">wiki.org/wiki/Bracket</a>
 * @since 21-Nov-2019
 */
public class WrapTracker implements Appendable {
	/**
	 * Currently appended wraps. <br/>
	 *
	 * <pre>
	 *     for example:
	 *     text passed = {a = [ b, c, 'r', "t
	 *     wraps = {, [, "
	 *     mode = "
	 * </pre>
	 */
	final protected ArrayList<Character> wraps = new ArrayList<>(50);
	/**
	 * Last wrap appended.
	 *
	 * @see #wraps
	 */
	protected char mode = ' ';
	/**
	 * If we are expecting two characters.
	 */
	protected char previous = ' ';

	/**
	 * Update the wraps list with the given sequence as part of the track.
	 *
	 * @param sequence to be appended
	 * @return this
	 * @throws NullPointerException if the given sequence is null
	 */
	@Override
	public WrapTracker append(CharSequence sequence) {
		Objects.requireNonNull(sequence, "sequence");
		for (Character character : sequence.toString().toCharArray())
			this.append(character);

		return this;
	}
	@Override
	public WrapTracker append(CharSequence csq, int start, int end) throws IOException {
		Objects.requireNonNull(csq, "csq");

		int length = csq.length();

		if (start < 0 || end < 0 || length < start || length < end)
			throw new IndexOutOfBoundsException();

		for (int i = start; i < end; i++)
			this.append(csq.charAt(i));

		return this;
	}
	/**
	 * Update the wraps list with the given character as part of the track.
	 *
	 * @param character to be appended
	 * @return this
	 */
	@Override
	public WrapTracker append(char character) {
		char previous = this.previous;
		this.previous = character;

		switch (this.mode) {
			case comment._SLASH_SLASH:
				if (character == mark.NEW_LINE)
					this.unwrap();
				return this;
			//=======================
			case comment._SLASH_STAR:
				if (previous == mark.STAR && character == mark.SLASH)
					this.unwrap();
				return this;
			//=======================
			case mark.BACK_SLASH:
				this.unwrap();
				return this;
			//=======================
			case mark.SLASH:
				if (character == mark.SLASH) {
					this.unwrap();
					this.wrap(comment._SLASH_SLASH);
				} else if (character == mark.STAR) {
					this.unwrap();
					this.wrap(comment._SLASH_STAR);
				}
				return this;
			//=======================
		}
		switch (character) {
			case mark.DOUBLE_QUOTE:
			case mark.QUOTE:
			case mark.STAR:
			case mark.TILDE:
			case mark.UNDER_SCORE:
				if (character == this.mode)
					this.unwrap();
				else this.wrap(character);
				return this;
			//=======================
			case mark.BACK_SLASH:
			case mark.SLASH:
				//
			case open.BRACE:
			case open.BRACKET:
			case open.CHEVRON:
			case open.CORNER_BRACKET:
			case open.DOUBLE_BRACKET:
			case open.DOUBLE_GUILLEMET:
			case open.GUILLEMET:
			case open.PARENTHESE:
			case open.TORTOISE:
			case open.UNDER_BRACKET:
				this.wrap(character);
				return this;
			//=======================
			case close.BRACE:
			case close.BRACKET:
			case close.CHEVRON:
			case close.CORNER_BRACKET:
			case close.DOUBLE_BRACKET:
			case close.DOUBLE_GUILLEMET:
			case close.GUILLEMET:
			case close.PARENTHESE:
			case close.TORTOISE:
			case close.UNDER_BRACKET:
				if (character == this.getCloseFromOpen(this.mode))
					this.unwrap();
				return this;
			//=======================
		}

		return this;
	}

	/**
	 * Returns true if the current last wrap is comment.
	 *
	 * @return whether the current wrap is comment or not
	 */
	public boolean isComment() {
		return this.mode == comment._SLASH_SLASH || this.mode == comment._SLASH_STAR;
	}

	/**
	 * Returns true if any wrap detected currently.
	 *
	 * @return whether any wrap detected currently
	 */
	public boolean isWrapped() {
		return this.wraps.size() != 0;
	}

	/**
	 * Get the count of wraps appended.
	 *
	 * @return the size of wraps list
	 */
	public int length() {
		return this.wraps.size();
	}

	/**
	 * Get a close symbol from the given open symbol.
	 *
	 * @param ch to get a close symbol for
	 * @return a close symbol for the given open symbol
	 */
	protected char getCloseFromOpen(char ch) {
		switch (ch) {
			case open.BRACE:
				return close.BRACE;
			case open.BRACKET:
				return close.BRACKET;
			case open.CHEVRON:
				return close.CHEVRON;
			case open.CORNER_BRACKET:
				return close.CORNER_BRACKET;
			case open.DOUBLE_BRACKET:
				return close.DOUBLE_BRACKET;
			case open.DOUBLE_GUILLEMET:
				return close.DOUBLE_GUILLEMET;
			case open.GUILLEMET:
				return close.GUILLEMET;
			case open.PARENTHESE:
				return close.PARENTHESE;
			case open.TORTOISE:
				return close.TORTOISE;
			case open.UNDER_BRACKET:
				return close.UNDER_BRACKET;
			default:
				throw new IllegalArgumentException(String.valueOf(ch));
		}
	}

	/**
	 * Get an open symbol from the given close symbol.
	 *
	 * @param ch to get an open symbol for
	 * @return an open symbol for the given close symbol
	 */
	protected char getOpenFromClose(char ch) {
		switch (ch) {
			case close.BRACE:
				return open.BRACE;
			case close.BRACKET:
				return open.BRACKET;
			case close.CHEVRON:
				return open.CHEVRON;
			case close.CORNER_BRACKET:
				return open.CORNER_BRACKET;
			case close.DOUBLE_BRACKET:
				return open.DOUBLE_BRACKET;
			case close.DOUBLE_GUILLEMET:
				return open.DOUBLE_GUILLEMET;
			case close.GUILLEMET:
				return open.GUILLEMET;
			case close.PARENTHESE:
				return open.PARENTHESE;
			case close.TORTOISE:
				return open.TORTOISE;
			case close.UNDER_BRACKET:
				return open.UNDER_BRACKET;
			default:
				throw new IllegalArgumentException(String.valueOf(ch));
		}
	}

	/**
	 * Remove the last wrap (aka. mode) from wraps list.
	 */
	protected void unwrap() {
		int lastIndex = this.wraps.size() - 1;
		if (lastIndex != -1) {
			this.wraps.remove(lastIndex);
			this.mode = lastIndex == 0 ? ' ' : this.wraps.get(lastIndex - 1);
		}
	}

	/**
	 * Add the given wrap to the wraps list.
	 *
	 * @param wrap to be added
	 */
	protected void wrap(char wrap) {
		this.wraps.add(this.mode = wrap);
	}

	/**
	 * Close symbols used by this class.
	 */
	final public static class close {
		/**
		 * End braces.
		 */
		final public static char BRACE = '}';
		/**
		 * End bracket.
		 */
		final public static char BRACKET = ']';
		/**
		 * End chevron.
		 */
		final public static char CHEVRON = 10217;//'⟩'
		/**
		 * End corner brackets.
		 */
		final public static char CORNER_BRACKET = 65379;//'｣'
		/**
		 * End double brackets.
		 */
		final public static char DOUBLE_BRACKET = 10215;//'⟧'
		/**
		 * End double guillemets.
		 */
		final public static char DOUBLE_GUILLEMET = 187;//'»'
		/**
		 * End guillemets.
		 */
		final public static char GUILLEMET = '>';
		/**
		 * End parentheses.
		 */
		final public static char PARENTHESE = ')';
		/**
		 * End tortoise.
		 */
		final public static char TORTOISE = 12309;//'〕'
		/**
		 * End under brackets.
		 */
		final public static char UNDER_BRACKET = 11813;//'⸥'

		/**
		 * No instance for you!.
		 */
		private close() {
			throw new AssertionError("No instance for you");
		}
	}

	/**
	 * Symbols for escaping.
	 */
	final public static class comment {
		/**
		 * Represents "\\".
		 */
		final public static char _SLASH_SLASH = '0';
		/**
		 * Represents "\*".
		 */
		final public static char _SLASH_STAR = '1';

		/**
		 * No instance for you!.
		 */
		private comment() {
			throw new AssertionError("No instance for you");
		}
	}

	/**
	 * Symbols for open and close.
	 */
	final public static class mark {
		/**
		 * Init back slash.
		 *
		 * @apiNote escapes the next character.
		 */
		final public static char BACK_SLASH = '\\';
		/**
		 * Init or end double quotation marks.
		 */
		final public static char DOUBLE_QUOTE = '"';
		/**
		 * Init new line.
		 */
		final public static char NEW_LINE = '\n';
		/**
		 * Init or end quotation marks.
		 */
		final public static char QUOTE = '\'';
		/**
		 * Init a slash.
		 *
		 * @apiNote escapes until the next '*<code>/</code>' if the next character is '*'. Or until the next '\n' if the next character is '\'.
		 */
		final public static char SLASH = '/';
		/**
		 * Init or end star marks.
		 */
		final public static char STAR = '*';
		/**
		 * Init or end tilde.
		 */
		final public static char TILDE = '~';
		/**
		 * Init or end under score.
		 */
		final public static char UNDER_SCORE = '_';

		/**
		 * No instance for you!.
		 */
		private mark() {
			throw new AssertionError("No instance for you");
		}
	}

	/**
	 * Open symbols used by this class.
	 */
	final public static class open {
		/**
		 * Init braces.
		 */
		final public static char BRACE = '{';
		/**
		 * Init bracket.
		 */
		final public static char BRACKET = '[';
		/**
		 * Init chevron.
		 */
		final public static char CHEVRON = 10216;//'⟨'
		/**
		 * Init corner brackets.
		 */
		final public static char CORNER_BRACKET = 65378;//'｢'
		/**
		 * Init double brackets.
		 */
		final public static char DOUBLE_BRACKET = 10214;//'⟦'
		/**
		 * Init double guillemets.
		 */
		final public static char DOUBLE_GUILLEMET = 171;//'«'
		/**
		 * Init guillemets.
		 */
		final public static char GUILLEMET = '<';
		/**
		 * Init parentheses.
		 */
		final public static char PARENTHESE = '(';
		/**
		 * Init tortoise.
		 */
		final public static char TORTOISE = 12308;//'〔'
		/**
		 * Init under brackets.
		 */
		final public static char UNDER_BRACKET = 11812;//'⸤'

		/**
		 * No instance for you!.
		 */
		private open() {
			throw new AssertionError("No instance for you");
		}
	}
}
