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
package cufy.text;

import cufy.concurrent.Forever;
import cufy.concurrent.Loop;
import org.junit.Assert;
import org.junit.Test;

import java.io.*;
import java.net.URL;
import java.util.*;

@SuppressWarnings("JavaDoc")
public class JSONTest {
	@Test
	public void commentTest() {
		String s = "{\n" +
				   "\t\"list\":[\n" +
				   "\t\t/*abcddffqefa*/\"Success\"//wadefcwacesfeqaescd\n" +
				   "\t]\n" +
				   "}";

		Map obj = (Map) JSON.parse(s);
		List arr = (List) obj.get("list");

		Assert.assertEquals("incorrect object size", 1, obj.size());
		Assert.assertEquals("incorrect array size", 1, arr.size());
		Assert.assertEquals("incorrect value", "Success", arr.get(0));
	}

	@Test
	public void comment_array_object_nested() {
		String source = "[3, 5, {9/*{],23myComment*/=//myMultiLineComment\n\"abc\"}]";

		//List<Object> list = JSONConverter.global.convert(source, ArrayList.class);
		List<Object> list = (List<Object>) JSON.parse(source);

		Assert.assertEquals("Wrong size", 3, list.size());
		Assert.assertEquals("Wrong 1st element", 3, list.get(0));
		Assert.assertEquals("Wrong 2nd element", 5, list.get(1));

		Map<Object, Object> map = (Map<Object, Object>) list.get(2);

		Assert.assertEquals("Wrong element in the key 9", "abc", map.get(9));
	}

	@Test
	public void directParse() throws IOException {
		String source = "{\"a\"=3, \"b\"=4}";

		Map map = new HashMap();

		JSON.global.parse(new StringReader(source), map);

		Assert.assertEquals("Unexpected length", 2, map.size());
		Assert.assertEquals("the key 'a' stores unexpected value", 3, map.get("a"));
		Assert.assertEquals("the key 'b' stores unexpected value", 4, map.get("b"));
	}

	@Test
	public void empty() {
		Collection collection = (Collection) JSON.parse("[]");
		Map map = (Map) JSON.parse("{}");

		Assert.assertTrue("expected empty collection!", collection.isEmpty());
		Assert.assertTrue("expected empty map!", map.isEmpty());

		collection = (Collection) JSON.parse("[0,]");
		map = (Map) JSON.parse("{0:0,}");

		Assert.assertEquals("expected singleton collection!", 1, collection.size());
		Assert.assertEquals("expected singleton map!", 1, map.size());
		Assert.assertEquals("wrong member value", 0, collection.iterator().next());
		Assert.assertEquals("wrong member value", 0, map.get(0));

		try {
			JSON.parse("{,}");
			Assert.fail("expected \"No equation symbol\" exception!");
		} catch (ParseException ignored) {
		}
		try {
			JSON.parse("{:}");
			Assert.fail("expected \"can't parse Empty.class\" exception!");
		} catch (ParseException ignored) {
		}
		try {
			JSON.parse("[,]");
			Assert.fail("expected \"Elements can't be empty\" exception!");
		} catch (ParseException ignored) {
		}
	}

	@Test
	public void format_object_array_nested() {
		Map<Object, Object> base = new HashMap<>(3);
		Map<Object, Object> map = new HashMap<>(3);
		base.put("map", map);
		map.put("number", Arrays.asList(9L, 3L, 5L));

		String expected = "{\n" +
						  "\t\"map\":{\n" +
						  "\t\t\"number\":[\n" +
						  "\t\t\t9L,\n" +
						  "\t\t\t3L,\n" +
						  "\t\t\t5L\n" +
						  "\t\t]\n" +
						  "\t}\n" +
						  "}";
		String actual = JSON.format(base);
		Assert.assertEquals("Wrong format", expected, actual);
	}

	@Test
	public void jsonplaceholder_typicode_comments() throws IOException, InterruptedException {
		InputStream c;

		try {
			c = new URL("https://jsonplaceholder.typicode.com/comments").openStream();
		} catch (IOException e) {
			//no internet!
			return;
		}

		Loop<?, ?> display = new Forever().thread();

		Thread.currentThread().setName("Parser");
		display.getThread(t -> t.setName("Display"));

		JSON.global.cparse(new BufferedReader(new InputStreamReader(c)), new ArrayList() {
			@Override
			public boolean add(Object element) {
				super.add(element);
				display.post(l -> {
//					System.out.println("\n----------------");
//					System.out.println("From thread: " + Thread.currentThread());
//					System.out.println(JSON.format(element));
//					System.out.println("----------------\n");
					JSON.format(element);
					return false;
				});

				return true;
			}
		});

		display.notify(Loop.BREAK).join(l -> {
			throw new InternalError("took the Display Loop more than 10 seconds to stop");
		}, 10000);
	}

	@Test
	public void jsonplaceholder_typicode_todos() throws IOException, InterruptedException {
		InputStream c;

		try {
			c = new URL("https://jsonplaceholder.typicode.com/todos").openStream();
		} catch (IOException e) {
			//no internet!
			return;
		}

		Loop<?, ?> display = new Forever().thread();

		Thread.currentThread().setName("Parser");
		display.getThread(t -> t.setName("Display"));

		JSON.global.cparse(new BufferedReader(new InputStreamReader(c)), new ArrayList() {
			@Override
			public boolean add(Object element) {
				super.add(element);
				display.post(l -> {
//					System.out.println("\n----------------");
//					System.out.println("From thread: " + Thread.currentThread());
//					System.out.println(JSON.format(element));
//					System.out.println("----------------\n");
					JSON.format(element);
					return false;
				});

				return true;
			}
		});

		display.notify(Loop.BREAK).join(l -> {
			throw new InternalError("took the Display Loop more than 10 seconds to stop");
		}, 10000);
	}

	@Test
	public void parse_array_keep_instance() throws IOException {
		ArrayList list = new ArrayList();
		ArrayList nestedOver = new ArrayList();
		String nestedNon = "abc";

		list.add(nestedOver);
		list.add(nestedNon);

		nestedOver.add(nestedNon);

		String source = "[[\"def\", [0]], \"def\", [0]]";

		JSON.global.parse(new StringReader(source), list);

		Assert.assertEquals("Wrong base size", 3, list.size());

		List nestedOverAfter = (List) list.get(0);

		Assert.assertSame("Instance not overwritten", nestedOver, nestedOverAfter);
		Assert.assertEquals("Wrong member value", "def", list.get(1));
		Assert.assertEquals("Wrong member value", new ArrayList<>(Collections.singletonList(0)), list.get(2));

		Assert.assertEquals("Wrong over size", 2, nestedOverAfter.size());

		Assert.assertEquals("Wrong member value", "def", nestedOverAfter.get(0));
		Assert.assertEquals("Wrong member value", new ArrayList<>(Collections.singletonList(0)), nestedOverAfter.get(1));

		source = "[[\"jhi\"]]";

		JSON.global.parse(new StringReader(source), list);

		Assert.assertEquals("Wrong base size", 1, list.size());

		nestedOverAfter = (List) list.get(0);

		Assert.assertSame("Instance not overwritten", nestedOver, nestedOverAfter);

		Assert.assertEquals("Wrong over size", 1, nestedOverAfter.size());

		Assert.assertEquals("Wrong member value", "jhi", nestedOverAfter.get(0));
	}

	@Test
	public void parse_object_array_nested() {
		Map<String, Map<String, List<Number>>> val = (Map<String, Map<String, List<Number>>>) JSON.parse("{\"map\":{\"number\":[9, 3, 5]}}");
		Map<String, List<Number>> map = val.get("map");
		List<Number> number = map.get("number");
		Assert.assertEquals("first number not detected", 9L, number.get(0).longValue());
		Assert.assertEquals("second number not detected", 3L, number.get(1).longValue());
		Assert.assertEquals("third number not detected", 5L, number.get(2).longValue());
	}
}
