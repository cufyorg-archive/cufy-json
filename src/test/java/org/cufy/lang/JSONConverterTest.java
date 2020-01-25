package org.cufy.lang;

import org.junit.Assert;
import org.junit.Test;

import java.util.*;

@SuppressWarnings("JavaDoc")
public class JSONConverterTest {
	@Test
	public void convert() {
		Map<Object, Object> map = new HashMap<>();
		map.put("a", 5L);
		map.put("b", Arrays.asList(0L, 1L, 2L));

		int length = map.size();
		Set<Object> values = new HashSet<>(map.values());

		String mapAsSource = JSONConverter.global.convert(map, String.class);
		HashSet<Object> set = JSONConverter.global.convert(mapAsSource, HashSet.class);
		String setAsSource = JSONConverter.global.convert(set, String.class);
		Object[] array = JSONConverter.global.convert(setAsSource, Object[].class);

		Assert.assertEquals("map->string->set Haven't included all items", length, set.size());
		Assert.assertEquals("map->string->set Items don't match", values, set);
		Assert.assertEquals("set->string->array Haven't included all items", length, array.length);
		Assert.assertArrayEquals("Haven't translate all items", values.toArray(), array);
	}
}
