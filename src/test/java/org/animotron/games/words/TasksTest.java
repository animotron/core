package org.animotron.games.words;

import org.junit.Ignore;
import org.junit.Test;

import static org.junit.Assert.fail;

public class TasksTest {

	@Test
	@Ignore
	public void test() {
		String s = "If Mary has 5 sheep and the wolf kills 2. How many sheep Mary has?";
		s = "(Mary has) 5 sheep. kills 2. (How many) sheep (Mary has)?";
		fail(s);
	}

}
