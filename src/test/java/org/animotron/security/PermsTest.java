/*
 *  Copyright (C) 2011-2013 The Animo Project
 *  http://animotron.org
 *
 *  This file is part of Animotron.
 *
 *  Animotron is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as
 *  published by the Free Software Foundation, either version 3 of
 *  the License, or (at your option) any later version.
 *
 *  Animotron is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *
 *  You should have received a copy of
 *  the GNU Affero General Public License along with Animotron.
 *  If not, see <http://www.gnu.org/licenses/>.
 */
package org.animotron.security;

import static org.junit.Assert.*;

import java.util.Random;
import java.util.TreeSet;

import org.animotron.security.Permission;
import org.animotron.security.PermissionBytes;
import org.junit.Test;

import static org.animotron.security.Permission.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class PermsTest {

	private static Random rnd = new Random();

	@Test
	public void test() {
		
		Permission perms = new PermissionBytes();
		assertTrue( perms.validate(rnd.nextLong(), READ) );
		assertTrue( perms.validate(rnd.nextLong(), UPDATE) );
		assertFalse( perms.validate(rnd.nextLong(), DELETE) );
		
		final long subject1 = rnd.nextLong();
		perms.set(subject1, READ);

		assertTrue( perms.validate(subject1, READ) );
		assertFalse( perms.validate(subject1, UPDATE) );
		assertFalse( perms.validate(subject1, DELETE) );

		final long subject2 = rnd.nextLong();
		perms.set(subject2, UPDATE);

		assertFalse( perms.validate(subject2, READ) );
		assertTrue( perms.validate(subject2, UPDATE) );
		assertFalse( perms.validate(subject2, DELETE) );

		perms.set(subject1, Permission.READ | UPDATE | DELETE);

		assertTrue( perms.validate(subject1, READ) );
		assertTrue( perms.validate(subject1, UPDATE) );
		assertTrue( perms.validate(subject1, DELETE) );
	}
	
	private TreeSet<Long> ids = new TreeSet<Long>();

	@Test
	public void testMany() {
		Permission perms = new PermissionBytes();
		
		for (int i = 0; i < 10000; i++) {
//			System.out.println("----- "+i);
			add_validate(perms);
		}
		
		System.out.println(perms.size());
		System.out.println(perms.bytes());
		
		long ts = System.nanoTime();
		
		final int pattern = rnd.nextInt(MAX);
		final long subject = rnd.nextLong();
		
		perms.validate(subject, pattern);
		
		System.out.println(System.nanoTime() - ts);
	}
	private static int MAX = READ | UPDATE | DELETE;
	
	private void add_validate(Permission perms) {
		final int pattern = rnd.nextInt(MAX);
		
		final long subject = rnd.nextLong();
		
//		System.out.println("adding "+subject+" "+pattern);
//		System.out.println();
		
		ids.add(subject);
		
		perms.set(subject, pattern);
		
		checkorder(perms);
		
		validate(perms, subject, pattern);
	}
	
	private void validate(final Permission perms, final long subject, final int pattern) {
		testPattern(perms, subject, READ, pattern);
		testPattern(perms, subject, UPDATE, pattern);
		testPattern(perms, subject, DELETE, pattern);
	}

	private void testPattern(Permission perms, long subject, int mode, int pattern) {
//		System.out.println("pattern = " + pattern + "; mode = "+mode+"; "+((pattern & mode) == mode)+" vs "+perms.validate(subject, mode));
//		if (perms.validate(subject, mode) != ((pattern & mode) == mode)) {
//			System.out.println("!!! "+perms.validate(subject, mode));
//		}
//		System.out.println("---");
		assertTrue(
			perms.validate(subject, mode) == ((pattern & mode) == mode)
		);
//		System.out.println("---");
	}
	
	private void checkorder(Permission perms) {
		int i = 0;
		for (Long l : ids) {
//			System.out.print(i+" "+l+" "+perms.val(i)+" ");
			if (!l.equals(perms.key(i))) {
				System.out.println(i+" "+l+" "+perms.val(i)+" * [ "+perms.key(i)+"] ");
//			} else {
//				System.out.println();
			}
			i++;
		}
	}
}
