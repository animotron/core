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
package org.animotron.perms;

import static org.junit.Assert.*;

import java.util.Random;

import org.animotron.security.Permission;
import org.animotron.security.PermissionBytes;
import org.junit.Test;

import static org.animotron.security.Permission.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class PermsTest {

	@Test
	public void test() {
		
		Random rnd = new Random();
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

}
