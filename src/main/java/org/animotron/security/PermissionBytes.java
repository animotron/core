/*
 *  Copyright (C) 2012-2013 The Animo Project
 *  http://animotron.org
 *
 *  This file is part of Animi.
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

import java.nio.ByteBuffer;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class PermissionBytes implements Permission {
	
	private final static int RECORD_SIZE = 9;
	private final static byte RU_ACCESS = READ | UPDATE;// & DELETE;
	
	long object;
	ByteBuffer perms;
	
	public PermissionBytes() {
		perms = ByteBuffer.allocate(1);
		perms.put(RU_ACCESS);
	}

	public PermissionBytes(long object, byte[] perms) {
		this.object = object;
		this.perms = ByteBuffer.wrap(perms);
	}
	
	public void set(long subject, int mode) {
		perms.position(1);
		
		while (perms.hasRemaining()) {
			if (subject == perms.getLong()) {
				perms.put((byte) (mode & 15));
				return;
			}
			perms.get();
		}
		
		//adding new
		final int oldSize = perms.capacity();
		final byte[] bs = new byte[oldSize + RECORD_SIZE];
		System.arraycopy(perms.array(), 0, bs, 0, oldSize);
		
		perms = ByteBuffer.wrap(bs);
		
		perms.position(oldSize);
		
		perms.putLong(subject);
		perms.put((byte) (mode & 15));
	}
	
	public boolean validate(long subject, int mode) {
		if (perms.capacity() == 1) {
			perms.position(0);
			return (perms.get() & mode) == mode;
		}
		perms.position(1);
		
		while (perms.hasRemaining()) {
			if (subject == perms.getLong()) {
				return (perms.get() & mode) == mode;
			}
			perms.get();
		}
		return false;
	}
}
