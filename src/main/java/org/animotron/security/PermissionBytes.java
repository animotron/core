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

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class PermissionBytes implements Permission {

	private final static int KEY_SIZE = 8;
	private final static int RECORD_SIZE = 9;
	private final static byte RU_ACCESS = READ | UPDATE;// & DELETE;

	final long object;
	byte[] perms;
	int size = 0;

	public PermissionBytes() {
		object = -1;
		perms = new byte[1];
		perms[0] = RU_ACCESS;
	}

	public PermissionBytes(long object, byte[] perms) {
		this.object = object;
		this.perms = perms;

		// XXX: check that size is right
		size = (perms.length - 1) / RECORD_SIZE;
	}
	
	public long bytes() {
		return perms.length;
	}

	public int size() {
		return size;
	}

	public void set(final long subject, final int mode) {

		if (size == 0) {
			add(0, subject, mode);
			return;
		}
		
		int pos = size / 2;
		int half = pos;
		
		while (true) {
	    	final int i = 1 + pos * RECORD_SIZE;
			final long comp = compare(pos, i, subject);
//			System.out.println("> "+comp+" pos="+pos+" ("+half+") "+key(pos));
			if (comp == 0) {
				//found
				perms[i + KEY_SIZE] = ((byte) (mode & 15));
				return;
			}
			if (half == 0) {
				if (comp > 0) {
					//shift right
					add(pos+1, subject, mode);
				} else if (comp < 0) {
					//shift left
					add(pos, subject, mode);
				}
				break;
			}
				
			if (comp > 0) {
				//shift right
				pos += half / 2 + 1;
			} else if (comp < 0) {
				//shift left
				pos -= half / 2 + 1;
			}
			half = half / 2;
		}
	}
	
	private void add(final int pos, final long subject, final int mode) {
//		System.out.println("add at pos="+pos);
		
    	final int i = 1 + pos * RECORD_SIZE;
		final int oldSize = perms.length;

		final byte[] bs = new byte[oldSize + RECORD_SIZE];

		System.arraycopy(perms, 0, bs, 0, i);
		System.arraycopy(perms, i, bs, i + RECORD_SIZE, oldSize - i);

		perms = bs;
		
		//adding new
		putLongB(i, subject);
		perms[i + KEY_SIZE] = (byte) (mode & 15);
		size++;
	}

	public boolean validate(final long subject, final int mode) {
		if (perms.length == 1) {
			return (perms[0] & mode) == mode;
		}

		int pos = size / 2;
		int half = pos;
		
		while (true) {
	    	final int i = 1 + pos * RECORD_SIZE;
			final long comp = compare(pos, i, subject);
//			System.out.println("< "+comp+" pos="+pos+" ("+half+") "+key(pos));
			if (comp == 0) {
				//found
				return (perms[i + KEY_SIZE] & mode) == mode;
			}
			if (half == 0)
				return false;
			
			if (comp > 0) {
				//shift right
				pos += half / 2 + 1;
			} else if (comp < 0) {
				//shift left
				pos -= half / 2 + 1;
			}
			half = half / 2;
		}
	}
	
	public long key(int pos) {
		if (pos < 0)
			return -1;

		if (pos >= size)
			return 1;

		int index = 1 + pos * RECORD_SIZE;
		
		return getLongB(index);
	}
	
	public byte val(int pos) {
		int index = 1 + pos * RECORD_SIZE + KEY_SIZE;
		return perms[index];
	}

	private int compare(final int pos, final int index, final long subject) {
		if (pos < 0)
			return +1;
		else if (pos >= size)
			return -1;
		
		try {
			return Long.compare(subject, getLongB(index));
		} catch (Exception e) {
			throw e;
		}
	}

    private void putLongB(final int i, final long x) {
    	perms[i    ] = long7(x);
    	perms[i + 1] = long6(x);
    	perms[i + 2] = long5(x);
    	perms[i + 3] = long4(x);
    	perms[i + 4] = long3(x);
    	perms[i + 5] = long2(x);
    	perms[i + 6] = long1(x);
    	perms[i + 7] = long0(x);
    }

	private long getLongB(final int i) {
        return 
    		makeLong(
        		perms[i    ],
        		perms[i + 1],
        		perms[i + 2],
        		perms[i + 3],
        		perms[i + 4],
        		perms[i + 5],
        		perms[i + 6],
        		perms[i + 7]
    		);
    }
    
	private static byte long7(final long x) { return (byte) (x >> 56); }
	private static byte long6(final long x) { return (byte) (x >> 48); }
	private static byte long5(final long x) { return (byte) (x >> 40); }
	private static byte long4(final long x) { return (byte) (x >> 32); }
	private static byte long3(final long x) { return (byte) (x >> 24); }
	private static byte long2(final long x) { return (byte) (x >> 16); }
	private static byte long1(final long x) { return (byte) (x >> 8 ); }
	private static byte long0(final long x) { return (byte) (x      ); }

	private static long makeLong(
			final byte b7,
			final byte b6,
			final byte b5,
			final byte b4,
			final byte b3,
			final byte b2,
			final byte b1,
			final byte b0) {
		return 
			(
				  (((long) b7       ) << 56) 
				| (((long) b6 & 0xff) << 48)
				| (((long) b5 & 0xff) << 40)
				| (((long) b4 & 0xff) << 32)
				| (((long) b3 & 0xff) << 24)
				| (((long) b2 & 0xff) << 16)
				| (((long) b1 & 0xff) << 8 )
				| (((long) b0 & 0xff)      )
			);
	}
}
