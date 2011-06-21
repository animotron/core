/*
 *  Copyright (C) 2011 The Animo Project
 *  http://animotron.org
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 3
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package org.animotron.manipulator;

import org.jetlang.channels.Channel;
import org.jetlang.channels.MemoryChannel;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Channels {
	
	public final Channel<Relationship> up = new MemoryChannel<Relationship>();
	public final Channel<Exception> upError = new MemoryChannel<Exception>();
	public final Channel<Void> upStop = new MemoryChannel<Void>();
	
	public final Channel<Relationship> down = new MemoryChannel<Relationship>();
	public final Channel<Exception> downError = new MemoryChannel<Exception>();
	public final Channel<Void> downStop = new MemoryChannel<Void>();
}

