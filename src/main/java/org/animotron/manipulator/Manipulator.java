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

import java.io.IOException;

import org.animotron.Catcher;
import org.animotron.io.PipedInputObjectStream;
import org.animotron.io.PipedOutputObjectStream;
import org.animotron.marker.Marker;
import org.animotron.walker.Walker;
import org.neo4j.graphdb.PropertyContainer;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public interface Manipulator {

	//for debug needs
	public boolean isPiped();
	
	public Walker markWalk(PropertyContainer op, PipedOutputObjectStream out, Catcher catcher);
	public Walker walk(PropertyContainer op, PipedOutputObjectStream out, Catcher catcher);
	
	public Walker walk(PropertyContainer op, PipedOutputObjectStream out, Marker marker, Catcher catcher);

	public PipedInputObjectStream markExecute(PropertyContainer op, Catcher catcher) throws IOException;
	public PipedInputObjectStream execute(PropertyContainer op, Catcher catcher) throws IOException;

	public void markExecute(PropertyContainer op, PipedOutputObjectStream out, Catcher catcher);
	public void execute(PropertyContainer op, PipedOutputObjectStream out, Catcher catcher);

	public void shutdown();
	
}