/*
 *  Copyright (C) 2011-2012 The Animo Project
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
package org.animotron.statement.operator;

import org.animotron.exception.AnimoException;
import org.animotron.statement.Prefix;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

/**
 * Operation 'AN'. Direct reference to 'the' instance.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class NONSTOP extends AN implements Prefix {

	public static final NONSTOP _ = new NONSTOP();

    public static boolean is(Relationship r) {
    	return !r.hasProperty(_.name());
    }

//	private static boolean debug = false;

	private NONSTOP() { super("^"); }

    @Override
	public Relationship build(Node parent, Object reference, byte[] hash, boolean ready, boolean ignoreNotFound) throws AnimoException {
        Relationship r = AN._.build(parent, reference, hash, ready, ignoreNotFound);
        r.setProperty(name(), true);
		return r;
	}
}