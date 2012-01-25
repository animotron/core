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
package org.animotron.graph.handler;

import org.animotron.manipulator.Controller;
import org.animotron.statement.Statement;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;

/**
 * Parse animo structure and generate events for output formater.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public interface GraphHandler {
	
    public abstract void start(Statement statement, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException;

    public abstract void end(Statement statement, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException;

    public abstract void start(Statement statement, Statement parent, Object[] param, int level, boolean isOne, int pos, boolean isLast) throws IOException;

    public abstract void end(Statement statement, Statement parent, Object[] param, int level, boolean isOne, int pos, boolean isLast) throws IOException;

    public abstract void start(Statement statement, Statement parent, Object param, int level, boolean isOne, int pos, boolean isLast) throws IOException;

    public abstract void end(Statement statement, Statement parent, Object param, int level, boolean isOne, int pos, boolean isLast) throws IOException;

	public void startGraph() throws IOException;

	public void endGraph() throws IOException;
	
	public Controller getController(); 

	@Deprecated //XXX: find better way 
	public boolean isStepMade();
}