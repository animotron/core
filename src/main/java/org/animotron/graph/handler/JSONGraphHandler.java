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
import org.animotron.manipulator.Profiler;
import org.animotron.statement.Statement;
import org.animotron.statement.ml.ATTRIBUTE;
import org.animotron.statement.ml.ELEMENT;
import org.animotron.statement.value.AbstractValue;
import org.animotron.statement.value.VALUE;
import org.codehaus.jackson.JsonGenerator;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;

/**
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class JSONGraphHandler implements GraphHandler {

	private JsonGenerator generator;

    private Controller controller = new Profiler();
    private boolean isNull = true;

    public JSONGraphHandler(JsonGenerator generator) {
		this.generator = generator;
	}
	
    public Controller getController() {
    	return controller;
    }
	
	@Override
	public void start(Statement statement, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {
	}

	@Override
	public void end(Statement statement, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {
	}

    @Override
    public void start(Statement statement, Statement parent, Object[] param, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        if (statement instanceof ATTRIBUTE) {
            generator.writeObjectField(param[0].toString(), AbstractValue.value(param[1]));
        } else if (statement instanceof ELEMENT) {
            generator.writeStartObject();
            generator.writeFieldName(param[0].toString());
            isNull = true;
        }
    }

    @Override
    public void end(Statement statement, Statement parent, Object[] param, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        if (statement instanceof ELEMENT) {
            if (isNull) {
                generator.writeNull();
            }
            generator.writeEndObject();
        }
    }

    @Override
    public void start(Statement statement, Statement parent, Object param, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        if (statement instanceof VALUE) {
            generator.writeObject(AbstractValue.value(param));
            isNull = false;
        }
    }

    @Override
    public void end(Statement statement, Statement parent, Object param, int level, boolean isOne, int pos, boolean isLast) throws IOException {
    }

    @Override
	public void startGraph() throws IOException {
        if (controller != null)
    		controller.start();
	}

	@Override
	public void endGraph() throws IOException {
        generator.close();
        if (controller != null)
            controller.end();
	}

	private boolean stepMade = false;

	public void stepMade() {
		stepMade = true;
	}

	public boolean isStepMade() {
		return stepMade;
	}

}