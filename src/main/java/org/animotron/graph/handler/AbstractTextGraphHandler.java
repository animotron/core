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

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class AbstractTextGraphHandler implements GraphHandler {

    private Handler out;
    
    protected Controller controller = new Profiler();

    public AbstractTextGraphHandler(GraphHandler gh) {
    	if (gh instanceof AbstractTextGraphHandler) {
			out = ((AbstractTextGraphHandler) gh).out;
			stepMade = true;
		} else
			throw new UnsupportedOperationException();
    }

    public AbstractTextGraphHandler(OutputStream stream) {
        out = new StreamHandler(stream);
    }

    public AbstractTextGraphHandler(StringBuilder builder) {
        out = new StringBuilderHandler(builder);
    }

    public AbstractTextGraphHandler(Writer writer) {
        out = new WriterHandler(writer);
    }

    public Controller getController() {
    	return controller;
    }

	private boolean stepMade = false;
	
	public boolean isStepMade() {
		return stepMade;
	}

    protected void write(String text) throws IOException {
        out.write(text);
    }

    private interface Handler {
        public void write(String string) throws IOException;
    }

    private class StreamHandler implements Handler {
        OutputStream out;
        public StreamHandler(OutputStream stream) {
            out = stream;
        }
        @Override
        public void write(String s) throws IOException {
            out.write(s.getBytes());
        }
    }

    private class StringBuilderHandler implements Handler {
        StringBuilder out;
        public StringBuilderHandler(StringBuilder builder) {
            out = builder;
        }
        @Override
        public void write(String s) {
            out.append(s);
        }
    }

    private class WriterHandler implements Handler {
        Writer out;
        public WriterHandler(Writer writer) {
            out = writer;
        }
        @Override
        public void write(String s) throws IOException {
            out.append(s);
        }
    }

}
