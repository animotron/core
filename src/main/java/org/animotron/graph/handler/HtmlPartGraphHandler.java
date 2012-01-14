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

import org.animotron.statement.Statement;
import org.animotron.statement.ml.ATTRIBUTE;
import org.animotron.statement.ml.COMMENT;
import org.animotron.statement.ml.ELEMENT;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.io.OutputStream;

/**
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class HtmlPartGraphHandler extends AbstractTextGraphHandler {

    public HtmlPartGraphHandler(GraphHandler gh) {
        super(gh);
    }

    public HtmlPartGraphHandler(OutputStream out) {
        super(out);
    }

    public HtmlPartGraphHandler(StringBuilder out) {
        super(out);
    }

    @Override
	public void start(Statement statement, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {
	}

	@Override
	public void end(Statement statement, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {
	}

    private boolean starting = false;

    private void close() throws IOException {
        if (starting) {
            write(">");
            starting = false;
        }
    }
    @Override
    public void start(Statement statement, Statement parent, Object[] param, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        if (statement instanceof ATTRIBUTE) {
            write(" ");
            write(param[0].toString());
            if (param[1] != null) {
                write("=\"");
                write(param[1].toString().replace("\"", "&quot;"));
                write("\"");
            }
        } else if (statement instanceof ELEMENT) {
            close();
            write("<");
            write(param[0].toString());
            starting = true;
        }
    }

    @Override
    public void end(Statement statement, Statement parent, Object[] param, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        String name = param[0].toString();
        if (statement instanceof ELEMENT) {
            if (!starting || name.equals("script") || name.equals("style")) {
                write("</");
                write(name);
                write(">");
            } else {
                write(" />");
            }
            starting = false;
        }
    }

    @Override
    public void start(Statement statement, Statement parent, Object param, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        if (statement instanceof COMMENT){
            close();
            write("<!--");
            write(param.toString());
            write("-->");
        } else if (statement instanceof VALUE) {
            close();
            write(param.toString());
        }
    }

    @Override
    public void end(Statement statement, Statement parent, Object param, int level, boolean isOne, int pos, boolean isLast) throws IOException {
    }

    @Override
    public void startGraph() throws IOException {
	}

    @Override
	public void endGraph() throws IOException {
	}

}