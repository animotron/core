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
        if (statement instanceof ELEMENT) {
            if (starting) {
                write("/>");
            } else {
                write("</");
                write(param[0].toString());
                write(">");
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