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
import org.animotron.statement.Suffix;
import org.animotron.statement.link.LINK;
import org.animotron.statement.ml.QNAME;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.STOPPER;
import org.animotron.statement.operator.Operator;
import org.animotron.statement.operator.REF;
import org.animotron.statement.value.AbstractValue;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;

import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnimoGraphHandler extends AbstractTextGraphHandler {
	
    public AnimoGraphHandler(GraphHandler gh) {
    	super(gh);
    }

	public AnimoGraphHandler(OutputStream stream) {
        super(stream);
    }

    public AnimoGraphHandler(StringBuilder builder) {
        super(builder);
    }

    public AnimoGraphHandler(Writer out) {
        super(out);
    }

    protected void write(Statement statement, Relationship r) throws IOException {
        write(statement, statement.reference(r));
    }

    protected void write(Statement statement, Object reference) throws IOException {
        if (statement instanceof QNAME) {
        	if (reference != null)
        		write(reference.toString());
        	
        } else if (statement instanceof AbstractValue) {
            if (!(statement instanceof VALUE)) {
                write(statement.name());
                if (reference != null) {
                    write(" ");
                }
            }
        	if (reference != null)
	            if (reference instanceof String) {
	                write("\"");
	                write(reference.toString().replaceAll("\"", "\\\\\""));
	                write("\"");
	            } else {
	                write(reference.toString());
	            }
        } else if (statement instanceof REF) {
            write(reference.toString());
            if (ps instanceof Suffix) {
                write(ps.name());
            }
        } else if (!(statement instanceof LINK || statement instanceof VALUE || statement instanceof Suffix)) {
            write(statement.name());
            if (reference != null) {
                write(" ");
                write(reference.toString());
            }
        }
    }

    private Statement ps = null;
    @Override
    public void start(Statement statement, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        if (statement instanceof AN) {
            if (level == 0) {
                if (dot) {
                    write(" ");
                }
            } else {
                if (!(ps instanceof LINK)) {
                    write(" ");
                }
                if (!isOne) {
                    write("(");
                }
            }
            Node n= r.getEndNode();
            if (r.hasProperty(STOPPER._.name())) {
                write(STOPPER._.name());
            } else if (!n.hasRelationship(REF._, OUTGOING)) {
                write(statement.name());
            } else if (pos == 0 && ps instanceof Operator) {
                write(statement.name());
                if (n.hasRelationship(OUTGOING)) {
                    write(" ");
                }
            }
            ps = statement;
        } else {
            start(statement, parent, statement.reference(r), level, isOne, 0, false);
        }
    }

    @Override
    public void end(Statement statement, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        end(statement, parent, level, isOne);
    }

    @Override
    public void start(Statement statement, Statement parent, Object[] param, int level, boolean isOne, int pos, boolean isLast) throws IOException {
    }

    @Override
    public void end(Statement statement, Statement parent, Object[] param, int level, boolean isOne, int pos, boolean isLast) throws IOException {
    }

    private boolean dot = false;
    @Override
    public void start(Statement statement, Statement parent, Object param, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        if (level == 0) {
            if (dot) {
                write(" ");
            }
        } else if (!(statement instanceof QNAME)) {
            if (!(ps instanceof LINK || statement instanceof Suffix)) {
                if (statement instanceof REF && ps instanceof REF && parent != null) {
                    write(", ");
                } else {
                    if (!(statement instanceof REF && ps instanceof AN)) {
                        write(" ");
                    }
                }
            }
            if (!(statement instanceof REF && parent != null || statement instanceof VALUE) && (!isOne || statement instanceof LINK)) {
                write("(");
            }
        }
        write(statement, param);
        ps = statement;
    }

    @Override
    public void end(Statement statement, Statement parent, Object param, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        end(statement, null, level, isOne);
    }

    private void end(Statement statement, Statement parent, int level, boolean isOne) throws IOException {
        if (level==0) {
            write(".");
            dot = true;
        } else if (!(statement instanceof REF && parent != null || statement instanceof QNAME || statement instanceof VALUE) && (!isOne || statement instanceof LINK)) {
            write(")");
        }
    }

    @Override
    public void startGraph() {
        if (controller != null)
    	    controller.start();
    }

    @Override
    public void endGraph() throws IOException {
        if (controller != null)
        	controller.end();
    }

}