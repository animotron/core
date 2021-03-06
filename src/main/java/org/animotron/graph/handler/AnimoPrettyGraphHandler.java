/*
 *  Copyright (C) 2011-2013 The Animo Project
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

import org.animotron.statement.Prefix;
import org.animotron.statement.Statement;
import org.animotron.statement.Suffix;
import org.animotron.statement.link.LINK;
import org.animotron.statement.operator.*;
import org.animotron.statement.operator.VALUE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnimoPrettyGraphHandler extends AnimoGraphHandler {

    private List<Object[]> list;
    private Stack<Object[]> stack;
    private final static String INDENT = "    ";

    public AnimoPrettyGraphHandler(OutputStream stream) {
        super (stream);
    }

    public AnimoPrettyGraphHandler(StringBuilder builder) {
        super(builder);
    }

    public AnimoPrettyGraphHandler(Writer out) {
        super(out);
    }

    @Override
    public void startGraph() {
        super.startGraph();
        stack = new Stack<Object[]>();
        list = new LinkedList<Object[]>();
    }

	@Override
    @SuppressWarnings("unchecked")
    public void start(Statement statement, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        Object[] item = {statement, r, level, isOne, new LinkedList<Object[]>(), !isOne, pos, parent};
        if (!stack.empty()) {
            Object[]p = stack.peek();
            ((List<Object[]>) p[4]).add(item);
        }
        stack.push(item);
    }

    @Override
    @SuppressWarnings("unchecked")
    public void end(Statement statement, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        Object[] root = stack.pop();
        int size = ((List<Object[]>) root[4]).size();
        //if (statement instanceof Prefix) size--;
        if (!stack.empty()) {
            if (stack.peek()[0] instanceof Prefix) size--;
        } else {
            list.add(root);
        }
        root[5] = !(statement instanceof REF || statement instanceof AN && pos == 0) && ((Boolean) root[5] || size > 1) || (level == 1 && statement instanceof DEF);
    }

    @Override
    public void endGraph() throws IOException {
        for (Object[] o : list) {
            write(o, 0);
            write("\n");
        }
    	super.endGraph();
    }

    Statement ps = null;

    @SuppressWarnings("unchecked")
    private void write(Object[] o, int indent) throws IOException {
        Statement statement = (Statement) o[0];
        int level = (Integer) o[2];
        boolean isOne = (Boolean) o[3];
        if (statement instanceof AN) {
            if (level != 0) {
                if (!(ps instanceof LINK)) {
            		if ((Boolean) o[5] || ps instanceof DEF) {
            			indent++;
                		write("\n");
            			for (int i = 0; i < indent; i++) {
            				write(INDENT);
            			}
        			} else {
        			    write(" ");
        			}
                }
                if (!isOne) {
                    write("(");
                }
            }
            Relationship r = (Relationship) o[1];
            Node n= r.getEndNode();
            if (r.hasProperty(NONSTOP._.name())) {
                write(NONSTOP._.name());
            } else if (!n.hasRelationship(REF._, OUTGOING)) {
                write(statement.name());
            } else if (isOne && ((Integer) o[6]) == 0 && ps instanceof Operator) {
                write(statement.name());
                if (n.hasRelationship(OUTGOING)) {
                    write(" ");
                }
            }
        } else {
            if (level != 0) {
                if ((Boolean) o[5]) {
                    indent++;
                    write("\n");
                    for (int i = 0; i < indent; i++) {
                        write(INDENT);
                    }
                } else if (!(ps instanceof LINK || statement instanceof Suffix)) {
                    if (statement instanceof REF && ps instanceof REF && o[7] != null) {
                        write(", ");
                    } else {
                        if (!(statement instanceof REF && ps instanceof AN)) {
                        	if (statement instanceof Evaluable) {
	                            indent++;
	                            write("\n");
	                            for (int i = 0; i < indent; i++) {
	                                write(INDENT);
	                            }
                        	} else
                        		write(" ");
                        }
                    }
                }
                if (!(statement instanceof REF && o[7] != null || statement instanceof VALUE) && (!isOne || statement instanceof LINK)) {
                    write("(");
                }
            }
            write(statement, (Relationship) o[1]);
        }
        ps = statement;
        for (Object[] i : (List<Object[]>) o[4]) {
            write(i, indent);
        }
        if (level == 0) {
            write(".");
        } else if (!(statement instanceof REF && o[7] != null || statement instanceof VALUE) && (!isOne || statement instanceof LINK)) {
            write(")");
        }
    }

}