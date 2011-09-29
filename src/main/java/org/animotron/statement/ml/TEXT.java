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
package org.animotron.statement.ml;

import org.animotron.statement.AbstractLink;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.util.StringTokenizer;

import static org.animotron.Properties.VALUE;
import static org.animotron.graph.AnimoGraph.createCache;
import static org.animotron.graph.AnimoGraph.createNode;
import static org.animotron.graph.AnimoGraph.getCache;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class TEXT extends AbstractLink implements MLOperator {
	
	public static final TEXT _ = new TEXT();
	
	private TEXT() { super("text"); }

    protected TEXT(String name) { super(name); }

    @Override
    public Relationship build(Node parent, String value, boolean ignoreNotFound) {
        value = removeWS(value);
        byte[] hash = hashReference(value).digest();
        Node child = getCache(hash);
        if (child == null) {
            child = createNode();
            VALUE.set(child, value);
            createCache(child, hash);
        }
        return parent.createRelationshipTo(child, relationshipType());
    }

    @Override
    public String reference(Relationship r) {
        return VALUE.get(r.getEndNode());
    }

    private String removeWS(String value) {
        StringBuilder buf = new StringBuilder();
        if (value.length() > 0) {
            StringTokenizer tok = new StringTokenizer(value);
            while (tok.hasMoreTokens()) {
                buf.append(tok.nextToken());
                if (tok.hasMoreTokens()) buf.append(' ');
            }
        }
        return buf.toString();
    }

}