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

import org.animotron.utils.MessageDigester;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.security.MessageDigest;

import static org.animotron.Properties.VALUE;
import static org.animotron.graph.AnimoGraph.createNode;
import static org.animotron.graph.AnimoGraph.order;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class TEXT extends MLOperator {
	
	public static final TEXT _ = new TEXT();
	
	private TEXT() { super("text"); }

    protected TEXT(String name) { super(name); }

    @Override
    public Relationship build(Node parent, String value, int order, boolean ignoreNotFound) {
        Node child = createNode();
        Relationship r = parent.createRelationshipTo(child, relationshipType());
        VALUE.set(child, value);
        order(r, order);
        return r;
    }

    @Override
    public String reference(Relationship r) {
        return VALUE.get(r.getEndNode());
    }

    @Override
    public MessageDigest hash(String reference) {
        MessageDigest md = MessageDigester.md();
        if (reference != null)
            md.update(reference.getBytes());
        return md;
    }

}