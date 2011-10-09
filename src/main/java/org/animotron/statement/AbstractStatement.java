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
package org.animotron.statement;

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.AnimoRelationshipType;
import org.animotron.graph.GraphOperation;
import org.animotron.inmemory.InMemoryRelationship;
import org.animotron.utils.MessageDigester;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;

import java.security.MessageDigest;

import static org.animotron.Properties.CID;
import static org.animotron.Properties.RID;
import static org.animotron.graph.RelationshipTypes.RESULT;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class AbstractStatement implements Statement {
	
    private MessageDigest md;
	private String name;
	public final RelationshipType relationshipType;
	public final String rType;
	
	public AbstractStatement(String name) {
		this.name = name;
		this.relationshipType = AnimoRelationshipType.get(name);
		rType = this.relationshipType.name();
        this.md = MessageDigester.md();
        md.update(name.getBytes());
	}

//	public AbstractStatement(VALUE reference, VALUE resultRelationshipType) {
//        this(reference);
//	}

	@Override
	public String name() {
		return name;
	}
	
	@Override
	public RelationshipType relationshipType() {
		return relationshipType;
	}
	
	protected Relationship createResult(final Relationship context, final Node node, final Relationship r, final RelationshipType rType) {
		return AnimoGraph.execute(new GraphOperation<Relationship>() {
			@Override
			public Relationship execute() {
				Relationship res = node.createRelationshipTo(r.getEndNode(), rType);
				//store to relationship arrow 
				RID.set(res, r.getId());
				//for debug
				CID.set(res, context.getId());
				AnimoGraph.result(res, context.getId());
				return res;
			}
		});
	}
	
	protected Relationship createResultInMemory(Node node, Relationship r) {
		Relationship res = new InMemoryRelationship(node, r.getEndNode(), RESULT);
		//store to relationship arrow 
		RID.set(res, r.getId());
		
		return res;
	}

    @Override
    public Object reference(Relationship r) {
        return null;
    }

    protected final MessageDigest md() {
        try {
            return (MessageDigest) md.clone();
        } catch (CloneNotSupportedException e) {
			//can't be, but throw runtime error
			throw new RuntimeException(e);
        }
    }

    @Override
    public MessageDigest hash(Object reference) {
        MessageDigest md = md();
        if (reference instanceof String ||
            reference instanceof Number ||
            reference instanceof Boolean)
            md.update(reference.toString().getBytes());
        return md;
    }

}
