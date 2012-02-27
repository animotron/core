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
package org.animotron;

import junit.framework.Assert;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.junit.Test;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;

import java.util.Iterator;

import static org.animotron.graph.AnimoGraph.createNode;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class OrderTest extends ATest {

    private enum Rel implements RelationshipType { A, B, C}

    @Test
	public void test_00() {

        Node node = AnimoGraph.execute(
            new GraphOperation<Node>() {
                @Override
                public Node execute() throws Throwable {
                    Node node = createNode();
                    node.createRelationshipTo(createNode(), Rel.A);
                    node.createRelationshipTo(createNode(), Rel.B);
                    node.createRelationshipTo(createNode(), Rel.C);
                    return node;
                }
            }
        );

        Iterator<Relationship> it = node.getRelationships().iterator();
        Assert.assertEquals(it.next().getType(), Rel.A);
        Assert.assertEquals(it.next().getType(), Rel.B);
        Assert.assertEquals(it.next().getType(), Rel.C);

    }

    @Test
	public void test_01() {

        Node node = AnimoGraph.execute(
            new GraphOperation<Node>() {
                @Override
                public Node execute() throws Throwable {
                    Node start = createNode();
                    Node end = createNode();
                    start.createRelationshipTo(end, Rel.A);
                    start.createRelationshipTo(end, Rel.B);
                    start.createRelationshipTo(end, Rel.C);
                    return start;
                }
            }
        );

        Iterator<Relationship> it = node.getRelationships().iterator();
        Assert.assertEquals(it.next().getType(), Rel.A);
        Assert.assertEquals(it.next().getType(), Rel.B);
        Assert.assertEquals(it.next().getType(), Rel.C);

    }

    @Test
	public void test_02() {

        Node node = AnimoGraph.execute(
            new GraphOperation<Node>() {
                @Override
                public Node execute() throws Throwable {
                    Node start = createNode();
                    Node end = createNode();
                    start.createRelationshipTo(end, Rel.A);
                    start.createRelationshipTo(end, Rel.B);
                    start.createRelationshipTo(end, Rel.C);
                    return end;
                }
            }
        );

        Iterator<Relationship> it = node.getRelationships().iterator();
        Assert.assertEquals(it.next().getType(), Rel.A);
        Assert.assertEquals(it.next().getType(), Rel.B);
        Assert.assertEquals(it.next().getType(), Rel.C);

    }

    @Test
	public void test_03() {

        final Node node = AnimoGraph.execute(
            new GraphOperation<Node>() {
                @Override
                public Node execute() throws Throwable {
                    return createNode();
                }
            }
        );

        AnimoGraph.execute(
            new GraphOperation<Void>() {
                @Override
                public Void execute() throws Throwable {
                    node.createRelationshipTo(createNode(), Rel.A);
                    node.createRelationshipTo(createNode(), Rel.B);
                    node.createRelationshipTo(createNode(), Rel.C);
                    return null;
                }
            }
        );

        Iterator<Relationship> it = node.getRelationships().iterator();
        Assert.assertEquals(it.next().getType(), Rel.A);
        Assert.assertEquals(it.next().getType(), Rel.B);
        Assert.assertEquals(it.next().getType(), Rel.C);

    }

    @Test
	public void test_04() {

        final Node node = AnimoGraph.execute(
            new GraphOperation<Node>() {
                @Override
                public Node execute() throws Throwable {
                    return createNode();
                }
            }
        );

        AnimoGraph.execute(
            new GraphOperation<Void>() {
                @Override
                public Void execute() throws Throwable {
                    node.createRelationshipTo(createNode(), Rel.A);
                    return null;
                }
            }
        );

        AnimoGraph.execute(
            new GraphOperation<Void>() {
                @Override
                public Void execute() throws Throwable {
                    node.createRelationshipTo(createNode(), Rel.B);
                    return null;
                }
            }
        );

        AnimoGraph.execute(
            new GraphOperation<Void>() {
                @Override
                public Void execute() throws Throwable {
                    node.createRelationshipTo(createNode(), Rel.C);
                    return null;
                }
            }
        );

        Iterator<Relationship> it = node.getRelationships().iterator();
        Assert.assertEquals(it.next().getType(), Rel.A);
        Assert.assertEquals(it.next().getType(), Rel.B);
        Assert.assertEquals(it.next().getType(), Rel.C);

    }

    @Test
	public void test_05() {

        final Node start = AnimoGraph.execute(
            new GraphOperation<Node>() {
                @Override
                public Node execute() throws Throwable {
                    return createNode();
                }
            }
        );

        final Node end = AnimoGraph.execute(
            new GraphOperation<Node>() {
                @Override
                public Node execute() throws Throwable {
                    return createNode();
                }
            }
        );

        AnimoGraph.execute(
            new GraphOperation<Void>() {
                @Override
                public Void execute() throws Throwable {
                    start.createRelationshipTo(end, Rel.A);
                    start.createRelationshipTo(end, Rel.B);
                    start.createRelationshipTo(end, Rel.C);
                    return null;
                }
            }
        );

        Iterator<Relationship> it = start.getRelationships().iterator();
        Assert.assertEquals(it.next().getType(), Rel.A);
        Assert.assertEquals(it.next().getType(), Rel.B);
        Assert.assertEquals(it.next().getType(), Rel.C);

        it = end.getRelationships().iterator();
        Assert.assertEquals(it.next().getType(), Rel.A);
        Assert.assertEquals(it.next().getType(), Rel.B);
        Assert.assertEquals(it.next().getType(), Rel.C);

    }

    @Test
	public void test_06() {

        final Node start = AnimoGraph.execute(
            new GraphOperation<Node>() {
                @Override
                public Node execute() throws Throwable {
                    return createNode();
                }
            }
        );

        final Node end = AnimoGraph.execute(
            new GraphOperation<Node>() {
                @Override
                public Node execute() throws Throwable {
                    return createNode();
                }
            }
        );

        AnimoGraph.execute(
            new GraphOperation<Void>() {
                @Override
                public Void execute() throws Throwable {
                    start.createRelationshipTo(end, Rel.A);
                    return null;
                }
            }
        );

        AnimoGraph.execute(
            new GraphOperation<Void>() {
                @Override
                public Void execute() throws Throwable {
                    start.createRelationshipTo(end, Rel.B);
                   return null;
                }
            }
        );

        AnimoGraph.execute(
            new GraphOperation<Void>() {
                @Override
                public Void execute() throws Throwable {
                    start.createRelationshipTo(end, Rel.C);
                    return null;
                }
            }
        );

        Iterator<Relationship> it = start.getRelationships().iterator();
        Assert.assertEquals(it.next().getType(), Rel.A);
        Assert.assertEquals(it.next().getType(), Rel.B);
        Assert.assertEquals(it.next().getType(), Rel.C);

        it = end.getRelationships().iterator();
        Assert.assertEquals(it.next().getType(), Rel.A);
        Assert.assertEquals(it.next().getType(), Rel.B);
        Assert.assertEquals(it.next().getType(), Rel.C);

    }

}

