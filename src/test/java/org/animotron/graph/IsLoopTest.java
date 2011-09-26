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
package org.animotron.graph;

import junit.framework.Assert;
import org.animotron.ATest;
import org.animotron.expression.Expression;
import org.animotron.Properties;
import org.animotron.exception.AnimoException;
import org.animotron.statement.operator.THE;
import org.animotron.statement.relation.IS;
import org.junit.Test;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;

import java.io.IOException;

import static org.animotron.expression.Expression._;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class IsLoopTest extends ATest {
	
    private void test(String op) {
        Node n = AnimoGraph.getTOP().getSingleRelationship(RelationshipTypes.TOP, Direction.OUTGOING).getEndNode();
        Assert.assertEquals("", op, Properties.NAME.get(n));
    }

	@Test
	public void storeAndSerializeResult() throws AnimoException, IOException {
        new Expression(_(THE._, "A"));
        test("A");
        new Expression(_(THE._, "A", _(IS._, "C")));
        test("C");
        new Expression(_(THE._, "B", _(IS._, "A")));
        test("C");
        new Expression(_(THE._, "C", _(IS._, "B")));
        test("C");
        new Expression(_(THE._, "C", _(IS._, "B"), _(IS._, "D")));
        test("D");
	}

}
