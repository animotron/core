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

import static org.animotron.Expression._;

import javax.xml.stream.XMLStreamException;

import org.animotron.ATest;
import org.animotron.Expression;
import org.animotron.operator.THE;
import org.animotron.operator.relation.HAVE;
import org.junit.Test;
import org.neo4j.graphdb.Relationship;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class HaveLoopTest extends ATest {
	
	private static final Relationship A = 
		new Expression(_(THE._, "A", _(HAVE._, "C")));
	
	private static final Relationship B = 
		new Expression(_(THE._, "B", _(HAVE._, "A")));
	
	private static final Relationship C = 
		new Expression(_(THE._, "C", _(HAVE._, "B")));
	
	@Test
	public void storeAndSerializeResult() throws XMLStreamException {
		
		System.out.println("Prepare");
		System.out.print("The a: ");
		GraphSerializer.serialize(A, System.out);
		System.out.println();
		System.out.print("The b: ");
		GraphSerializer.serialize(B, System.out);
		System.out.println();
		System.out.print("The c: ");
		GraphSerializer.serialize(C, System.out);
		System.out.println();
		System.out.println();
		
        
//        Transaction tx = beginTx();
//        try { 
//	        Relationship r = THE._.relationship("B");
//	        assertNotNull(r);
//	        XMLStreamWriter writer = OUTPUT_FACTORY.createXMLStreamWriter(System.out);
//	        StAXGraphSerializer serializer = new StAXGraphSerializer(writer);
//	        serializer.serialize(r);
//	        tx.success();
//	        
//        } catch (Exception e) {
//        	e.printStackTrace();
//			fail(e.toString());
//        } finally {
//        	tx.finish();
//        }
            
        System.out.println("done.");
	}
}
