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
package org.animotron.tmp;

import org.animotron.graph.AnimoGraph;
import org.junit.Ignore;
import org.junit.Test;
import org.neo4j.graphdb.Transaction;

import java.io.IOException;

/**
 * @author Ferenc Kovacs
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ObjectTopicNameTest extends ATest {

	/*
	 * Test One
	 * Object-Topic-Name
	 * Real world Logic Language
	 * 
	 * External word connected/contacted through a surface to internal world.
	 * This internal world is cognition/mind.
	 * The surface is interface/logic/semantic primitives
	 * External world is environment/context/object
	 * 
	 * Flow/transition/metamorphosis is experienced through interface as interaction
	 * external becomes internal
	 * internal becomes external
	 * through energy passed across the interface
	 * Logic rotates to find match when flow is possible
	 * 
	 * External and internal may be in sync and out of sync
	 * Internal is n search of a match
	 * Both external and internal are moving, they have to be stopped to see if their surface fit , it they have a hit or a match
	 * 
	 * Match is when the surface of external and that of internal are found to be in sync,  complementary or identical � connected (as true and valid connection) allowing the flow)
	 *  
	 * External is reduced to object first, the smallest thing to work with
	 * 
	 * 1.	Object belongs to external world � object will copied or doubled in the presence of another object in its internal world, or surface, like in case of a mirror
	 * 2.	Object must have a copy recorded in internal world. It will be a point-like zero dimension animo object
	 * 3.	Object must have a copy in Logic (interface) called Object for the link up or docking
	 * 4.	Object is topic in a dialog between external world and internal world �  language, near surface level
	 * 5.	topic is a noun or noun phrase at surface level
	 * 6.	Object -topic- animo object are connected through the same desire to be contacted/connected which may be seen from internal external and reflexive (commutative) aspects
	 * 
	 * 7.	The triangle of meaning is then realized here as referent (chunk of reality, an object) with a mirrored copy in the mind, animo point, which is connected to symbol/sign, etc. or a name (surface level) to go both ways, and meaning/relation, which is concept in the mind also called object (mental object, logic, semantic) naming the former relationship
	 * 8.	so we need equivalence and reciprocal relations:> external object (name to be specified in the discourse) > but on surface called topic > internal  (animo) object called a) name by programmer to create an animo point  b) and object by semantic parser that deals with below the surface structures connected to surface structures
	 * 9.	The only surface and below the surface connection now is
	 * 10.	topic (generic term) that has to be made specific in conversation
	 * 11.	Such specification is done through adding a comment. By saying that topic is object, we make object familiar for the system (see name)
	 * 12.	It should be possible to say topic is object and object is topic
	 * 13.	and name is a reference both to the external object and the copy (internal representation in animo) of the same object.  We then may  chose to have property and relation as topics
	 * 
	 * Animo identities
	 * 
	 * 14.	animo object is one point on surface where flow must be created in a contact
	 * 15.	that point is connected to the external which is represented/reflected in animo by an animo object (could find another name or word for it).
	 * 16.	The point on the surface connected to the external has a name to refer to the external reflected or copied by the internal. This is a materialization reference name of external and internal objects connected through being related and are considered identical or equivalent through topic and/or noun phrase
	 * 17.	The point on surface is called topic when external and internal are connected in a dialog (application context)
	 * 
	 */
	@Test
	@Ignore
	public void test_01() throws Throwable {
		
		String obj = "the "+uuid()+" name \"object\".";
		
		Transaction tx = AnimoGraph.beginTx();
		try {
			testAnimo(obj);
			//Labels.add(the, "object");
			
			tx.success();
		} finally {
			AnimoGraph.finishTx(tx);
		}
		
		testAnimiParser("object\n", obj);
		
		//say - object, get answer - object
		testAnimi("object\n", "object");
	}
	
	@Test
	@Ignore
	public void test_02() throws Throwable {
		
		//say - object, get answer - object
		testAnimi("object\n", "object");

		//? == any word
		testWord("object");
		
	}

	private void testWord(String word) throws IOException {
//		IndexHits<Relationship> hits = Labels.search(word);
//		
//		Relationship result = null;
//		for (Relationship r : hits) {
//			if (result == null)
//				result = r;
//			else
//				Assert.fail("more then one result");
//		}
//		
//		if (result == null)
//			Assert.fail("expecting animo object for '"+word+"', but get none");
//		
//		String actual = CachedSerializer.ANIMO.serialize(result);
//		Pattern pattern = Pattern.compile("the [0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12} name \""+word+"\".");
//		
//		System.out.println(actual);
//		
//		Matcher matcher = pattern.matcher(actual);
//		Assert.assertTrue(matcher.find());
//		Assert.assertFalse(matcher.find());
	}

}
