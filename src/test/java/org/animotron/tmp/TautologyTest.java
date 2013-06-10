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
package org.animotron.tmp;

import org.junit.Ignore;
import org.junit.Test;

import static org.junit.Assert.fail;

/**
 * @author Ferenc Kovacs
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class TautologyTest extends ATest {

	/*
	 * Test two tautology
	 * 
	 * If a topic (word is used)  it has to make sense
	 * A word (topic) makes sense, if it exist.
	 * It it exists, it has the property of existence
	 * So the existence of the property of existence must be checked
	 * This is done in two worlds
	 * a) perception (sensors)
	 * b) cognition (memory, thinking)
	 * 
	 * If a property of existence exists or if the object called topic has the property, 
	 * then it must be tangible and comparable with the representation of the topic-internally, 
	 * were representations are structured in object-relation-properties pattern implemented or 
	 * materialized by animo graphs
	 * 
	 * so if there is an object with a matching property of existence in the animo graph 
	 * that matches the perception of reality (which, if not is done real time and original, 
	 * live objects, must be reduced to a 2D representation of the same that is available or 
	 * accessible by the computer in a different, dummy reality topology.
	 * So the match is sought which will be two fitting  patterns to see that they are identical.
	 * 
	 * So you have a number of options with respect to the dimensions of the objects to compare. 
	 * The simplest one is a single dot representing an object representing a dot (in the animo graph)
	 * If this is true, then it is fine. We should then say that by using Tautology
	 * 
	 * This is natural language surface level
	 * An object is an object
	 * (or object is object  notice the insertion of IS as the indication of the property exists)
	 * Tautology is the end of a topic line with topic comment chain. You have nothing new to say. 
	 * This situation may be made explicit in the dialog by the computer in its answer.
	 * 
	 * Next you should add comment to the topic which exist in the context until a new topic is introduced. 
	 * However from the tautology or fact you may introduce reasoning
	 * 
	 * If an object is an object
	 * 
	 * and this has been verified as above, then it has a property, which is recursion to OBJECT
	 * 
	 * in other words
	 * property in connection with an object will be a mental object, an abstraction created in the second cycle 
	 * starting from object. the same is true about relation
	 * 
	 * So now we have a chance for making a definition
	 * A definition is the substitution of a topic (a noun phrase) with other topics (with other non phrases)
	 * In terms of objects (logic) it is the substitution of object with relation or property in any direction or combination
	 * 
	 * Next we come to conclusions
	 * object is (object/relation x property)
	 * from the formula you can derive
	 * 
	 * the above combinations as equations or identities or conversions, etc.
	 * 
	 */
	@Test
	@Ignore
	public void test_01() throws Throwable {
		testAnimi("object\n", "object");
		
		testAnimi("object is object\n", "object");
		fail("Not yet implemented");
	}
}
