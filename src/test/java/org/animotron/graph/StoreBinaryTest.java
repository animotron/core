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

import org.animotron.ATest;
import org.animotron.expression.CommonExpression;
import org.animotron.expression.JExpression;
import org.animotron.graph.serializer.AnimoSerializer;
import org.animotron.statement.compare.WITH;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.THE;
import org.animotron.statement.query.ANY;
import org.junit.Test;
import org.neo4j.graphdb.Relationship;

import java.io.ByteArrayInputStream;
import java.io.File;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.value;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class StoreBinaryTest extends ATest {
	
	private static final String TXT = 
		"Lorem ipsum dolor sit amet, consectetur adipiscing elit." +
		" Phasellus rutrum gravida ante nec consectetur. Sed maur" +
		"is libero, vulputate a viverra nec, porta at purus. Done" +
		"c sed consequat lorem. Donec lacinia metus euismod mi el" +
		"eifend mattis. Mauris porttitor risus sed risus tempor a" +
		"uctor. Curabitur quam augue, vestibulum ut aliquam eget," +
		" tincidunt vitae nisi. Donec libero purus, convallis non" +
		" semper non, molestie adipiscing sapien. Sed facilisis e" +
		"rat in ligula aliquet consectetur. Nulla luctus, velit a" +
		"c faucibus tincidunt, justo sem aliquam elit, eu mattis " +
		"arcu nunc eu diam. Fusce vulputate nunc imperdiet diam c" +
		"onvallis ultrices eu sit amet velit. Lorem ipsum dolor s" +
		"it amet, consectetur adipiscing elit. Curabitur eget sem" +
		" eu nisl luctus feugiat a eget enim. Nulla ut dui purus," +
		" sit amet cursus est. Suspendisse potenti.";

    private static final String s = File.separator;
    private static final String PATH = s+"content"+s+"article"+s+"test.txt";

	@Test
	public void storeAndSerialize() throws Exception {
        System.out.println("Test binary stream ...");
        
    	Relationship r = new CommonExpression(new ByteArrayInputStream(TXT.getBytes()), PATH);
        AnimoSerializer.serialize(r, System.out);

        JExpression s = new JExpression(
          _(AN._, THE._.reference(r).toString())
        );
        assertBinary(s, TXT);
        
        JExpression f = new JExpression(
          _(ANY._, "file")
        );
        assertBinary(f, TXT);

        JExpression t = new JExpression(
          _(ANY._, "test.txt")
        );
        assertBinary(t, TXT);

        JExpression n = new JExpression(
          _(ANY._, "article", _(WITH._, "name", value("test")))
        );
        assertBinary(n, TXT);

        JExpression e = new JExpression(
          _(ANY._, "file", _(WITH._, "extension", value("txt")))
        );
        assertBinary(e, TXT);

        JExpression a = new JExpression(
          _(ANY._, "article")
        );
        assertBinary(a, TXT);

        JExpression c = new JExpression(
          _(ANY._, "content")
        );
        assertBinary(c, TXT);

        System.out.println("done.");
	}

}
