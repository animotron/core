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
package org.animotron.graph;

import org.animotron.ATest;
import org.animotron.exception.AnimoException;
import org.animotron.expression.BinaryExpression;
import org.animotron.expression.DefaultDescription;
import org.animotron.expression.JExpression;
import org.animotron.graph.serializer.CachedSerializer;
import org.animotron.statement.compare.WITH;
import org.animotron.statement.operator.AN;
import org.animotron.statement.query.ANY;
import org.junit.Test;
import org.neo4j.graphdb.Relationship;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;

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
	public void storeAndSerialize() throws Throwable {
        System.out.println("Test binary stream ...");
        
    	Relationship r = new TestExpression(new ByteArrayInputStream(TXT.getBytes()), PATH);
    	CachedSerializer.ANIMO.serialize(r, System.out);

        JExpression s = new JExpression(
          _(AN._, r)
        );
        assertBinary(s, TXT);
        
        JExpression f = new JExpression(
          _(ANY._, "file")
        );
        assertBinary(f, TXT);

        JExpression t = new JExpression(
            _(ANY._, "test")
        );
        assertBinary(t, TXT);

        t = new JExpression(
            _(ANY._, "txt")
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

    private class TestExpression extends BinaryExpression {

        private String path;

        public TestExpression(InputStream stream, String path) throws Throwable {
            super(stream, true);
            this.path = path;
        }

        @Override
        protected void description() throws AnimoException, IOException {
            DefaultDescription.create(builder, path);
        }
    }
}
