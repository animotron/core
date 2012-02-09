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
package org.animotron.statement.language;

import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.junit.Assert;
import org.junit.Test;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import static org.animotron.expression.AnimoExpression.__;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class WordTest extends ATest {

	@Test
	public void test_01() throws Exception {
        __(
            "the test1 word \"test1\".",
            "the test2 word \"test2\"."
		);
        
        Thread.sleep(1000);
        
        Relationship r;
        IndexHits<Relationship> hits = WORD._.search("test1");
        try {
        	Assert.assertEquals(true, hits.hasNext());
        	
        	r = hits.next();

        	Assert.assertEquals(4, r.getId());

        	Assert.assertEquals(false, hits.hasNext());
        } finally {
        	hits.close();
        }
        
        hits = WORD._.search("test2");
        try {
        	Assert.assertEquals(true, hits.hasNext());
        	
        	r = hits.next();

        	Assert.assertEquals(9, r.getId());

        	Assert.assertEquals(false, hits.hasNext());
        } finally {
        	hits.close();
        }

        hits = WORD._.search("test*");
        try {
        	Assert.assertEquals(true, hits.hasNext());
        	
        	r = hits.next();
        	Assert.assertEquals(4, r.getId());

        	Assert.assertEquals(true, hits.hasNext());

        	r = hits.next();
        	Assert.assertEquals(9, r.getId());

        	Assert.assertEquals(false, hits.hasNext());
        } finally {
        	hits.close();
        }

        Thread.sleep(1000);
	}

	@Test
	public void test_02() throws Exception {
        __(
            "the test1 (test) (word \"test1\").",
            "the test2 (test) (word \"test2\").",
            "the pest3 (test) (word \"pest3\")."
		);
        
        Thread.sleep(1000);
        
        assertAnimoResult("any test search \"test1\"", "the test1 (test).");
        assertAnimoResult("any test search \"test2\"", "the test2 (test).");

        assertAnimoResult("all test search \"test*\"", "the test1 (test). the test2 (test).");
	}

	@Test
	public void test_10() throws Exception {
        __(
            "the test1 (word \"test1\")."
		);
        
        assertAnimoResult("word test1", "\"test1\".");
	}
}