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

import java.io.IOException;

import junit.framework.Assert;

import org.animotron.ATest;
import org.animotron.cache.FileCache;
import org.animotron.expression.AnimoExpression;
import org.animotron.graph.serializer.CachedSerializer;
import org.junit.Test;

import static org.animotron.expression.AnimoExpression.__;
import static org.junit.Assert.assertNotNull;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class DependencyTrackingTest extends ATest {

    @Test
    public void test_00() throws Throwable {

        __(
	        "the goods word \"goods\".",
	        "the qty (part (number) (UoM)).",
	        "the price (part (number) (currency) (UoM)).",
	        "the cost (part (number) (currency)).",
	        
	        "the item (goods aaa) (qty (1) (kg)) (cost (10) (USD))."
        );

        assertAnimoResult(
            "get cost item",
            "cost 10 (USD)."
        );

        __(
	        "the item (goods aaa) (qty (1) (kg)) (cost (5) (USD))."
        );

        assertAnimoResult(
            "get cost item",
            "cost 5 (USD)."
		);
    }

    @Test
    public void test_01() throws Throwable {

        __(
	        "the goods word \"goods\".",
	        "the qty (part (number) (UoM)).",
	        "the price (part (number) (currency) (UoM)).",
	        "the cost (part (number) (currency)).",
	        
	        "the item (goods aaa) (qty (1) (kg)) (cost (10) (USD))."
        );

        assertCachedAnimoResult(
            "get cost item",
            "cost 10 (USD)."
        );

        __(
	        "the item (goods aaa) (qty (1) (kg)) (cost (5) (USD))."
        );

        assertCachedAnimoResult(
            "get cost item",
            "cost 5 (USD)."
		);
    }

    protected void assertCachedAnimoResult(String op, String expected) throws IOException {
        assertNotNull(op);
        System.out.println("Animo result serializer...");
        
        String result = CachedSerializer.ANIMO_RESULT.serialize(new AnimoExpression(op), FileCache._);
        
        System.out.println(result);
        Assert.assertEquals("", expected, result);
        System.out.println();
    }
}