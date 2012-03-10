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
package org.animotron.statement.math;

import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.animotron.statement.operator.THE;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;
import org.neo4j.graphdb.Relationship;

import static org.animotron.expression.AnimoExpression.__;
import static org.animotron.graph.RelationshipTypes.TRI;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class FormulaTest extends ATest {

	@Test
	@Ignore
	public void test_01() throws Throwable {
        __(
            new AnimoExpression("the qty / (get cost) (get price)."),
            new AnimoExpression("the cost * (get qty) (get price)."),
            new AnimoExpression("the price / (get cost) (get qty).")

            //new AnimoExpression("the m1 (qty 10) (cost 50).")
		);
        
        int count = 0;
        Relationship qty = THE._.get("qty");
        
        System.out.println("qty");
        for (Relationship r : qty.getEndNode().getRelationships(TRI)) {
        	System.out.println(r);
        	count++;
	    	if (r.getId() == 20) {
	    		Assert.assertEquals("*", r.getProperty("TYPE"));
	    		Assert.assertEquals((long)6, r.getProperty("TO_NODE"));
	    		Assert.assertEquals((long)2, r.getStartNode().getId());
	    		Assert.assertEquals((long)8, r.getEndNode().getId());
	    	} else if (r.getId() == 27) {
        		Assert.assertEquals("/", r.getProperty("TYPE"));
        		Assert.assertEquals((long)8, r.getProperty("TO_NODE"));
        		Assert.assertEquals((long)6, r.getStartNode().getId());
        		Assert.assertEquals((long)2, r.getEndNode().getId());
        	} else {
        		Assert.fail("That should not be "+r);
        	}
        }
        Assert.assertEquals(2, count);
        
        count = 0;
        Relationship cost = THE._.get("cost");
        
        System.out.println("cost");
        for (Relationship r : cost.getEndNode().getRelationships(TRI)) {
        	System.out.println(r);
        	count++;
	    	if (r.getId() == 11) {
	    		Assert.assertEquals("/", r.getProperty("TYPE"));
	    		Assert.assertEquals((long)2, r.getProperty("TO_NODE"));
	    		Assert.assertEquals((long)6, r.getStartNode().getId());
	    		Assert.assertEquals((long)8, r.getEndNode().getId());
	    	} else if (r.getId() == 27) {
        		Assert.assertEquals("/", r.getProperty("TYPE"));
        		Assert.assertEquals((long)8, r.getProperty("TO_NODE"));
        		Assert.assertEquals((long)6, r.getStartNode().getId());
        		Assert.assertEquals((long)2, r.getEndNode().getId());
        	} else {
        		Assert.fail("That should not be "+r);
        	}
        }
        Assert.assertEquals(2, count);

        //assertStringResult("get cost m1.", "50");
	}
	
    @Test
    public void test_02() throws Throwable {
        __(
	        "the item1 (goods item) (cost * (10) (USD)).",
	        "the item2 (goods item) (cost * (5) (USD))."
        );

        assertAnimoResult(
            "+ get cost all item",
            "* (15) (USD)."
		);
    }

}