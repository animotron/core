/*
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
package org.animotron.operator;

import static org.animotron.Expression._;
import static org.animotron.Expression.text;

import org.animotron.ATest;
import org.animotron.Expression;
import org.animotron.operator.query.GET;
import org.animotron.operator.relation.HAVE;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class GetTests extends ATest {
	
	@Test
	public void getFromPFlow() throws Exception {
        System.out.println("Test empty 'get' ...");
        
    	Expression A = new Expression(
			_(THE._, "A", _(HAVE._, "B", _(GET._, "C")))
		);
    	
    	Expression B = new Expression(
			_(THE._, "D", _(AN._, "A", _(HAVE._, "C", text("."))))
		);
    	
        assertAnimo(A, "<the:A><have:B/></the:A>");
        assertAnimo(B, "<the:D><the:A><have:B><have:C>.</have:C></have:B></the:A></the:D>");
        
        //System.out.println("done.");
	}
	
//	@Test
//	public void anyWithUse() throws Exception {
//        System.out.println("Test 'get' ...");
//        
//    	new Expression(
//			_(THE._, "A")
//		);
//    	new Expression(
//			_(THE._, "B", _(IS._, "A"))
//		);
//    	new Expression(
//			_(THE._, "C", _(IS._, "B"))
//		);
//    	new Expression(
//			_(THE._, "D", _(ANY._, "A"))
//		);
//    	new Expression(
//			_(THE._, "E", _(AN._, "D", _(USE._, "B")))
//		);
//    	new Expression(
//			_(THE._, "F", _(AN._, "D", _(USE._, "C")))
//		);
//    	new Expression(
//			_(THE._, "G", _(AN._, "E", _(USE._, "A")))
//		);
//    	
//        //System.out.println("any:A");
//        //assertEquals("D", "<the:D><the:A></the:A><the:B></the:B><the:C></the:C></the:D>");
//
//        //System.out.println("an:D use:B");
////        assertAnimo(E, "<the:E><the:D><the:B></the:B><the:C></the:C></the:D></the:E>");
//
//        //System.out.println("done.");
//	}
}
