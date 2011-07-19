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
import org.animotron.operator.query.SELF;
import org.animotron.operator.relation.HAVE;
import org.animotron.operator.relation.IS;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class SelfTest extends ATest {
	
	@Test
	public void getFromPFlow() throws Exception {
        System.out.println("Test empty 'self' ...");
        
    	new Expression(
			_(THE._, "A")
		);
    	
    	new Expression(
			_(THE._, "B")
		);
    	
    	Expression C = new Expression(
			_(THE._, "C", _(HAVE._, "A", text(".")), _(HAVE._, "B", _(SELF._, "A")))
		);

    	Expression CC = new Expression(
			_(THE._, "CC", _(HAVE._, "A", text("CC")), _(HAVE._, "B", _(SELF._, "A")))
		);

    	Expression D = new Expression(
			_(THE._, "D", _(IS._, "C"), _(HAVE._, "A", text(":")))
		);

    	Expression E = new Expression(
			_(THE._, "E", _(GET._, "B", _(AN._, "C")))
		);
    	
//    	Expression F = new Expression(
//			_(THE._, "F", _(GET._, "B", _(AN._, "D")))
//		);
    	
        assertAnimo(C, "<the:C><have:A>.</have:A><have:B><have:A>.</have:A></have:B></the:C>");
        assertAnimo(CC, "<the:CC><have:A>CC</have:A><have:B><have:A>CC</have:A></have:B></the:CC>");
        assertAnimo(D, "<the:D><is:C/><have:A>:</have:A></the:D>");
        //System.out.println("self:B an:C");
        assertAnimo(E, "<the:E><have:B><have:A>.</have:A></have:B></the:E>");
//
//        //System.out.println("self:B an:D");
//        assertAnimo(F, "<the:F><have:B>:<have:B></the:F>");

        //System.out.println("done.");
	}
	
//	@Test
//	public void anyWithUse() throws Exception {
//        System.out.println("Test 'self' ...");
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
