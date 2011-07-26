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
import org.animotron.instruction.string.AfterLast;
import org.animotron.operator.query.GET;
import org.animotron.operator.query.SELF;
import org.animotron.operator.relation.HAVE;
import org.animotron.operator.relation.IS;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class GetTest extends ATest {
	
	@Test
	public void getFromPFlow_an_with_param() throws Exception {
        System.out.println("Test empty 'get' ...");

    	new Expression(
			_(THE._, "A", _(HAVE._, "B", _(GET._, "C")))
		);

    	Expression D = new Expression(
			_(THE._, "D", _(AN._, "A", _(HAVE._, "C", text("."))))
		);

        assertAnimo(D, "<the:D><the:A><have:B><have:C>.</have:C></have:B></the:A></the:D>");
        //System.out.println("done.");
}
	
	@Test
	public void getFromPFlow_cross_an_with_param() throws Exception {
        System.out.println("Test cross 'get' ...");
        
    	new Expression(
			_(THE._, "A", _(HAVE._, "B", _(GET._, "C")))
		);
    	
    	new Expression(
			_(THE._, "D", _(HAVE._, "E", _(GET._, "B")))
		);
    	
    	Expression X = new Expression(
			_(THE._, "X", _(AN._, "D", _(AN._, "A", _(HAVE._, "C", text(":")))))
		);
        	
        assertAnimo(X, "<the:X><the:D><have:E><have:B><have:C>:</have:C></have:B></have:E></the:D></the:X>");
        //System.out.println("done.");
	}

	@Test
	public void getFromPFlow_an_with_an() throws Exception {
        System.out.println("Test empty 'get' on AN with AN...");

    	new Expression(
			_(THE._, "A", _(HAVE._, "B", _(GET._, "C")))
		);

    	new Expression(
			_(THE._, "D", _(HAVE._, "C", text(".")))
		);

    	Expression E = new Expression(
			_(THE._, "E", _(AN._, "A", _(AN._, "D")))
		);

    	assertAnimo(E, "<the:E><the:A><have:B><have:C>.</have:C></have:B></the:A></the:E>");
        //System.out.println("done.");
	}
	
	@Test
	public void getFromPFlow_an_with_more_an() throws Exception {
        System.out.println("Test empty 'get' on AN with AN...");

    	new Expression(
			_(THE._, "A", _(HAVE._, "B", _(GET._, "C")))
		);

    	new Expression(
			_(THE._, "D", _(HAVE._, "C", text(".")))
		);

    	new Expression(
			_(THE._, "E", _(HAVE._, "C", text(":")))
		);

    	Expression F = new Expression(
			_(THE._, "F", _(AN._, "A", _(AN._, "D"), _(AN._, "E", _(HAVE._, "C", text("_")))))
		);

    	assertAnimo(F, "<the:F><the:A><have:B><have:C>.</have:C><have:C>_</have:C></have:B></the:A></the:F>");
        //System.out.println("done.");
	}
	
    @Test
    public void checkHAVEonIS() throws Exception {
        System.out.println("Mime type use case ...");

        new Expression(
        _(THE._, "A",
            _(HAVE._, "A1", text("some.path")),

            _(IC._, "A2",
                _(SELF._, "A1")),

            _(IC._, "B1",
                _(AfterLast._,
                    text("."),
                    _(SELF._, "A1")))
        ));

        new Expression(
        _(THE._, "B",
            _(IS._, "A"),
            _(HAVE._, "A1", text("test.txt"))
        ));

        Expression C0 = new Expression(
        _(THE._, "C0",
            _(GET._, "A1",
                _(AN._, "A")
        )));
        assertAnimo(C0, "<the:C0><have:A1>some.path</have:A1></the:C0>");

        Expression C1 = new Expression(
        _(THE._, "C1",
            _(GET._, "A1",
                _(AN._, "B")
        )));
        assertAnimo(C1, "<the:C1><have:A1>test.txt</have:A1></the:C1>");

        Expression C2 = new Expression(
        _(THE._, "C2",
            _(GET._, "A2",
                _(AN._, "B")
        )));
        assertAnimo(C2, "<the:C2><have:A2><have:A1>test.txt</have:A1></have:A2></the:C2>");

        Expression C3 = new Expression(
        _(THE._, "C3",
            _(GET._, "E1",
                _(AN._, "B")
        )));
        assertAnimo(C3, "<the:C3/>");

        Expression C4 = new Expression(
        _(THE._, "C4",
            _(GET._, "B1",
                _(AN._, "B")
        )));
        assertAnimo(C4, "<the:C4><have:B1>txt</have:B1></the:C4>");

        //System.out.println("done.");
    }

    @Test
    public void getFromPFlow_an_with_stack() throws Exception {
        System.out.println("Test empty 'get' on AN with AN...");

        new Expression(
            _(THE._, "A", _(GET._, "X"))
        );

        new Expression(
            _(THE._, "B", _(GET._, "Y"), _(AN._, "A"))
        );

        new Expression(
            _(THE._, "C", _(GET._, "Z"), _(AN._, "B"))
        );

        Expression E = new Expression(
            _(THE._, "E", _(AN._, "C", _(HAVE._, "X", text("α")), _(HAVE._, "β", text("γ")), _(HAVE._, "Z", text("Z"))))
        );

        assertAnimo(E, "<the:E><the:C><have:C>γ</have:C><the:B><have:B>β</have:B><the:A><have:A>α</have:A></the:A></the:B></the:C></the:E>");
        //System.out.println("done.");
    }


}
