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
 *  but WITHOUT ALL WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package org.animotron.statement.operator;

import org.animotron.ATest;
import org.animotron.Expression;
import org.animotron.statement.query.GET;
import org.animotron.statement.query.SELF;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.animotron.statement.string.AfterLast;
import org.junit.Test;

import static org.animotron.Expression._;
import static org.animotron.Expression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class GetTest extends ATest {
	
	@Test
	public void getOnManyAN() throws Exception {
    	new Expression(
			_(THE._, "A", _(HAVE._, "Z", text("A")))
		);

    	new Expression(
			_(THE._, "B", _(HAVE._, "Z", text("B")))
		);

    	Expression d = new Expression(
			_(THE._, "d", _(GET._, "Z", _(AN._, "A"), _(AN._, "A")))
		);
        assertAnimoResult(d, "the d have Z \"A\" have Z \"B\"\n");
	}

	@Test
	public void getFromPFlow_an_with_param() throws Exception {
    	new Expression(
			_(THE._, "A", _(HAVE._, "B", _(GET._, "C")))
		);

    	Expression D = new Expression(
			_(THE._, "D", _(AN._, "A", _(HAVE._, "C", text("."))))
		);
    	//assertXMLResult(D, "<the:D><the:A><have:B><have:C>.</have:C></have:B></the:A></the:D>");
        assertAnimoResult(D, "the D the A have B have C \".\"\n");
	}
	
	@Test
	public void getFromPFlow_cross_an_with_param() throws Exception {
        
    	new Expression(
			_(THE._, "A", _(HAVE._, "B", _(GET._, "C")))
		);
    	
    	new Expression(
			_(THE._, "D", _(HAVE._, "E", _(GET._, "B")))
		);
    	
    	Expression X = new Expression(
			_(THE._, "X", _(AN._, "D", _(AN._, "A", _(HAVE._, "C", text(":")))))
		);
    	//assertXMLResult(X, "<the:X><the:D><have:E><have:B><have:C>:</have:C></have:B></have:E></the:D></the:X>");
        assertAnimoResult(X, "the X the D have E have B have C \":\"\n");
	}

	@Test
	public void getFromPFlow_an_with_an() throws Exception {

    	new Expression(
			_(THE._, "A", _(HAVE._, "B", _(GET._, "C")))
		);

    	new Expression(
			_(THE._, "D", _(HAVE._, "C", text(".")))
		);

    	Expression E = new Expression(
			_(THE._, "E", _(AN._, "A", _(AN._, "D")))
		);
    	//assertXMLResult(E, "<the:E><the:A><have:B><have:C>.</have:C></have:B></the:A></the:E>");
    	assertAnimoResult(E, "the E the A have B have C \".\"\n");
	}
	
	@Test
	public void getFromPFlow_an_with_more_an() throws Exception {

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
    	//assertXMLResult(F, "<the:F><the:A><have:B><have:C>.</have:C><have:C>:</have:C></have:B></the:A></the:F>");
    	assertAnimoResult(F, "the F the A have B have C \".\" have C \":\"\n");
	}
	
    @Test
    public void checkIConIS() throws Exception {

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
        //assertXMLResult(C0, "<the:C0><have:A1>some.path</have:A1></the:C0>");
        assertAnimoResult(C0, "the C0 have A1 \"some.path\"\n");

        Expression C1 = new Expression(
        _(THE._, "C1",
            _(GET._, "A1",
                _(AN._, "B")
        )));
        //assertXMLResult(C1, "<the:C1><have:A1>test.txt</have:A1></the:C1>");
        assertAnimoResult(C1, "the C1 have A1 \"test.txt\"\n");

        Expression C2 = new Expression(
        _(THE._, "C2",
            _(GET._, "A2",
                _(AN._, "B")
        )));
        //assertXMLResult(C2, "<the:C2><have:A2><have:A1>test.txt</have:A1></have:A2></the:C2>");
        assertAnimoResult(C2, "the C2 have A2 have A1 \"test.txt\"\n");

        Expression C3 = new Expression(
        _(THE._, "C3",
            _(GET._, "E1",
                _(AN._, "B")
        )));
        //assertXMLResult(C3, "<the:C3/>");
        assertAnimoResult(C3, "the C3\n");

        Expression C4 = new Expression(
        _(THE._, "C4",
            _(GET._, "B1",
                _(AN._, "B")
        )));
        //assertXMLResult(C4, "<the:C4><have:B1>txt</have:B1></the:C4>");
        assertAnimoResult(C4, "the C4 have B1 \"txt\"\n");
    }

    @Test
    public void checkHAVEonIS() throws Exception {
        new Expression(
        _(THE._, "A",
            _(HAVE._, "A1", text("some.path")),

            _(HAVE._, "A2",
                _(SELF._, "A1")),

            _(HAVE._, "B1",
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
        //assertXMLResult(C0, "<the:C0><have:A1>some.path</have:A1></the:C0>");
        assertAnimoResult(C0, "the C0 have A1 \"some.path\"\n");

        Expression C1 = new Expression(
        _(THE._, "C1",
            _(GET._, "A1",
                _(AN._, "B")
        )));
        //assertXMLResult(C1, "<the:C1><have:A1>test.txt</have:A1></the:C1>");
        assertAnimoResult(C1, "the C1 have A1 \"test.txt\"\n");

        Expression C2 = new Expression(
        _(THE._, "C2",
            _(GET._, "A2",
                _(AN._, "B")
        )));
        //assertXMLResult(C2, "<the:C2><have:A2><have:A1>test.txt</have:A1></have:A2></the:C2>");
        assertAnimoResult(C2, "the C2 have A2 have A1 \"test.txt\"\n");

        Expression C3 = new Expression(
        _(THE._, "C3",
            _(GET._, "E1",
                _(AN._, "B")
        )));
        //assertXMLResult(C3, "<the:C3/>");
        assertAnimoResult(C3, "the C3\n");

        Expression C4 = new Expression(
        _(THE._, "C4",
            _(GET._, "B1",
                _(AN._, "B")
        )));
        //assertXMLResult(C4, "<the:C4><have:B1>txt</have:B1></the:C4>");
        assertAnimoResult(C4, "the C4 have B1 \"txt\"\n");
    }

    @Test
    public void getFromPFlow_an_with_stack() throws Exception {

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
            _(THE._, "E", _(AN._, "C", _(HAVE._, "X", text("α")), _(HAVE._, "Y", text("β")), _(HAVE._, "Z", text("γ"))))
        );
        //assertXMLResult(E, "<the:E><the:C><have:Z>γ</have:Z><the:B><have:Y>β</have:Y><the:A><have:X>α</have:X></the:A></the:B></the:C></the:E>");
        assertAnimoResult(E, "the E (the C have Z \"γ\" (the B have Y \"β\" the A have X \"α\"))\n");
    }

    @Test
    public void getFromPFlow_more_an_with_stack() throws Exception {

        new Expression(
            _(THE._, "A", _(GET._, "X"), _(HAVE._, "Z", text("γ")))
        );

        new Expression(
            _(THE._, "B", _(GET._, "Y"), _(AN._, "A"))
        );

        new Expression(
            _(THE._, "C", _(GET._, "Z"))
        );

        Expression E = new Expression(
            _(THE._, "E", _(AN._, "C", _(AN._, "B", _(HAVE._, "B", text("β")))))
        );
        //assertXMLResult(E, "<the:E><the:C><have:Z>γ</have:Z></the:C></the:E>");
        assertAnimoResult(E, "the E the C have Z \"γ\"\n");
    }

    @Test
    public void getFromPFlow_one_more_an_with_stack() throws Exception {

        new Expression(
            _(THE._, "A", _(GET._, "X"), _(HAVE._, "Z", text("γ")))
        );

        new Expression(
            _(THE._, "B", _(GET._, "Y"))
        );

        new Expression(
            _(THE._, "C", _(GET._, "Z"))
        );

        Expression E = new Expression(
            _(THE._, "E", _(AN._, "C", _(AN._, "B", _(AN._, "A"))))
        );
        //assertXMLResult(E, "<the:E><the:C><have:Z>γ</have:Z></the:C></the:E>");
        assertAnimoResult(E, "the E the C have Z \"γ\"\n");
    }

    @Test
    public void getFromPFlow_an_an_an() throws Exception {

        new Expression(
            _(THE._, "A", _(HAVE._, "Y", _(GET._, "X")))
        );

        new Expression(
            _(THE._, "B", _(HAVE._, "Z", _(GET._, "Y")))
        );

        new Expression(
            _(THE._, "C", _(GET._, "Z"))
        );

        Expression D = new Expression(
            _(THE._, "D", _(AN._, "C", _(AN._, "B", _(AN._, "A", _(HAVE._, "X", text("."))))))
        );
        //assertXMLResult(D, "<the:D><the:C><have:Z><have:Y><have:X>.</have:X></have:Y></have:Z></the:C></the:D>");
        assertAnimoResult(D, "the D the C have Z have Y have X \".\"\n");
    }
}