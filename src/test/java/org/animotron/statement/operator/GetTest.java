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
import org.animotron.expression.JExpression;
import org.animotron.statement.query.GET;
import org.animotron.statement.query.SELF;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.animotron.statement.string.AfterLast;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class GetTest extends ATest {
	
	@Test
	public void getOnManyAN() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(HAVE._, "Z", text("A")))
                ),
                new JExpression(
                        _(THE._, "B", _(HAVE._, "Z", text("B")))
                )
        );

    	JExpression d = new JExpression(
			_(THE._, "d", _(GET._, "Z", _(AN._, "A"), _(AN._, "B")))
		);
        assertAnimoResult(d, "the d (have Z \"A\") (have Z \"B\")");

	}

	@Test
	public void getFromPFlow_an_with_param() throws Exception {

    	JExpression.__(new JExpression(
                _(THE._, "A", _(HAVE._, "B", _(GET._, "C")))
        ));
    	JExpression D = new JExpression(
			_(THE._, "D", _(AN._, "A", _(HAVE._, "C", text("."))))
		);
        assertAnimoResult(D, "the D the A have B have C \".\"");

    	JExpression.__(new JExpression(
                _(THE._, "A1", _(GET._, "B1"))
        ));

    	JExpression d = new JExpression(
			_(THE._, "d", _(AN._, "A1", _(HAVE._, "B1", text("."))))
		);
        assertAnimoResult(d, "the d the A1 have B1 \".\"");

	}
	
	@Test
	public void getFromPFlow_cross_an_with_param() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(HAVE._, "B", _(GET._, "C")))
                ),
                new JExpression(
                        _(THE._, "D", _(HAVE._, "E", _(GET._, "B")))
                )
        );
    	
    	JExpression X = new JExpression(
			_(THE._, "X", _(AN._, "D", _(AN._, "A", _(HAVE._, "C", text(":")))))
		);
        assertAnimoResult(X, "the X the D have E have B have C \":\"");

	}

	@Test
	public void getFromPFlow_an_with_an() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(HAVE._, "B", _(GET._, "C")))
                ),
                new JExpression(
                        _(THE._, "D", _(HAVE._, "C", text(".")))
                )
        );

    	JExpression E = new JExpression(
			_(THE._, "E", _(AN._, "A", _(AN._, "D")))
		);
    	assertAnimoResult(E, "the E the A have B have C \".\"");

	}
	
	@Test
	public void getFromPFlow_an_with_more_an() throws Exception {

    	JExpression.__(
                new JExpression(
                        _(THE._, "A", _(HAVE._, "B", _(GET._, "C")))
                ),
                new JExpression(
                        _(THE._, "D", _(HAVE._, "C", text(".")))
                ),
                new JExpression(
                        _(THE._, "E", _(HAVE._, "C", text(":")))
                )
        );

    	JExpression F = new JExpression(
			_(THE._, "F", _(AN._, "A", _(AN._, "D"), _(AN._, "E", _(HAVE._, "C", text("_")))))
		);
    	assertAnimoResult(F, "the F the A have B (have C \".\") (have C \"_\")");

	}
	
    @Test
    public void checkIConIS() throws Exception {

        JExpression.__(

                new JExpression(
                        _(THE._, "A",
                                _(HAVE._, "A1", text("some.path")),

                                _(IC._, "A2",
                                        _(SELF._, "A1")),

                                _(IC._, "B1",
                                        _(AfterLast._,
                                                text("."),
                                                _(SELF._, "A1")))
                        )),

                new JExpression(
                        _(THE._, "B",
                                _(IS._, "A"),
                                _(HAVE._, "A1", text("test.txt"))
                        ))

        );

        JExpression C0 = new JExpression(
        _(THE._, "C0",
            _(GET._, "A1",
                _(AN._, "A")
        )));
        assertAnimoResult(C0, "the C0 have A1 \"some.path\"");

        JExpression C1 = new JExpression(
        _(THE._, "C1",
            _(GET._, "A1",
                _(AN._, "B")
        )));
        assertAnimoResult(C1, "the C1 have A1 \"test.txt\"");

        JExpression C2 = new JExpression(
        _(THE._, "C2",
            _(GET._, "A2",
                _(AN._, "B")
        )));
        assertAnimoResult(C2, "the C2 have A2 have A1 \"test.txt\"");

        JExpression C3 = new JExpression(
        _(THE._, "C3",
            _(GET._, "E1",
                _(AN._, "B")
        )));
        assertAnimoResult(C3, "the C3");

        JExpression C4 = new JExpression(
        _(THE._, "C4",
            _(GET._, "B1",
                _(AN._, "B")
        )));
        assertAnimoResult(C4, "the C4 have B1 \"txt\"");

    }

    @Test
    public void checkHAVEonIS() throws Exception {

        JExpression.__(

                new JExpression(
                        _(THE._, "A",
                                _(HAVE._, "A1", text("some.path")),

                                _(HAVE._, "A2",
                                        _(SELF._, "A1")),

                                _(HAVE._, "B1",
                                        _(AfterLast._,
                                                text("."),
                                                _(SELF._, "A1")))
                        )),

                new JExpression(
                        _(THE._, "B",
                                _(IS._, "A"),
                                _(HAVE._, "A1", text("test.txt"))
                        ))

        );

//        JExpression C0 = new JExpression(
//        _(THE._, "C0",
//            _(GET._, "A1",
//                _(AN._, "A")
//        )));
//        assertAnimoResult(C0, "the C0 have A1 \"some.path\"");
//
//        JExpression C1 = new JExpression(
//        _(THE._, "C1",
//            _(GET._, "A1",
//                _(AN._, "B")
//        )));
//        assertAnimoResult(C1, "the C1 have A1 \"test.txt\"");
//
//        JExpression C2 = new JExpression(
//        _(THE._, "C2",
//            _(GET._, "A2",
//                _(AN._, "B")
//        )));
//        assertAnimoResult(C2, "the C2 have A2 have A1 \"test.txt\"");
//
//        JExpression C3 = new JExpression(
//        _(THE._, "C3",
//            _(GET._, "E1",
//                _(AN._, "B")
//        )));
//        assertAnimoResult(C3, "the C3");

        JExpression C4 = new JExpression(
        _(THE._, "C4",
            _(GET._, "B1",
                _(AN._, "B")
        )));
        assertAnimoResult(C4, "the C4 have B1 \"txt\"");

    }

    @Test
    public void getFromPFlow_an_with_stack() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(GET._, "X"))
                ),
                new JExpression(
                        _(THE._, "B", _(GET._, "Y"), _(AN._, "A"))
                ),
                new JExpression(
                        _(THE._, "C", _(GET._, "Z"), _(AN._, "B"))
                )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(AN._, "C", _(HAVE._, "X", text("α")), _(HAVE._, "Y", text("β")), _(HAVE._, "Z", text("γ"))))
        );
        assertAnimoResult(E, "the E the C (have Z \"γ\") (the B (have Y \"β\") (the A have X \"α\"))");

    }

    @Test
    public void getFromPFlow_more_an_with_stack() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(GET._, "X"), _(HAVE._, "Z", text("γ")))
                ),
                new JExpression(
                        _(THE._, "B", _(GET._, "Y"), _(AN._, "A"))
                ),
                new JExpression(
                        _(THE._, "C", _(GET._, "Z"))
                )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(AN._, "C", _(AN._, "B", _(HAVE._, "B", text("β")))))
        );
        assertAnimoResult(E, "the E the C have Z \"γ\"");

    }

    @Test
    public void getFromPFlow_one_more_an_with_stack() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(GET._, "X"), _(HAVE._, "Z", text("γ")))
                ),
                new JExpression(
                        _(THE._, "B", _(GET._, "Y"))
                ),
                new JExpression(
                        _(THE._, "C", _(GET._, "Z"))
                )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(AN._, "C", _(AN._, "B", _(AN._, "A"))))
        );
        assertAnimoResult(E, "the E the C have Z \"γ\"");
    }

    @Test
    public void getFromPFlow_an_an_an() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(HAVE._, "Y", _(GET._, "X")))
                ),
                new JExpression(
                        _(THE._, "B", _(HAVE._, "Z", _(GET._, "Y")))
                ),
                new JExpression(
                        _(THE._, "C", _(GET._, "Z"))
                )
        );

        JExpression D = new JExpression(
            _(THE._, "D", _(AN._, "C", _(AN._, "B", _(AN._, "A", _(HAVE._, "X", text("."))))))
        );
        assertAnimoResult(D, "the D the C have Z have Y have X \".\"");

    }

}