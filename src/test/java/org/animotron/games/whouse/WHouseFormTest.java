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
package org.animotron.games.whouse;

import org.animotron.ATest;
import org.animotron.expression.Expression;
import org.animotron.expression.JSONExpression;
import org.codehaus.jackson.JsonFactory;
import org.junit.Test;

import static org.animotron.expression.AnimoExpression.__;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class WHouseFormTest extends ATest {

    private static final JsonFactory FACTORY = new JsonFactory();

    @Test
    public void test_00() throws Exception {

        __(
                "the companyA (party) (word \"Company A & Co.\")",
                "the centralWhouse (party) (word \"Central warehouse\")",

                "the book (goods).",
                "the ISBN:0-387-97061-4 (book) (word \"Origins of Programming\")",
                "the ISBN:3-540-11157-3 (book) (word \"Algorithms in Modern Mathematics and Computer Science: Proceedings, Urgench, Uzbek SSR September 16-22, 1979 (Lecture Notes in Computer Science)\")",

                "the pcs (UoM) (word \"pcs\")",

                "the EUR (currency) (word \"EUR\")",
                "the USD (currency) (word \"USD\")"

        );

        Expression doc = new JSONExpression(FACTORY.createJsonParser(
            "{" +
                "\"event\" : null, " +
                "\"date\" : \"D2012-02-11\", " +
                "\"issue-party\" : {\"companyA\" : null}, " +
                "\"receive-party\" : {\"centralWhouse\" : null}, " +
                "\"SKU\" : {" +
                    "\"uuidA\" : {" +
                        "\"goods\" : {\"ISBN:0-387-97061-4\" : null}, " +
                        "\"qty\" : {\"number\" : 1, \"UoM\" : {\"pcs\" : null}}, " +
                        "\"price\" : {\"number\" : 35, \"currency\" : {\"EUR\" : null}, \"UoM\" : {\"pcs\" : null}}, " +
                        "\"cost\" : {\"number\" : 35, \"currency\" : {\"EUR\" : null}}" +
                    "}, " +
                    "\"uuidB\" : {" +
                        "\"goods\" : {\"ISBN:3-540-11157-3\" : null}, " +
                        "\"qty\" : {\"number\" : 1, \"UoM\" : {\"pcs\" : null}}, " +
                        "\"price\" : {\"number\" : 35, \"currency\" : {\"USD\" : null}, \"UoM\" : {\"pcs\" : null}}, " +
                        "\"cost\" : {\"number\" : 35, \"currency\" : {\"USD\" : null}}" +
                    "}" +
                "}" +
            "}"
        ), "docA");

        assertAnimo(
                doc,
                "the docA " +
                	"(event) " +
                    "(date \"D2012-02-11\") " +
                    "(issue-party companyA) " +
                    "(receive-party centralWhouse) " +
                    "(SKU " +
                        "(uuidA " +
                        	"(goods ISBN:0-387-97061-4) " +
                        	"(qty (number 1) (UoM pcs)) " +
                        	"(price (number 35) (currency EUR) (UoM pcs)) " +
                        	"(cost (number 35) (currency EUR))) " +
                        "(uuidB " +
                        	"(goods ISBN:3-540-11157-3) " +
                        	"(qty (number 1) (UoM pcs)) " +
                        	"(price (number 35) (currency USD) (UoM pcs)) " +
                        	"(cost (number 35) (currency USD))))."
        );

//        assertAnimoResult(
//    		"all centralWhouse with date \"D2012-02-11\"",
//    		"the docA (event) (date) (issue-party) (receive-party) (SKU).");
//
//        assertAnimoResult(
//    		"all centralWhouse,event with date \"D2012-02-11\"",
//    		"the docA (event) (date) (issue-party) (receive-party) (SKU).");
//
//        assertAnimoResult(
//    		"get SKU all centralWhouse,event with date \"D2012-02-11\"",
//    		"SKU (uuidA) (uuidB).");

        __(
    		"the person party.",
    		"the personA person.",

            "the docB " +
                	"(event) " +
                    "(date \"D2012-02-12\") " +
                    "(issue-party centralWhouse) " +
                    "(receive-party personA) " +
                    "(SKU " +
                        "(uuidA " +
                        	"(goods ISBN:0-387-97061-4) " +
                        	"(qty (number 1) (UoM pcs)) " +
                        	"(price (number 35) (currency EUR) (UoM pcs)) " +
                        	"(cost (number 35) (currency EUR)))" +
                	")."
		);

//        assertAnimoResult(
//    		"get SKU all centralWhouse,event between date (\"D2012-02-11\") (\"D2012-02-12\") ",
//    		"SKU (uuidA) (uuidB). SKU uuidA.");

        __(
                "the whouse-issue " +
                    "(word \"warehouse issue document\") " +
                    "(part (date) (issue-party) (whouse-party) (table row SKU)).",

                "the SKU part (goods) (qty) (price) (cost).",

                "the date word \"date\".",
                "the whouse-party word \"warehouse\".",
                "the issue-party word \"issue\".",
                "the goods word \"goods\".",
                "the qty word \"quantity\".",
                "the price word \"price\".",
                "the cost word \"cost\".",

                "the generate-form each (get prism) (any form-widget).",
                "the generate-table-row each (get row get prism) (any table-row-widget).",

                "the html-form " +
                    "(form-widget)" +
                    "(\\form (@name id this prism)" +
                        "(each (get part) " +
                            "(ptrn (this part) " +
                                "(?is table html-table) " +
                                "(html-label-input)))).",

                "the html-input \\input (@name id this part).",

                "the html-label-input \\label (word this part) (html-input).",

                "the html-table " +
                    "each (get row this part) "+
                        "(\\table (@name id this row) " +
                            "(html-table-head) (html-table-row)).",

                "the html-table-head \\tr each (get part this row) (\\th word this part).",

                "the html-table-row (table-row-widget) (\\tr (@name \"uuid\") (each (get part this row) (\\td html-input))).",

                "the fill-form " +
                    "(each (get prism) " +
                        "(each (get part this prism) " +
                            "(ptrn (this part) " +
                                "(?is table fill-form prism get row this part) " +
                                "(fill-input)))).",

                "the fill-input \\input " +
                    "(@name id this part) " //+
//                    "(each (get (this part) (this object)) " +
//                        "(@id id this this part)" +
//                        "(@value word this this part))."

        );

//        assertAnimoResult(
//                "generate-form prism whouse-issue",
//                "generate-form " +
//                    "the html-form " +
//                        "(form-widget) " +
//                        "(\\form (@name \"whouse-issue\") " +
//                            "(html-label-input " +
//                                "\\label \"date\" (html-input \\input @name \"date\")) " +
//                            "(html-label-input " +
//                                "\\label \"issue\" (html-input \\input @name \"issue-party\")) " +
//                            "(html-label-input " +
//                                "\\label \"warehouse\" (html-input \\input @name \"whouse-party\")) " +
//                            "(html-table " +
//                                "\\table (@name \"SKU\") " +
//                                    "(html-table-head \\tr (\\th \"goods\") (\\th \"quantity\") (\\th \"price\") (\\th \"cost\")) " +
//                                    "(html-table-row (table-row-widget) " +
//                                        "(\\tr (@name \"uuid\") " +
//                                            "(\\td html-input \\input @name \"goods\") " +
//                                            "(\\td html-input \\input @name \"qty\") " +
//                                            "(\\td html-input \\input @name \"price\") " +
//                                            "(\\td html-input \\input @name \"cost\")))))."
//        );
//
//        assertAnimoResult(
//                "generate-table-row prism whouse-issue",
//                "generate-table-row " +
//                    "the html-table-row (table-row-widget) " +
//                        "(\\tr (@name \"uuid\") " +
//                            "(\\td html-input \\input @name \"goods\") " +
//                            "(\\td html-input \\input @name \"qty\") " +
//                            "(\\td html-input \\input @name \"price\") " +
//                            "(\\td html-input \\input @name \"cost\"))."
//        );

        assertAnimoResult(
                "fill-form (prism whouse-issue) (object docA) ",
                "fill-form " +
                    "the html-table-row (table-row-widget) " +
                        "(\\tr (@name \"uuid\") " +
                            "(\\td html-input \\input @name \"goods\") " +
                            "(\\td html-input \\input @name \"qty\") " +
                            "(\\td html-input \\input @name \"price\") " +
                            "(\\td html-input \\input @name \"cost\"))."
        );

    }

}
