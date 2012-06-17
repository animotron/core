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

//import org.codehaus.jackson.JsonFactory;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class WHouseFormTest extends ATest {

    private static final JsonFactory FACTORY = new JsonFactory();

    @Test
    public void test_00() throws Throwable {

        __(
                "def companyA (party) (word \"Company A & Co.\")",
                "def centralWhouse (party) (word \"Central warehouse\")",

                "def book (goods).",
                "def ISBN:0-387-97061-4 (book) (word \"Origins of Programming\")",
                "def ISBN:3-540-11157-3 (book) (word \"Algorithms in Modern Mathematics and Computer Science: Proceedings, Urgench, Uzbek SSR September 16-22, 1979 (Lecture Notes in Computer Science)\")",

                "def pcs (UoM) (word \"pcs\")",

                "def EUR (currency) (word \"EUR\")",
                "def USD (currency) (word \"USD\")"

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
                "def docA " +
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

        assertAnimoResult(
    		"all event (with party centralWhouse) (with date \"D2012-02-11\")",
    		"def docA (event) (date) (issue-party) (receive-party) (SKU).");

        assertAnimoResult(
       		"all event (with receive-party centralWhouse) (with date \"D2012-02-11\")",
    		"def docA (event) (date) (issue-party) (receive-party) (SKU).");

//        assertAnimoResult(
//           		"all event (with centralWhouse) (with date \"D2012-02-11\")",
//        		"def docA (event) (date) (issue-party) (receive-party) (SKU).");

        assertAnimoResult(
    		"get SKU all event (with receive-party centralWhouse) (with date \"D2012-02-11\")",
    		"uuidA. uuidB.");

        __(
    		"def person party.",
    		"def personA person.",

            "def docB " +
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

        assertAnimoResult(
    		"get SKU all event (with party centralWhouse) (between date (\"D2012-02-11\") (\"D2012-02-12\")) ",
    		"uuidA. uuidB. uuidA.");

        __(
                "def whouse-issue " +
                    "(word \"warehouse issue document\") " +
                    "(part (date) (issue-party) (receive-party) (table row SKU)).",

                "def SKU part (goods) (qty) (price) (cost).",

                "def date word \"date\".",
                "def receice-party (word \"receiver\") (party,receive).",
                "def issue-party (word \"issue\") (party,issue).",
                "def goods word \"goods\".",
                "def qty (word \"quantity\") (part (number) (UoM)).",
                "def price (word \"price\") (part (number) (currency) (UoM)).",
                "def cost (word \"cost\") (part (number) (currency)).",

                "def generate-form each (get prism) (any form-widget).",
                "def generate-table-row each (get row get prism) (any table-row-widget).",

                "def html-form " +
                    "(form-widget)" +
                    "(\\form (@name id this prism)" +
                        "(each (get part) " +
                            "(ptrn (this part) " +
                                "(?is table html-table) " +
                                "(html-label-input)))).",

                "def html-input \\input (@name id this part).",

                "def html-label-input \\label (word this part) (html-input).",

                "def html-table " +
                    "each (get row this part) "+
                        "(\\table (@name id this row) " +
                            "(html-table-head) (html-table-row)).",

                "def html-table-head \\tr each (get part this row) (\\th word this part).",

                "def html-table-row (table-row-widget) (\\tr (@name \"uuid\") (each (get part this row) (\\td html-input))).",

                "def fill-form " +
                    "each (get prism) " +
                        "(each (get part this prism) " +
                            "(ptrn (this part) " +
                                "(?is table fill-table) " +
                                "(fill-input))).",

                "def fill-table " +
                    "each (get row this part) " +
                        "(fill-form prizm this row).",

                "def fill-input \\input " +
                    "(@name id this part) " +
                    "(each (get (this part) (this object)) " +
                        "(@id id this this part)" +
                        "(@value word this this part))."

        );

        assertAnimoResult(
                "generate-form prism whouse-issue",
                "generate-form " +
                    "def html-form " +
                        "(form-widget) " +
                        "(\\form (@name \"whouse-issue\") " +
                            "(html-label-input " +
                                "\\label \"date\" (html-input \\input @name \"date\")) " +
                            "(html-label-input " +
                                "\\label \"issue\" (html-input \\input @name \"issue-party\")) " +
                            "(html-label-input " +
                                "\\label \"warehouse\" (html-input \\input @name \"whouse-party\")) " +
                            "(html-table " +
                                "\\table (@name \"SKU\") " +
                                    "(html-table-head \\tr (\\th \"goods\") (\\th \"quantity\") (\\th \"price\") (\\th \"cost\")) " +
                                    "(html-table-row (table-row-widget) " +
                                        "(\\tr (@name \"uuid\") " +
                                            "(\\td html-input \\input @name \"goods\") " +
                                            "(\\td html-input \\input @name \"qty\") " +
                                            "(\\td html-input \\input @name \"price\") " +
                                            "(\\td html-input \\input @name \"cost\")))))."
        );

        assertAnimoResult(
                "generate-table-row prism whouse-issue",
                "generate-table-row " +
                    "def html-table-row (table-row-widget) " +
                        "(\\tr (@name \"uuid\") " +
                            "(\\td html-input \\input @name \"goods\") " +
                            "(\\td html-input \\input @name \"qty\") " +
                            "(\\td html-input \\input @name \"price\") " +
                            "(\\td html-input \\input @name \"cost\"))."
        );

        assertAnimoResult(
                "fill-form (prism whouse-issue) ",// (object docA) ",
                "fill-form " +
                    "def html-table-row (table-row-widget) " +
                        "(\\tr (@name \"uuid\") " +
                            "(\\td html-input \\input @name \"goods\") " +
                            "(\\td html-input \\input @name \"qty\") " +
                            "(\\td html-input \\input @name \"price\") " +
                            "(\\td html-input \\input @name \"cost\"))."
        );

    }

    @Test
    public void test_01() throws Throwable {
    	__(
			"def form part field.",
			"def generator " +
				"\\form " +
					"(@id id this generator) " +
					"(each (get part this generator) (\\input @name id this part))"
		);
        assertAnimoResult("generator form", "generator \\form (@id \"form\") (\\input @name \"field\").");
        
    	__(
			"def form2 part (field1) (field2)."
		);
        assertAnimoResult(
    		"generator form2", 
    		"generator \\form (@id \"form2\") (\\input @name \"field1\") (\\input @name \"field2\").");

    }
}
