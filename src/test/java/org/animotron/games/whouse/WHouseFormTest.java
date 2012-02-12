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

import java.io.IOException;

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
            "the SKU " +
        		"(word " +
            		"(lang-en \"stock-keeping unit\") " +
    				"(lang-ru \"единица учета запасов\") " +
				") " +
				"(goods, qty, price, cost).",

			"the qty" +
        		"(word " +
        			"(lang-en \"quantity\") " +
        			"(lang-ru \"количество\") " +
				") " +
				"(/ (get cost) (get price))" +
				"(number, UoM).",

			"the price" +
				"(/ (get cost) (get qty)).",
				
			"the cost" +
				"(* (get qty) (get price))" +
				"(number, currency).",

            "the whouse-receive " +
        		"(word " +
            		"(lang-en \"ware house receive\") " +
    				"(lang-ru \"складской приход\") " +
				") " +
				"(receive-party, issue-party, (goods, qty, price, cost)).",

			"the receiptsForWhouse " +
			"(D2012-01-29)" +
			"(issue companyA) "+
			"(receive whouse) "+
			"(paper (qty 10,kg) (cost 50,USD)) "+
			"(pen (qty 3,box10) (cost 15,USD)). "

		);

	}

    @Test
    public void test_01() throws Exception {
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

                "the generate-form each (get prizm) (any form-widget).",

                "the html-form " +
                    "(form-widget)" +
                        "\\form (@name id this prizm)" +
                            "(each (get part) " +
                                "(ptrn (this part) " +
                                    "(?is table html-table) " +
                                    "(html-label-input))).",

                "the html-input \\input (@name id this part).",

                "the html-label-input \\label (word this part) (html-input).",

                "the html-table " +
                    "each (get row this part) "+
                        "(\\table (@name id this row) " +
                            "(\\tr each (get part this row) (\\th word this part)) " +
                            "(\\tr (@name \"uuid\") (each (get part this row) (\\td html-input))))."
        );

        assertAnimoResult(
                "generate-form prizm whouse-issue",
                "generate-form " +
                    "the html-form " +
                        "(form-widget) " +
                        "(\\form (@name \"whouse-issue\") " +
                            "(html-label-input " +
                                "\\label \"date\" (html-input \\input (@name \"date\"))) " +
                            "(html-label-input " +
                                "\\label \"issue\" (html-input \\input (@name \"issue-party\"))) " +
                            "(html-label-input " +
                                "\\label \"warehouse\" (html-input \\input (@name \"whouse-party\"))) " +
                            "(html-table " +
                                "\\table (@name \"SKU\")" +
                                    "(\\tr (\\th \"goods\") (\\th \"quantity\") (\\th \"price\") (\\th \"cost\")) " +
                                    "(\\tr (@name \"uuid\") " +
                                        "(\\td html-input \\input (@name \"goods\")) " +
                                        "(\\td html-input \\input (@name \"qty\")) " +
                                        "(\\td html-input \\input (@name \"price\")) " +
                                        "(\\td html-input \\input (@name \"cost\")))))."
        );
        
    }

    @Test
    public void test_02 () throws IOException {

        __(
                "the companyA (party) (word \"Company A & Co.\")",
                "the centralWhouse (party) (word \"Central warehouse\")",

                "the book (goods).",
                "the ISBN:0-387-97061-4 (book) (word \"Origins of Programming\")",

                "the pcs (UoM) (word \"pcs\")",

                "the EUR (currency) (word \"EUR\")",
                "the USD (currency) (word \"USD\")"

        );

        Expression doc = new JSONExpression(FACTORY.createJsonParser(
            "{" +
                "\"whouse-issue\" : null, " +
                "\"date\" : \"D2012-02-11\", " +
                "\"issue-party\" : {\"companyA\" : null}, " +
                "\"whouse-party\" : {\"centralWhouse\" : null}, " +
                "\"SKU\" : {" +
                    "\"uuidA\" : {" +
                        "\"goods\" : {\"ISBN:0-387-97061-4\" : null}, " +
                        "\"qty\" : {\"number\" : 1, \"UoM\" : {\"pcs\" : null}}, " +
                        "\"price\" : {\"number\" : 35, \"currency\" : {\"EUR\" : null}, \"UoM\" : {\"pcs\" : null}}, " +
                        "\"cost\" : {\"number\" : 35, \"currency\" : {\"EUR\" : null}}" +
                    "}, " +
                    "\"uuidB\" : {" +
                        "\"goods\" : {\"ISBN:0-387-97061-3\" : null}, " +
                        "\"qty\" : {\"number\" : 1, \"UoM\" : {\"pcs\" : null}}, " +
                        "\"price\" : {\"number\" : 60, \"currency\" : {\"USD\" : null}, \"UoM\" : {\"pcs\" : null}}, " +
                        "\"cost\" : {\"number\" : 60, \"currency\" : {\"USD\" : null}}" +
                    "}" +
                "}" +
            "}"
        ), "docA");

        assertAnimo(
                doc,
                "the docA (whouse-issue) " +
                    "(date \"D2012-02-11\") " +
                    "(issue-party companyA) " +
                    "(whouse-party centralWhouse) " +
                    "(SKU " +
                        "(uuidA " +
                        	"(goods ISBN:0-387-97061-4) " +
                        	"(qty (number 1) (UoM pcs)) " +
                        	"(price (number 35) (currency EUR) (UoM pcs)) " +
                        	"(cost (number 35) (currency EUR))) " +
                        "(uuidB " +
                        	"(goods ISBN:0-387-97061-3) " +
                        	"(qty (number 1) (UoM pcs)) " +
                        	"(price (number 60) (currency USD) (UoM pcs)) " +
                        	"(cost (number 60) (currency USD))))."
        );
        
        assertAnimoResult(
    		"get goods all centralWhouse,whouse-issue with date \"D2012-02-11\"", 
    		"goods ISBN:0-387-97061-4 (book goods). goods ISBN:0-387-97061-4 (book goods). goods ISBN:0-387-97061-3.");

    }

}
