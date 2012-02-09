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
import org.junit.Test;

import static org.animotron.expression.AnimoExpression.__;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class WHouseFormTest extends ATest {

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
                "the goods word \"goods\".",
                "the qty word \"quantity\".",
                "the price word \"price\".",
                "the cost word \"cost\".",

                "the generate-form " +
                        "each (get prizm) " +
                            "(each (get part) " +
                                "(any ptrn (this part) " +
                                    "(?is table widget-table) " +
                                    "(widget-input))).",

                "the html-input (widget-input) (\\input (@id uuid) (@name id this part) (@data-prizm id this pizm)).",

                "the html-table (widget-table) " +
                    "(\\table " +
                        "(\\tr each (get part get row this part) (\\th word this part)) " +
                        "(\\tr each (get part get row this part) (\\td html-input)))."
        );

        assertAnimoResult(
                "generate-form prizm whouse-issue",
                "generate-form " +
                        "(the html-input (widget-input) " +
                        "(\\input @id \"issue-party\")) " +
                        "(the html-input (widget-input) " +
                        "(\\input @id \"whouse-party\")) " +
                        "(the html-table (widget-table) " +
                        "(\\table " +
                        "(\\tr " +
                        "(\\th \"goods\") " +
                        "(\\th \"quantity\") " +
                        "(\\th \"price\") " +
                        "(\\th \"cost\")) " +
                        "(\\tr " +
                        "(\\td \\input @id \"goods\") " +
                        "(\\td \\input @id \"qty\") " +
                        "(\\td \\input @id \"price\") " +
                        "(\\td \\input @id \"cost\"))))."
        );

    }

}
