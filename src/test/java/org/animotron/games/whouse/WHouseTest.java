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
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.Expression;
import org.junit.Ignore;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class WHouseTest extends ATest {

	@Test
    @Ignore
	public void test_00() throws Throwable {
		
		//party: person & organization
		// + receipt or issue
        tAnimo("def party.");

        tAnimo("def receipt-party (party) (receipty).");

        tAnimo("def issue-party (party) (issue).");

        tAnimo("def person party.");

        tAnimo("def organization party.");

        tAnimo("def ORG-1 organization.");

        tAnimo("def ORG-2 organization.");

        tAnimo("def I person.");

		//unit of measure
        tAnimo("def UoM.");

        tAnimo("def kilo number Q N1000.");

		//kg -> kilo + gramm
        tAnimo("def gram UoM.");

        tAnimo("def kilogram (kilo) (gram).");

		//currency
        tAnimo("def USD currency.");

		//Stock Keeping Unit
        tAnimo("def SKU (reference) (qty) (price) (cost).");

		//documents structure
        tAnimo("def document date.");

        tAnimo("def whouse-document (document) (issue-party) (receipt-party) (SKU).");

        tAnimo("def whouse-receipt (whouse-receipt) (receipt).");

        tAnimo("def whouse-issue (whouse-document) (issue).");

        tAnimo("def whouse-transfer (receipt) (issue).");

        //documents
        tAnimo("def R01 (whouse-document) (date 'T2011-08-07') (issue-party ORG1) (receipt-party I) (SKU item01).");
        tAnimo("def item01 reference 'item01'.");

        Expression a = tAnimo("def a all whouse-receive with party I.");
        assertAnimoResult(a, "a.");

        //TODO: how to answer "what do I have?" ("SKU") (answer "item01")
        //How may of "item01" I have?

        Expression f = tAnimo("def f form R01.");

        assertXMLResult(f, "<form id='R01'>" +
                "<input id='date' value='T2011-08-07'>07 August 2011</input>" +
                "<input id='issue-party' value='an:ORG-01'>Organization 01</input>" +
                "<input id='receipt-party' value='an:I'>I</input>" +
                "<table>" +
                "<head>" +
                "<col id='reference'>reference</col>" +
                "<col id='qty'>qty</col>" +
                "<col id='price'>price</col>" +
                "<head>" +
                "<row>" +
                "<col><input id='item01*reference' value='item01'>item01</input></col>" +
                "<col><input id='item01*qty' value='have:number Q:N2; have:UoM an:KG'>2 kg</input></col>" +
                "<col><input id='item01*price' value='have:number Q:N2; have:UoM an:KG'>5 USD per gram</input></col>" +
                "<row>" +
                "</table>" +
                "</form>");
	}

	@Test
	@Ignore
	public void test_01() throws Throwable {
        __(
            "def kilo number 1000.", //* 1000
            "def UoM.",
            "def gram.",
            "def kg (kilo, gram).", //the base unit of mass in the International System of Units

            "def measument1 qty (number 1000) (UoM gram)."
        );
        Expression e = new AnimoExpression("get qty (measument1) (UoM kg)."); //???
    	assertStringResult(e, "have qty (number 1) (UoM kg)");
	}

	public void test_02() throws Throwable {
        __(
            "def SKU " +
        		"(word " +
            		"(lang-en \"stock-keeping unit\") " +
    				"(lang-ru \"единица учета запасов\") " +
				") " +
				"(goods, qty, price, cost).",

			"def qty" +
        		"(word " +
        			"(lang-en \"quantity\") " +
        			"(lang-ru \"количество\") " +
				") " +
				"(/ (get cost) (get price))" +
				"(number, UoM).",

			"def price" +
				"(/ (get cost) (get qty)).",
				
			"def cost" +
				"(* (get qty) (get price))" +
				"(number, currency)."
		);
    	assertAnimoResult("html-widget qty", "<label>quantity<input id=\"\" name=\"\" value=\"\"/></label>");
    	
    	__(
            "def whouse-receive " +
        		"(word " +
            		"(lang-en \"ware house receive\") " +
    				"(lang-ru \"складской приход\") " +
				") " +
				"(receive-party, issue-party, (goods, qty, price, cost)).",

			"def receiptsForWhouse " +
			"(D2012-01-29)" +
			"(issue companyA) "+
			"(receive whouse) "+
			"(paper (qty 10,kg) (cost 50,USD)) "+
			"(pen (qty 3,box10) (cost 15,USD)). "
		);
    	assertAnimoResult("get price whouse,paper", "/ (5,USD) (kg).");
    	assertAnimoResult("get qty whouse,paper,D2012-01-30", "(10,kg).");

    	__(
			"def issueForWhouse " +
			"(D2012-01-30)" +
			"(issue whouse) "+
			"(paper (qty 1,kg))."
		);
    	assertAnimoResult("get price whouse,paper", "/ (5,USD) (kg).");
    	assertAnimoResult("get qty whouse,paper,D2012-01-31", "(9,kg).");
	}
}
