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
import org.animotron.expression.JExpression;
import org.animotron.statement.compare.WITH;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Q;
import org.animotron.statement.operator.THE;
import org.animotron.statement.query.ALL;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;
import org.neo4j.graphdb.Relationship;

import static org.animotron.expression.AnimoExpression.__;
import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.value;
import static org.animotron.graph.RelationshipTypes.TRI;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class WHouseTest extends ATest {

	@Test
    @Ignore
	public void test_00() throws Exception {
		
		//party: person & organization
		// + receipt or issue
		new JExpression(
    		_(THE._, "party")
        );

		new JExpression(
    		_(THE._, "receipt-party", 
				_(AN._, "party"),
				_(AN._, "receipt")
		)	);

		new JExpression(
    		_(THE._, "issue-party", 
				_(AN._, "party"),
				_(AN._, "issue")
		)	);

		new JExpression(
    		_(THE._, "person", 
				_(AN._, "party")
		)	);

		new JExpression(
    		_(THE._, "organization", 
				_(AN._, "party")
		)	);

		new JExpression(
    		_(THE._, "ORG-01", 
				_(AN._, "organization")
		)	);

		new JExpression(
    		_(THE._, "ORG-02", 
				_(AN._, "organization")
		)	);

		new JExpression(
    		_(THE._, "I", 
				_(AN._, "person")
		)	);
		
		//unit of measure
		new JExpression(_(THE._, "UoM"));
		
		new JExpression(
    		_(THE._, "kilo", 
				_(AN._, "number", _(Q._, "N1000"))
		)	);

		//kg -> kilo + gramm
		new JExpression(
    		_(THE._, "gram", 
				_(AN._, "UoM")
		)	);

		new JExpression(
    		_(THE._, "kilogram", 
				_(AN._, "kilo"),
				_(AN._, "gram")
		)	);

		//currency
		new JExpression(
    		_(THE._, "USD", 
				_(AN._, "currency")
		)	);
		
		//Stock Keeping Unit
		new JExpression(
    		_(THE._, "SKU",
				_(AN._, "reference"),
				_(AN._, "qty"),
				_(AN._, "price"),
				_(AN._, "cost")
		)	);

		//documents structure
		new JExpression(
    		_(THE._, "document", _(AN._, "date"))
        );

        new JExpression(
    		_(THE._, "whouse-document", 
				_(AN._, "document"),
				_(AN._, "issue-party"),
				_(AN._, "receipt-party"),
				_(AN._, "SKU")
		)	);

        new JExpression(
    		_(THE._, "whouse-receipt", 
				_(AN._, "whouse-document"),
				//I do receipt
				_(AN._, "receipt")
		)	);

        new JExpression(
    		_(THE._, "whouse-issue", 
				_(AN._, "whouse-document"),
				//I do issue
				_(AN._, "issue")
		)	);

        new JExpression(
    		_(THE._, "whouse-transfer", 
				_(AN._, "whouse-document"),
				//I do receipt & issue
				_(AN._, "receipt"),
				_(AN._, "issue")
		)	);
        
        //documents
        new JExpression(
    		_(THE._, "R01", 
				_(AN._, "whouse-document"),
				_(AN._, "date", value("T2011-08-07")), //TODO: date instruction
				_(AN._, "issue-party", _(AN._, "ORG-01")),
				_(AN._, "receipt-party", _(AN._, "I")),
				_(AN._, "SKU",
					_(THE._, "item01", 
						_(AN._, "reference", value("item01")),
						_(AN._, "qty",
							_(AN._, "number", _(Q._, "N2")),
							_(AN._, "UoM", _(AN._, "KG")) //TODO: _(AN._, "UoM", KG))
						),
						_(AN._, "price", //
							_(AN._, "number", _(Q._, "N5")),
							_(AN._, "UoM", _(AN._, "G")),
							_(AN._, "currency", _(AN._, "USD"))
						)
				)	)
		)	);
        
        JExpression a = new JExpression(
    		_(THE._, "a", 
				_(ALL._, "whouse-receive",
					_(WITH._, "party", _(AN._, "I"))))
		);
        assertAnimoResult(a, "the a.");

        //TODO: how to answer "what do I have?" ("SKU") (answer "item01")
        //How may of "item01" I have?

        JExpression f = new JExpression(
    		_(THE._, "f", 
				_(AN._, "form", _(AN._, "R01")))
		);

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
	public void test_01() throws Exception {
        __(
            "the kilo number 1000.", //* 1000
            "the UoM.",
            "the gram.",
            "the kg (kilo, gram).", //the base unit of mass in the International System of Units

            "the measument1 qty (number 1000) (UoM gram)."
        );
        Expression e = new AnimoExpression("get qty (measument1) (UoM kg)."); //???
    	assertStringResult(e, "have qty (number 1) (UoM kg)");
	}

	public void test_02() throws Exception {
        __(
            "the SKU " +
        		"(word " +
            		"(lang-en \"stock-keeping unit\") " +
    				"(lang-ru \"единица учета запасов\") " +
				") " +
				"(part " +
					"(name)" +
					"(qty)" +
					"(price)" +
					"(cost)" +
			").",
			
			"the qty" +
        		"(word " +
        			"(lang-en \"quantity\") " +
        			"(lang-ru \"количество\") " +
				") " +
				"(/ (get cost) (get price))" +
				"(number, UoM).",

			"the price" +
				"(/ (get cost) (get qty))",
				
			"the cost" +
				"(* (get qty) (get price))" +
				"(number, currency)"
		);
    	assertAnimoResult("html-widget qty", "<label>quantity<input id=\"\" name=\"\" value=\"\"/></label>");
	}
}
