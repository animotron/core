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
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package org.animotron.games.whouse;

import org.animotron.ATest;
import org.animotron.expression.JExpression;
import org.animotron.statement.compare.WITH;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Q;
import org.animotron.statement.operator.THE;
import org.animotron.statement.query.ALL;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.junit.Ignore;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class WHouseTest extends ATest {

	@Test
    @Ignore
	public void test() throws Exception {
		
		//party: person & organization
		// + receipt or issue
		new JExpression(
    		_(THE._, "party")
        );

		new JExpression(
    		_(THE._, "receipt-party", 
				_(IS._, "party"),
				_(IS._, "receipt")
		)	);

		new JExpression(
    		_(THE._, "issue-party", 
				_(IS._, "party"),
				_(IS._, "issue")
		)	);

		new JExpression(
    		_(THE._, "person", 
				_(IS._, "party")
		)	);

		new JExpression(
    		_(THE._, "organization", 
				_(IS._, "party")
		)	);

		new JExpression(
    		_(THE._, "ORG-01", 
				_(IS._, "organization")
		)	);

		new JExpression(
    		_(THE._, "ORG-02", 
				_(IS._, "organization")
		)	);

		new JExpression(
    		_(THE._, "I", 
				_(IS._, "person")
		)	);
		
		//unit of measure
		new JExpression(_(THE._, "UoM"));
		
		new JExpression(
    		_(THE._, "kilo", 
				_(HAVE._, "number", _(Q._, "N1000"))
		)	);

		//kg -> kilo + gramm
		new JExpression(
    		_(THE._, "gram", 
				_(IS._, "UoM")
		)	);

		new JExpression(
    		_(THE._, "kilogram", 
				_(IS._, "kilo"),
				_(IS._, "gram")
		)	);

		//currency
		new JExpression(
    		_(THE._, "USD", 
				_(IS._, "currency")
		)	);
		
		//Stock Keeping Unit
		new JExpression(
    		_(THE._, "SKU",
				_(HAVE._, "reference"),
				_(HAVE._, "qty"),
				_(HAVE._, "price"),
				_(HAVE._, "cost")
		)	);

		//documents structure
		new JExpression(
    		_(THE._, "document", _(HAVE._, "date"))
        );

        new JExpression(
    		_(THE._, "whouse-document", 
				_(IS._, "document"),
				_(HAVE._, "issue-party"),
				_(HAVE._, "receipt-party"),
				_(HAVE._, "SKU") 
		)	);

        new JExpression(
    		_(THE._, "whouse-receipt", 
				_(IS._, "whouse-document"),
				//I do receipt
				_(IS._, "receipt")
		)	);

        new JExpression(
    		_(THE._, "whouse-issue", 
				_(IS._, "whouse-document"),
				//I do issue
				_(IS._, "issue")
		)	);

        new JExpression(
    		_(THE._, "whouse-transfer", 
				_(IS._, "whouse-document"),
				//I do receipt & issue
				_(IS._, "receipt"),
				_(IS._, "issue")
		)	);
        
        //documents
        new JExpression(
    		_(THE._, "R01", 
				_(IS._, "whouse-document"),
				_(HAVE._, "date", text("T2011-08-07")), //TODO: date instruction
				_(HAVE._, "issue-party", _(AN._, "ORG-01")),
				_(HAVE._, "receipt-party", _(AN._, "I")),
				_(HAVE._, "SKU", 
					_(THE._, "item01", 
						_(HAVE._, "reference", text("item01")),
						_(HAVE._, "qty", 
							_(HAVE._, "number", _(Q._, "N2")),
							_(HAVE._, "UoM", _(AN._, "KG")) //TODO: _(HAVE._, "UoM", KG)) 
						),
						_(HAVE._, "price", //
							_(HAVE._, "number", _(Q._, "N5")),
							_(HAVE._, "UoM", _(AN._, "G")),
							_(HAVE._, "currency", _(AN._, "USD")) 
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
}
