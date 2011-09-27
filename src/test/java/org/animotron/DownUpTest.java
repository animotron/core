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
package org.animotron;

import org.animotron.exception.AnimoException;
import org.animotron.expression.JExpression;
import org.animotron.statement.operator.THE;
import org.animotron.statement.query.ALL;
import org.animotron.statement.query.GET;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.junit.Ignore;
import org.junit.Test;

import java.io.IOException;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class DownUpTest extends ATest {
	
	private void common() throws IOException {
		new JExpression(
				_(THE._, "B", 
					_(IS._, "A"), 
					_(HAVE._, "X", text("B")))
			);

	    	new JExpression(
				_(THE._, "C", 
					_(IS._, "A"), 
					_(HAVE._, "X", text("C")))
			);
	}

	@Test
	public void up() throws AnimoException, IOException {
    	
		common();

    	JExpression a = new JExpression(
			_(THE._, "a", 
				_(ALL._, "A",
					text("bla "), _(GET._, "X"))
			) 
		);
    	
    	assertAnimoResult(a, "the a (the B (is A) (have X \"B\")) (the C (is A) (have X \"C\"))");
	}
	
	@Test
    @Ignore
    public void down() throws AnimoException, IOException {
    	
		common();

    	JExpression a = new JExpression(
			_(THE._, "a", 
				_(ALL._, "A", //XXX: how to set mode?
					text("bla "), _(GET._, "X"))
					
					//another possible way
					//EACH._, text("bla "), _(GET._, "X")
			) 
		);
    	
    	assertAnimoResult(a, "the a (\"bla\") (have X \"B\") (\"bla\") (have X \"C\")");
	}
}
