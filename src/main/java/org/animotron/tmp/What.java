/*
 *  Copyright (C) 2012 The Animo Project
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
package org.animotron.tmp;

import org.animotron.manipulator.OnQuestion;
import org.animotron.statement.instruction.DetermInstruction;
import org.animotron.utils.MessageDigester;

import static org.animotron.expression.AnimoExpression.__;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class What extends DetermInstruction {
	
	public final static What _ = new What();
	
	public What() {
		super("what");
		
		init();
	}
	
    public void init() {
		Brain brain = Brain.parse("what");
		
		if (brain.mentalState.isEmpty()) {
			String uuid = MessageDigester.uuid().toString();
			__("def "+uuid+" 'what'.", "def what "+uuid+".");
		}
    }
    
	@Override
	public OnQuestion onCalcQuestion() {
		System.out.println("WHAT");
		// TODO Auto-generated method stub
		return null;
	}
}