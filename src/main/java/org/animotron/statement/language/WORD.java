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
package org.animotron.statement.language;

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.index.Order;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.instruction.Instruction;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Prepare;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.index.IndexManager;
import org.neo4j.graphdb.index.RelationshipIndex;

/**
 * 'WORD' instruction.
 *
 *  @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public class WORD extends Instruction implements Evaluable, Prepare {

	public static final WORD _ = new WORD();
	
	RelationshipIndex words;
	
	private static final String NAME = "word"; 

	private WORD() { 
		super(NAME);
		
		IndexManager index = AnimoGraph.getDb().index();
		words = index.forRelationships( NAME );
	}


	@Override
	public OnQuestion onCalcQuestion() {
		return new Calc();
	}
	
	class Calc extends OnQuestion {
        @Override
        public void act(final PFlow pf) {
        	;
        }
	}

	@Override
	public OnQuestion onPrepareQuestion() {
		return new Prepare();
	}
	
	class Prepare extends OnQuestion {
		public boolean needAnswer() {
			return false;
		}

		@Override
    	public void act(final PFlow pf) {
			AnimoGraph.execute(new GraphOperation<Void>() {

				@Override
				public Void execute() throws Exception {
					IndexHits<Relationship> hits = Order.queryDown(pf.getOPNode());
					try {
						for (Relationship r : hits) {
							if (r.isType(VALUE._)) {
								words.add(r, NAME, VALUE._.reference(r));
							}
						}
					} catch (Exception e) {
						e.printStackTrace();
					} finally {
						hits.close();
					}
					return null;
				}
			});
		}
	}

	public IndexHits<Relationship> search(String word) {
		//System.out.println(words.get(NAME, word).getSingle());
		return words.query(NAME, word+"*");
	}
}