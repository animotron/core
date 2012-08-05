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
package org.animotron.statement.value;

import org.animotron.exception.AnimoException;
import org.animotron.expression.AbstractExpression;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.builder.FastGraphBuilder;
import org.animotron.graph.index.AbstractIndex;
import org.animotron.io.Pipe;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.DEF;
import org.animotron.statement.operator.Prepare;
import org.animotron.statement.operator.REF;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexManager;
import org.neo4j.index.bdbje.BerkeleyDbIndexImplementation;

import java.io.IOException;
import java.util.Stack;

import static org.animotron.graph.AnimoGraph.createNode;
import static org.animotron.graph.AnimoGraph.execute;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class VALUE extends AbstractValue implements Prepare {

    public final static VALUE _ = new VALUE();
    
    private Processor processor = new Processor();

    private VALUE() { super("value"); }

    protected VALUE(String... name) { super(name); }

    private AbstractIndex<Node> value = new AbstractIndex<Node>(name()) {
        @Override
        public void init(IndexManager index) {
            init(index.forNodes(name, BerkeleyDbIndexImplementation.DEFAULT_CONFIG));
        }
    };

    public void init(IndexManager index) {
        value.init(index);
	}

	public void add(Node n, Object reference) {
        value.add(n, reference);
	}

	public Node get(Object name) {
        return value.get(name);
	}

    @Override
    protected Node createChild(Object reference, boolean ready, boolean ignoreNotFound) throws AnimoException {
        Node child = super.createChild(reference, ready, ignoreNotFound);
        add(child, reference);
        return child;
    }
    
    public void shutdown() throws IOException, InterruptedException {
    	processor.toProcess(null);
    	
		processor.thread.join();
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
        public void act(PFlow pf) throws Throwable {
    		processor.toProcess(pf.getVector());
        }
    };
    
    class Processor implements Runnable {
    	
//    	Pipe pipe = Pipe.newInstance();
    	Stack<QCAVector> stack = new Stack<QCAVector>();
    	
    	Thread thread;
    	
    	public Processor() {
    		thread = new Thread(this);
    		thread.start();
		}
    	
    	public void toProcess(QCAVector v) throws IOException {
    		//XXX: put mark here?
//    		pipe.write(v);
    		
    		stack.add(v);
    	}

		@Override
		public void run() {
        	QCAVector v;
//        	while ((v = pipe.take()) != null) {

        	while (true) {
	        	while (stack.empty()) {
	        		try {
	        			Thread.sleep(500);
	        		} catch (Exception e) {}
	        	}

	        	v = stack.pop();
        		if (v == null) return;
        		
	            process(v.getClosest());
        	}
		}
    
	    private void process(final Relationship r) {
	    	
	        Object o = reference(r);
	        if (o instanceof String) {
	            final String s = (String) o;
	            System.out.println("-> "+Thread.currentThread()+" "+s);
	            if (s.length() > 1) {
	            	try {
	                    execute(new GraphOperation<Void>() {
	                        @Override
	                        public Void execute() throws Throwable {
	                            Relationship x = new AbstractExpression(new FastGraphBuilder()) {
	
	                                private void step(final String value, final int i) throws AnimoException, IOException {
	                                    if (i >= 0) {
	                                    	System.out.println("> "+Thread.currentThread()+" "+value.charAt(i));
	                                        Relationship def = new AbstractExpression(new FastGraphBuilder()) {
	                                            @Override
	                                            public void build() throws Throwable {
	                                                builder.start(DEF._);
	                                                    builder._(String.valueOf(value.charAt(i)));
	                                                builder.end();
	                                            }
	                                        };
	                                        builder.start(AN._);
	                                        	try {
	                                        		builder._(REF._, def.getEndNode());
	                                        	} catch (Exception e) {
	                                        		System.out.println(""+Thread.currentThread()+" "+e.getMessage());
	                                        		e.printStackTrace();
												}
	                                            step(value, i-1);
	                                        builder.end();
	                                    }
	                                }
	
	                                @Override
	                                public void build() throws Throwable {
	                                    builder.start(DEF._);
	                                        step(s, s.length()-1);
	                                    builder.end();
	                                }
	
	                            };
	
	                            Node proxy = createNode();
	
	                            r.getEndNode().createRelationshipTo(proxy, AN._);
	                            proxy.createRelationshipTo(x.getEndNode(), REF._);
	
	                            return null;
	                        }
	                    });
	            	} catch (Throwable e) {
						// TODO: handle exception
	            		e.printStackTrace();
					}
	            }
	        }
	    }
    }
}