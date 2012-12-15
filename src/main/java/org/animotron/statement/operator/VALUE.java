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
package org.animotron.statement.operator;

import org.animotron.exception.AnimoException;
import org.animotron.expression.Expression;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.index.AbstractIndex;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.AbstractStatement;
import org.animotron.statement.instruction.Instruction;
import org.animotron.statement.string.LOWER_CASE;
import org.animotron.statement.string.UPPER_CASE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexManager;
import org.neo4j.index.bdbje.BerkeleyDbIndexImplementation;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.Stack;

import static org.animotron.graph.AnimoGraph.createNode;
import static org.animotron.graph.AnimoGraph.execute;
import static org.animotron.graph.Properties.VALUE;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class VALUE extends AbstractStatement implements Prepare {

    public final static VALUE _ = new VALUE();
    
    private Processor processor = null;

    private VALUE() { super("value"); }

    private AbstractIndex<Node> value = new AbstractIndex<Node>(name()) {
        @Override
        public void init(IndexManager index) {
            init(index.forNodes(name, BerkeleyDbIndexImplementation.DEFAULT_CONFIG));
        }
    };

    public void init(IndexManager index) {
        value.init(index);
        
        if (processor != null)
        	processor.thread.interrupt();
        
        processor = new Processor();
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

    @Override
    public Object reference(Relationship r) {
        return  reference(r.getEndNode());
    }

    public Object reference(Node n) {
        if (VALUE.has(n)) {
            return VALUE.get(n);
        } else {
            return null;
        }
    }

    public static Object value(Object o) {
        if (o instanceof String) {
            String s = (String) o;
            try {
                return Long.valueOf(s);
            } catch (NumberFormatException el) {
                try {
                    return Double.valueOf(s);
                } catch (NumberFormatException ed) {
                    if (Boolean.FALSE.toString().equals(s))
                        return Boolean.FALSE;
                    if (Boolean.TRUE.toString().equals(s))
                        return Boolean.TRUE;
                    return s;
                }
            }
        }
        return o;
    }

    public static Number number(Object o) {
        if (o instanceof Number) {
            return (Number)o;

        } else if (o instanceof String) {
            String s = (String) o;
            try {
                return Long.valueOf(s);
            } catch (NumberFormatException el) {
                try {
                    return Double.valueOf(s);
                } catch (NumberFormatException ed) {
                    if (Boolean.FALSE.toString().equals(s))
                        return BigDecimal.ZERO;
                    else if (Boolean.TRUE.toString().equals(s))
                        return BigDecimal.ONE;
                    else if (s.isEmpty())
                        return BigDecimal.ZERO;

                }
            }
        }
        throw new IllegalArgumentException("This is not a number '"+o+"'.");
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
    }
    
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
	        			Thread.sleep(50);
	        		} catch (Exception e) {}
	        	}

	        	v = stack.pop();
        		if (v == null) return;
        		
	           // process(v.getClosest());
        	}
		}
    
	    private void process(final Relationship r) {
	    	
	        Object o = reference(r);
	        if (o instanceof String) {
	            final String s = (String) o;
//	            System.out.println("-> "+Thread.currentThread()+" "+s);
	            if (s.length() > 1) {
	            	try {
	                    execute(new GraphOperation<Void>() {
	                        @Override
	                        public Void execute() throws Throwable {
	                            Relationship x = new Expression() {

                                    private Expression letter(final String uuid1, final Instruction c, final String uuid2) {
                                        return new Expression() {
                                            @Override
                                            public void build() throws Throwable {
                                                builder.start(DEF._, uuid1);
                                                    builder._(uuid1);
                                                    builder.start(AN._);
                                                        builder._(REF._, "letter");
                                                    builder.end();
                                                    builder.start(AN._);
                                                        builder._(REF._, c.name());
                                                        builder.start(AN._);
                                                            builder._(REF._, uuid2);
                                                        builder.end();
                                                    builder.end();
                                                builder.end();
                                            }
                                        };
                                    }

	                                private void step(final String value, final int i) throws AnimoException, IOException {
	                                    if (i >= 0) {
	                                        Relationship def;
                                            final String s = String.valueOf(value.charAt(i));
//	                                    	System.out.println("> "+Thread.currentThread()+" "+expression.charAt(i));
                                            if (!s.toLowerCase().equals(s.toUpperCase())) {
                                                Relationship l = __(letter(s.toLowerCase(), UPPER_CASE._, s.toUpperCase()));
                                                Relationship u = __(letter(s.toUpperCase(), LOWER_CASE._, s.toLowerCase()));
                                                def = s.equals(s.toLowerCase()) ? l : u;
                                            } else {
                                                def = new Expression() {
                                                    @Override
                                                    public void build() throws Throwable {
                                                        builder.start(DEF._);
                                                            builder._(s);
                                                        builder.end();
                                                    }
                                                };
	                                        }
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

    public static Expression expression(final Object o) {
        return new Expression() {
            @Override
            public void build() throws Throwable {
                builder._(o);
            }
        };
    }

    //used by tests
	public void waitToBeEmpty() {
		while (!processor.stack.isEmpty())
			try {
				Thread.sleep(100);
			} catch (Exception e) {
                e.printStackTrace();
			}
	}
}