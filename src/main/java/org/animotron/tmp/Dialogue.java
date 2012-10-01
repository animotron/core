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
package org.animotron.tmp;

import javolution.util.FastList;
import org.animotron.io.PipedOutput;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Dialogue implements Runnable {
	
	//private InputStream in;
	private OutputStream out;
	
    private Reader reader;
    //private Writer writer;
    
    private PipedOutput<Relationship> leaned;

    public Dialogue(Reader in, PipedOutput<Relationship> leaned) {
    	//this.in = null;
    	this.out = null;
    	
    	reader = in;
    	//writer = null;

    	this.leaned = leaned;
    }

    public Dialogue(Reader in, OutputStream out) {
    	//this.in = null;
    	this.out = out;
    	
    	reader = in;
    	//writer = null;

    	this.leaned = null;
    }

    public Dialogue(InputStream in, OutputStream out) {
    	//this.in = in;
    	this.out = out;
    	
    	reader = new InputStreamReader(in);
    	//writer = new PrintWriter(out);
    }
    
    private StringBuilder s = new StringBuilder();

    public void run() {
        int len;
        char[] buff = new char[1024];
        
        String word = null;
        
        List<Relationship> token = null;

        try {
			while ((len=reader.read(buff))>0) {
			    for (int i = 0; i < len; i++) {
			        char ch = buff[i];
			        
			        if (ch == '\n') {
			        	if (token == null || token.size() == 0) {
			        		//learn
			        		
							word = s.toString();
							System.out.println("final checking "+word);

							if (token == null)
			        			token = new FastList<Relationship>();
				        		
//			        		token.add(expr);

			        	} else {
			        		if (leaned != null) {
			        			for (Relationship r : token) {
			        				leaned.write(r);
			        			}
			        		}
			        	}
			        	
		        		if (out != null)  {
		        			//answer
		        		}
		        			
			        } else {
			        	if (s.length() == 0 && ch == ' ')
			        		;
			        	else {
				        	s.append(ch);
							word = s.toString();
		
					        token = check(word);
					        if (token != null && token.size() > 0) {
					        	s = new StringBuilder();
					        }
			        	}
			        }
			    }

			}
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			if (leaned != null)
				try {
					leaned.close();
				} catch (IOException e) {}
			if (out != null)
				try {
					out.close();
				} catch (IOException e) {}
		}
    	
    }

	private List<Relationship> check(String word) {
		
		List<Relationship> nodes = null;

		Node n = VALUE._.get(word);
		if (n != null) {
			nodes = new ArrayList<Relationship>();
			
			for (Relationship r : n.getRelationships(VALUE._, Direction.INCOMING))
				nodes.add(r);
			System.out.println("found '"+word+"'");
		}
		
		return nodes;
	}
}
