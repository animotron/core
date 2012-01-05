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

import static org.animotron.graph.AnimoGraph.startDB;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;

import org.animotron.expression.AnimoExpression;
import org.animotron.graph.serializer.CachedSerializer;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Shell {

	public static void main( String[] args ) {

        startDB("data");

		boolean pretty = true;
		String argument;

		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		
		PrintWriter writer = new PrintWriter(System.out); 
		PrintWriter error = new PrintWriter(System.err); 

		while (true) {
			writer.print("> ");
			writer.flush();
	        try {
	        	argument = in.readLine();
	        } catch (Exception e) {
	        	e.printStackTrace();
	        	return;
			}
			
			if ("quit".equals(argument)) break;
			
	        try {
	        	AnimoExpression expression = new AnimoExpression(argument);
			
				String result = pretty ? CachedSerializer.PRETTY_ANIMO_RESULT.serialize(expression) : CachedSerializer.ANIMO_RESULT.serialize(expression);
				writer.write(result);
				writer.flush();
			} catch (Exception e) {
				e.printStackTrace();
				error.write(e.getMessage());
				error.flush();
			}
		}
	}
}