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

import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.Expression;
import org.junit.Ignore;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ConnectionTest extends ATest {
	
    @Test
    public void mimeType_usecase() throws Throwable {

        tAnimo("def mime-type.");
        tAnimo("def file (reference 'file') (path) (extension after-last '.' get path) (any mime-type with extension get extension).");
        tAnimo("def fileA (^file) (path '/home/test.txt').");
        tAnimo("def value-plain (mime-type) (type 'text/plain') (extension 'txt' 'value').");

        Expression test;
        
        test = new AnimoExpression("get reference fileA");
        assertAnimoResult(test, "\"file\".");

        test = new AnimoExpression("get path fileA");
        assertAnimoResult(test, "\"/home/test.txt\".");

        test = new AnimoExpression("get extension fileA");
        assertAnimoResult(test, "\"txt\".");

        test = new AnimoExpression("get mime-type fileA");
        //XXX: assertAnimoResult(D, "D mime-type the value-plain (mime-type) (type \"value/plain\") (extension \"txt\" \"value\").");
        //assertAnimoResult(test, "mime-type the value-plain (mime-type) (type) (extension).");
        assertAnimoResult(test, "value-plain (mime-type) (type) (extension).");

        test = new AnimoExpression("get type get mime-type fileA");
        assertAnimoResult(test, "\"value/plain\".");
    }
	
    @Test
    public void mimeType_one_more_usecase() throws Throwable {

        tAnimo("def mime-type.");
        tAnimo("def file (reference 'file') (path1 'some.path.value') (path2 get path1) (extension1 after-last '.' get path1) (any mime-type with extension get extension1).");
        tAnimo("def fileA (^file) (path '/home/test.txt').");
        tAnimo("def value-plain (mime-type) (type 'text/plain') (extension 'txt' 'value').");

        Expression test;

//        test = new AnimoExpression("get path1 fileA");
//        assertAnimoResult(test, "\"some.path.value\".");
//
//        test = new AnimoExpression("get path2 fileA");
//        assertAnimoResult(test, "\"some.path.value\".");
//
//        test = new AnimoExpression("get extension1 fileA");
//        assertAnimoResult(test, "\"value\".");

        test = new AnimoExpression("get mime-type fileA");
        //XXX: assertAnimoResult(test, "value-plain (mime-type) (type \"value/plain\") (extension \"txt\" \"value\").");
        assertAnimoResult(test, "value-plain (mime-type) (type) (extension).");

        test = new AnimoExpression("get type get mime-type fileA");
        assertAnimoResult(test, "\"value/plain\".");
    }

    @Test
    @Ignore
    public void mimeType_parallel() throws Throwable {

        tAnimo("def mime-type.");
        tAnimo("def file (reference 'file') (path) (extension after-last '.' get path) (any mime-type with extension get extension).");
        tAnimo("def fileA (file) (path '/home/test.txt').");
        tAnimo("def value-plain (mime-type) (type 'text/plain') (extension 'txt' 'value').");

        Expression test;

        test = new AnimoExpression("def test (get mime-type fileA) (get type)");
        assertAnimoResult(test, "test (mime-type the value-plain (mime-type) (type) (extension)) (type \"value/plain\").");
    }
}