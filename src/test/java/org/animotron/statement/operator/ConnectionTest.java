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
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ConnectionTest extends ATest {
	
    @Test
    public void mimeType_usecase() throws Throwable {

        tAnimo("def mime-type.");
        tAnimo("def file (reference 'file') (extension after-last '.' (get path)) (any mime-type with extension get extension).");
        tAnimo("def fileA (^file) (path '/home/test.txt').");
        tAnimo("def value-plain (mime-type) (type 'text/plain') (extension 'txt' 'value').");

        assertAnimoResult("get reference fileA", "\"file\".");

        assertAnimoResult("get path fileA", "\"/home/test.txt\".");

        assertAnimoResult("get extension fileA", "\"txt\".");

        assertAnimoResult("get mime-type fileA", "value-plain (mime-type) (type) (extension).");

        assertAnimoResult("get type get mime-type fileA", "\"text/plain\".");
    }
	
    @Test
    public void mimeType_one_more_usecase() throws Throwable {

        tAnimo("def mime-type.");
        tAnimo("def file (reference 'file') (path1 'some.path.value') (path2 get path1) (extension1 after-last '.' (get path1)) (any mime-type with extension get extension1).");
        tAnimo("def fileA (^file) (path '/home/test.txt').");
        tAnimo("def value-plain (mime-type) (type 'text/plain') (extension 'txt' 'value').");

        assertAnimoResult("get path1 fileA", "\"some.path.value\".");

        assertAnimoResult("get path2 fileA", "\"some.path.value\".");

        assertAnimoResult("get extension1 fileA", "\"value\".");

        assertAnimoResult("get mime-type fileA", "value-plain (mime-type) (type) (extension).");// (mime-type) (type) (extension).");

        assertAnimoResult("get type get mime-type fileA", "\"text/plain\".");
    }

    @Test
    public void mimeType_parallel() throws Throwable {

        tAnimo("def mime-type.");
        tAnimo("def file (reference 'file') (path) (extension after-last '.' (get path)) (any mime-type with extension get extension).");
        tAnimo("def fileA (file) (path '/home/test.txt').");
        tAnimo("def value-plain (mime-type) (type 'text/plain') (extension 'txt' 'value').");

        assertAnimoResult(
    		"def test (get mime-type fileA) (get type)",
    		"test (value-plain)."
		);
    }
}