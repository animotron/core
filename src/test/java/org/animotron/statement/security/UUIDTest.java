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
package org.animotron.statement.security;

import junit.framework.Assert;
import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.animotron.graph.serializer.CachedSerializer;
import org.junit.Test;

import java.io.IOException;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class UUIDTest extends ATest {
    
    public void test(String expr) throws IOException {
        String res = CachedSerializer.STRING.serialize(new AnimoExpression(expr));
        Assert.assertEquals(res.replaceAll("[0-9a-f]", "x"), "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx");
    }

    @Test
    public void test_00() throws Throwable {
        test("uuid ");
    }

    @Test
    public void test_01() throws Throwable {
        test("uuid \"f0683653-3c4f-476e-9b32-5d070596c890\"");
    }
    
}