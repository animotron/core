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
package org.animotron.synchro;

import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.Expression;
import org.animotron.graph.traverser.AnimoTraverser;
import org.jgroups.JChannel;
import org.jgroups.Message;
import org.jgroups.ReceiverAdapter;
import org.jgroups.View;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import static org.animotron.graph.AnimoGraph.startDB;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */

/**
 * It's important to set -Djava.net.preferIPv4Stack=true JVM argument
 * to run unit test 
 * 
 */
public class BinaryFormatTest extends ATest{

    private  void test(String exp) throws IOException {
        exp = exp.replace('\'', '"');
        Expression e = new AnimoExpression(exp);
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        AnimoTraverser._.traverse(new BinaryGraphHandler(baos), e);
        stop();
        start();
        ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
        assertAnimo(new BinaryAnimoExpression(bais), exp);
    }

    @Test
    public void test_00() throws IOException {
        test("def a (b) (c 'foo').");
    }

}