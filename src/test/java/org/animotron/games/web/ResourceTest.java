/*
 *  Copyright (C) 2011-2013 The Animo Project
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
package org.animotron.games.web;

import org.animotron.ATest;
import org.animotron.expression.Expression;
import org.junit.Test;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class ResourceTest extends ATest {

    @Test
    public void test1() throws Throwable {

        tAnimo("def service ^resource.");
        tAnimo("def root-service (service) (root) (title 'root').");
        tAnimo("def not-found-service (service) (not-found) (title 404).");
        tAnimo("def rest any resource.");

        Expression s = tAnimo("def s rest use root.");

        //assertAnimoResult(s, "s rest root-service (service resource) (root) (title \"root\").");
        assertAnimoResult(s, "s rest root-service (service resource) (root) (title).");
    }

    @Test
    public void test2() throws Throwable {

        tAnimo("def service ^resource.");
        tAnimo("def root-service (service) (root) (title 'root').");
        tAnimo("def root-service1 (service) (root) (title 'root1').");
        tAnimo("def not-found-service (service) (not-found) (title 404).");
        tAnimo("def rest all resource.");

        Expression s = tAnimo("def s rest use root.");

        //assertAnimoResult(s, "s rest (root-service (service resource) (root) (title \"root\")) (root-service1 (service resource) (root) (title \"root1\")).");
        assertAnimoResult(s, "s rest (root-service (service resource) (root) (title)) (root-service1 (service resource) (root) (title)).");

    }

    @Test
    public void test3() throws Throwable {

        tAnimo("def service ^resource.");
        tAnimo("def root-service (^service) (root) (title 'root').");
        tAnimo("def root-service1 (root-service) (title 'root1').");
        tAnimo("def not-found-service (service) (not-found) (title 404).");
        tAnimo("def rest all resource.");

        Expression s = tAnimo("def s rest use root.");

        //assertAnimoResult(s, "s rest (root-service (service resource) (root) (title \"root\")) (root-service1 (root-service (service resource) (root) (title \"root\")) (title)) (title \"root1\")).");
        //assertAnimoResult(s, "s rest (root-service (service resource) (root) (title)) (root-service1 (root-service (service resource) (root) (title)) (title)).");
        assertAnimoResult(s, "s rest " +
        		"(root-service (service resource) (root) (title)) " +
        		"(root-service1 (root-service (service resource) (root) (title)) (title)).");

    }

    @Test
    public void test4() throws Throwable {

        tAnimo("def root-service (service) (root) (title 'root').");
        tAnimo("def not-found-service (service) (not-found) (title 404).");
        tAnimo("def rest any service.");

        Expression s = tAnimo("def s rest use root.");

        //assertAnimoResult(s, "s rest root-service (service) (root) (title \"root\").");
        assertAnimoResult(s, "s rest root-service (service) (root) (title).");
    }

    @Test
    public void test5() throws Throwable {

        tAnimo("def root-service (service) (root) (title 'root').");
        tAnimo("def root-service1 (service) (root) (title 'root1').");
        tAnimo("def not-found-service (service) (not-found) (title 404).");
        tAnimo("def rest all service.");

        Expression s = tAnimo("def s rest use root.");

        //assertAnimoResult(s, "s rest (root-service (service) (root) (title \"root\")) (root-service1 (service) (root) (title \"root1\")).");
        assertAnimoResult(s, "s rest (root-service (service) (root) (title)) (root-service1 (service) (root) (title)).");
    }

    @Test
    public void test6() throws Throwable {

        tAnimo("def root-service (^service) (root) (title 'root').");
        tAnimo("def root-service1 (root-service) (title 'root1').");
        tAnimo("def not-found-service (service) (not-found) (title 404).");
        tAnimo("def rest all service.");

        Expression s = tAnimo("def s rest use root.");

        //assertAnimoResult(s, "s rest (root-service (service) (root) (title \"root\")) (root-service1 (root-service) (title \"root1\")).");
        //assertAnimoResult(s, "s rest (root-service (service) (root) (title)) (root-service1 (root-service (service) (root) (title)) (title)).");
        assertAnimoResult(s, "s rest " +
    		"(root-service (service) (root) (title)) " +
    		"(root-service1 (root-service (service) (root) (title)) (title)).");
    }
}