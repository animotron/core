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
package org.animotron.expression;

import org.animotron.exception.AnimoException;
import org.animotron.graph.builder.GraphBuilder;
import org.animotron.statement.operator.THE;
import org.animotron.statement.value.STREAM;

import java.io.IOException;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public abstract class AbstractBinaryExpression extends Expression {

    public AbstractBinaryExpression(GraphBuilder builder) throws Throwable {
        super(builder);
    }

    @Override
    public void build() throws Throwable {
        builder.start(THE._, id());
            builder._(STREAM._, stream());
            description();
        builder.end();
    }

    protected abstract void description() throws AnimoException, IOException;
    protected abstract String stream();

    protected String id() {
        return null;
    }

}
