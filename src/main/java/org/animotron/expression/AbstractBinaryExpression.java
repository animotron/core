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
import org.animotron.statement.operator.DEF;

import java.io.IOException;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class AbstractBinaryExpression extends AbstractExpression {

    public final static String MIME_TYPE = "mime-type";
    public final static String TYPE = "type";

    public AbstractBinaryExpression(GraphBuilder builder) {
        super(builder);
    }

    @Override
    public void build() throws Throwable {
        builder.start(DEF._, id());
            mime();
            description();
        builder.end();
    }

    protected abstract void description() throws AnimoException, IOException;
    protected abstract void mime() throws AnimoException, IOException;

    protected String id() {
        return null;
    }

}
