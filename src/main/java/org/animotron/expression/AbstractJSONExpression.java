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

import org.animotron.graph.builder.FastGraphBuilder;
import org.animotron.graph.builder.GraphBuilder;
import org.codehaus.jackson.JsonParser;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class AbstractJSONExpression extends AbstractExpression {

    protected final String name;
    protected final JsonParser parser;

    public AbstractJSONExpression(JsonParser parser) throws Throwable {
        this(parser, null);
    }

    public AbstractJSONExpression(JsonParser parser, String name) throws Throwable {
        this(new FastGraphBuilder(), parser, name);
    }

    public AbstractJSONExpression(GraphBuilder builder, JsonParser parser) throws Throwable {
        this(builder, parser, null);
    }

    public AbstractJSONExpression(GraphBuilder builder, JsonParser parser, String name) throws Throwable {
        super(builder);
        this.name = name;
        this.parser = parser;
    }

}
