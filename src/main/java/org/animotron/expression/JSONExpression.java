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

import org.animotron.graph.builder.GraphBuilder;
import org.animotron.statement.link.LINK;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.REF;
import org.codehaus.jackson.JsonParser;
import org.codehaus.jackson.JsonToken;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class JSONExpression extends AbstractJSONExpression {

    public JSONExpression(JsonParser parser) {
        super(parser);
    }

    public JSONExpression(GraphBuilder builder, JsonParser parser) {
        super(builder, parser);
    }

    @Override
    public void build() throws Exception {
        JsonToken token;
        while((token = parser.nextToken()) != null) {
            switch (token) {
                case START_ARRAY  : builder.start(LINK._);
                                    break;
                case START_OBJECT : builder.start(AN._);
                                    builder._(REF._, parser.getCurrentName());
                                    break;
                case END_ARRAY    :
                case END_OBJECT   : builder.end();
                                    break;
                default           : builder._(parser.getText());
            }
        }
    }

}
