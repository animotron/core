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
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.REF;
import org.animotron.statement.operator.THE;
import org.codehaus.jackson.JsonParser;
import org.codehaus.jackson.JsonToken;

import static org.codehaus.jackson.JsonToken.END_ARRAY;
import static org.codehaus.jackson.JsonToken.END_OBJECT;
import static org.codehaus.jackson.JsonToken.FIELD_NAME;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class JSONExpression extends AbstractJSONExpression {

    public JSONExpression(JsonParser parser) {
        super(parser);
    }

    public JSONExpression(JsonParser parser, String name) {
        super(parser, name);
    }

    public JSONExpression(GraphBuilder builder, JsonParser parser) {
        super(builder, parser);
    }

    public JSONExpression(GraphBuilder builder, JsonParser parser, String name) {
        super(builder, parser, name);
    }

    @Override
    public void build() throws Throwable {
        int l = 0;
        JsonToken token, prev = null;
        builder.start(THE._, name);
        while((token = parser.nextToken()) != null) {
            switch (token) {
                case START_OBJECT           :
                case START_ARRAY            : prev = token;
                                              break;
                case FIELD_NAME             : if (l > 0 && (prev == FIELD_NAME || prev == END_OBJECT || prev == END_ARRAY)) {
                                                builder.end();
                                                l--;
                                              }
                                              builder.start(AN._);
                                              builder._(REF._, parser.getCurrentName());
                                              prev = token;
                                              l++;
                                              break;
                case END_OBJECT             :
                case END_ARRAY              : if (l > 0 && (prev == FIELD_NAME || prev == END_OBJECT || prev == END_ARRAY)) {
                                                builder.end();
                                                l--;
                                              }
                                              prev = token;
                                              break;
                case VALUE_NUMBER_FLOAT     : builder._(parser.getDoubleValue());
                                              break;
                case VALUE_NUMBER_INT       : builder._(parser.getLongValue());
                                              break;
                case VALUE_STRING           : builder._(parser.getText());
                                              break;
                case VALUE_FALSE            :
                case VALUE_TRUE             : builder._(parser.getBooleanValue());
                                              break;
                default                     :
            }
        }
        for (int i = 0; i < l; i++) {
            builder.end();
        }
        builder.end();
    }

}
