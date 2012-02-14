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
package org.animotron.graph.serializer;

import com.ctc.wstx.api.WriterConfig;
import com.ctc.wstx.stax.WstxOutputFactory;
import org.animotron.cache.Cache;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.handler.*;
import org.animotron.graph.traverser.*;
import org.animotron.manipulator.QCAVector;
import org.codehaus.jackson.JsonFactory;
import org.neo4j.graphdb.Relationship;

import javax.xml.stream.XMLStreamException;
import java.io.IOException;
import java.io.OutputStream;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.Field;

import static org.animotron.graph.Properties.CACHE;
import static org.animotron.utils.MessageDigester.byteArrayToHex;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class CachedSerializer extends AbstractSerializer {
	
    public static CachedSerializer STRING = new CachedSerializer(ResultTraverser._, ".txt") {
		@Override
		protected GraphHandler handler(StringBuilder out) {
	        return new TextGraphHandler(out);
		}
        @Override
        protected GraphHandler handler(Writer out) throws IOException {
            return new TextGraphHandler(out);
        }
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
	        return new TextGraphHandler(out);
		}
    };
	
	public static CachedSerializer XML = new CachedSerializer(MLResultTraverser._, ".xml") {
	    private final WstxOutputFactory OUTPUT_FACTORY = new WstxOutputFactory();
		{
	        WriterConfig conf = OUTPUT_FACTORY.getConfig();
	        conf.doSupportNamespaces(true);
	        conf.enableAutomaticNamespaces(false);
		}
		@Override
		protected GraphHandler handler(StringBuilder out) {
	        throw new UnsupportedOperationException();
		}
        @Override
        protected GraphHandler handler(Writer out) throws IOException {
	        try {
	            return new StAXGraphHandler(OUTPUT_FACTORY.createXMLStreamWriter(out));
	        } catch (XMLStreamException e) {
	            throw new IOException(e);
	        }
        }
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
	        try {
	            return new StAXGraphHandler(OUTPUT_FACTORY.createXMLStreamWriter(out));
	        } catch (XMLStreamException e) {
	            throw new IOException(e);
	        }
		}
        @Override
        public String serialize(Relationship r) throws IOException {
            StringWriter out = new StringWriter(256);
            traverser.traverse(handler(out), r);
            return out.toString();
        }
        @Override
        public String serialize(QCAVector v) throws IOException {
            StringWriter out = new StringWriter(256);
            traverser.traverse(handler(out), v);
            return out.toString();
        }
    };

	public static CachedSerializer JSON = new CachedSerializer(MLResultTraverser._, ".json") {
	    private final JsonFactory OUTPUT_FACTORY = new JsonFactory();
		@Override
		protected GraphHandler handler(StringBuilder out) {
	        throw new UnsupportedOperationException();
		}
		@Override
		protected GraphHandler handler(Writer out) throws IOException {
            return new JSONGraphHandler(OUTPUT_FACTORY.createJsonGenerator(out));
		}
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
            return new JSONGraphHandler(OUTPUT_FACTORY.createJsonGenerator(out));
		}
        @Override
        public String serialize(Relationship r) throws IOException {
            StringWriter out = new StringWriter(256);
            traverser.traverse(handler(out), r);
            return out.toString();
        }
        @Override
        public String serialize(QCAVector v) throws IOException {
            StringWriter out = new StringWriter(256);
            traverser.traverse(handler(out), v);
            return out.toString();
        }
	};

    public static CachedSerializer HTML = new CachedSerializer(MLResultTraverser._, ".html") {
        @Override
        protected GraphHandler handler(StringBuilder out) {
            return new HtmlGraphHandler(out);
        }
        @Override
        protected GraphHandler handler(Writer out) throws IOException {
            return new HtmlGraphHandler(out);
        }
        @Override
        protected GraphHandler handler(OutputStream out) throws IOException {
            return new HtmlGraphHandler(out);
        }
    };

    public static CachedSerializer HTML_PART = new CachedSerializer(MLResultTraverser._, ".html.part") {
        @Override
        protected GraphHandler handler(StringBuilder out) {
            return new HtmlPartGraphHandler(out);
        }
        @Override
        protected GraphHandler handler(Writer out) {
            return new HtmlPartGraphHandler(out);
        }
        @Override
        protected GraphHandler handler(OutputStream out) throws IOException {
            return new HtmlPartGraphHandler(out);
        }
    };

    public static CachedSerializer PRETTY_ANIMO_RESULT = new CachedSerializer(AnimoResultTraverser._, "-res-pretty.animo") {
		@Override
		protected GraphHandler handler(StringBuilder out) {
	        return new AnimoPrettyGraphHandler(out);
		}
		@Override
		protected GraphHandler handler(Writer out) {
	        return new AnimoPrettyGraphHandler(out);
		}
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
	        return new AnimoPrettyGraphHandler(out);
		}
	};
	
	public static CachedSerializer PRETTY_ANIMO = new CachedSerializer(AnimoTraverser._, "-src-pretty.animo") {
		@Override
		protected GraphHandler handler(StringBuilder out) {
			return new AnimoPrettyGraphHandler(out);
		}
		@Override
		protected GraphHandler handler(Writer out) {
			return new AnimoPrettyGraphHandler(out);
		}
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
			return new AnimoPrettyGraphHandler(out);
		}
	};

	public static CachedSerializer ANIMO_RESULT = new CachedSerializer(AnimoResultTraverser._, "-res.animo") {
		@Override
		protected GraphHandler handler(StringBuilder out) {
			return new AnimoGraphHandler(out);
		}
		@Override
		protected GraphHandler handler(Writer out) {
			return new AnimoGraphHandler(out);
		}
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
			return new AnimoGraphHandler(out);
		}
	};
	
	public static CachedSerializer ANIMO = new CachedSerializer(AnimoTraverser._, "-src.animo") {
		@Override
		protected GraphHandler handler(StringBuilder out) {
			return new AnimoGraphHandler(out);
		}
		@Override
		protected GraphHandler handler(Writer out) {
			return new AnimoGraphHandler(out);
		}
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
			return new AnimoGraphHandler(out);
		}
	};

	public static CachedSerializer ANIMO_RESULT_ONE_STEP = new CachedSerializer(new AnimoResultOneStepTraverser(), "-1step-res.animo") {
		@Override
		protected GraphHandler handler(StringBuilder out) {
			return new AnimoGraphHandler(out);
		}
		@Override
		protected GraphHandler handler(Writer out) {
			return new AnimoGraphHandler(out);
		}
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
			return new AnimoGraphHandler(out);
		}
	};

	private String ext;

    protected CachedSerializer(AnimoTraverser traverser, String ext){
        super(traverser);
        this.ext = ext;
	}

    private String key (Relationship r) throws IOException {
        StringBuilder s = new StringBuilder(2);
        s.append(byteArrayToHex(DigestSerializer._.serialize(r)));
        s.append(ext);
        return s.toString();
    }

    public final void serialize(Relationship r, OutputStream out, Cache cache) throws IOException {
        String key = key(r);
        if (cache.available(key)) {
            cache.get(key, out);
        } else {
            OutputStream os = cache.stream(key, out);
            try {
                serialize(r, os);
                cache(r, cache, key);
            } catch (IOException e) {
                cache.drop(key);
                throw e;
            }
            os.close();
        }
    }

    public final void serialize(Relationship r, StringBuilder out, Cache cache) throws IOException {
        String key = key(r);
        if (cache.available(key)) {
            cache.get(key, out);
        } else {
            OutputStream os = cache.stream(key, out);
            try {
                serialize(r, os);
                cache(r, cache, key);
            } catch (IOException e) {
                cache.drop(key);
                throw e;
            }
            os.close();
        }
    }

    public final String serialize(Relationship r, Cache cache) throws IOException {
        String key = key(r);
        if (cache.available(key)) {
            return cache.get(key);
        } else {
            StringBuilder out = new StringBuilder(1024);
            OutputStream os = cache.stream(key, out);
            try {
                serialize(r, os);
                cache(r, cache, key);
            } catch (IOException e) {
                cache.drop(key);
                throw e;
            }
            os.close();
            return out.toString();
        }
    }

    private void cache(final Relationship r, final Cache cache, final String key) {
        AnimoGraph.execute(new GraphOperation<Void>() {
            @Override
            public Void execute() throws Exception {
                String entity;
                entity = entity(key, cache);
                if (CACHE.has(r)) {
                    String[] e = (String[]) CACHE.get(r);
                    String[] e1 = new String[e.length + 1];
                    for (int i = 0; i < e.length; i++) {
                        if (e[i] == entity) {
                            return null;
                        }
                        e1[i] = e[i];
                    }
                    e1[e.length] = entity;
                    CACHE.set(r, e1);
                } else {
                    String[] e = new String[] {entity};
                    CACHE.set(r, e);
                }
                return null;
            }
        });
    }
    
    public static String entity(String key, Cache cache) {
        return key + "@" + cache.getClass().getName();
    }

    public static void drop(String entity) throws IOException {
        String[] token = entity.split("@");
        try {
            Class<?> clazz = Class.forName(token[1]);
            Field field = clazz.getField("_");
            Cache cache = (Cache) field.get(clazz);
            cache.drop(token[0]);
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        } catch (NoSuchFieldException e) {
            e.printStackTrace();
        }
    }

    public static void drop(final Relationship r) {
        AnimoGraph.execute(new GraphOperation<Void>() {
            @Override
            public Void execute() throws IOException {
                if (CACHE.has(r)) {
                    try {
                        for (String i : (String[]) CACHE.get(r)) {
                            drop(i);
                        }
                    } catch (IOException e) {
                        throw e;
                    } finally {
                        CACHE.remove(r);
                    }
                }
                return null;
            }
        });
    }

}