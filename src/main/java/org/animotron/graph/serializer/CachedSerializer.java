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
import org.animotron.graph.GraphOperation;
import org.animotron.graph.handler.*;
import org.animotron.graph.traverser.*;
import org.animotron.manipulator.QCAVector;
import org.animotron.utils.MessageDigester;
import org.codehaus.jackson.JsonFactory;
import org.neo4j.graphdb.Relationship;

import javax.xml.stream.XMLStreamException;
import java.io.IOException;
import java.io.OutputStream;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.Field;

import static org.animotron.graph.AnimoGraph.execute;
import static org.animotron.graph.Properties.CACHE;
import static org.animotron.graph.Properties.RUUID;

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

    private static String key(String uuid, String ext) throws IOException {
        StringBuilder s = new StringBuilder(2);
        s.append(uuid);
        s.append(ext);
        return s.toString();
    }

    private String uuid(Relationship r) {
        return RUUID.has(r) ? (String) RUUID.get(r) : MessageDigester.uuid().toString();
    }

    public final void serialize(Relationship r, OutputStream out, Cache cache) throws Throwable {
        serialize(r, out, cache, uuid(r));
    }

    public final void serialize(Relationship r, OutputStream out, Cache cache, String uuid) throws Throwable {
        String key = key(uuid, ext);
        if (cache.available(key)) {
            cache.get(key, out);
        } else {
            OutputStream os = cache.stream(key, out);
            try {
                serialize(r, os);
                cache(r, cache, uuid);
            } catch (Throwable t) {
                cache.drop(key);
                throw t;
            }
            os.close();
        }
    }

    public final void serialize(Relationship r, StringBuilder out, Cache cache) throws Throwable {
        serialize(r, out, cache, uuid(r));
    }

    public final void serialize(Relationship r, StringBuilder out, Cache cache, String uuid) throws Throwable {
        String key = key(uuid, ext);
        if (cache.available(key)) {
            cache.get(key, out);
        } else {
            OutputStream os = cache.stream(key, out);
            try {
                serialize(r, os);
                cache(r, cache, uuid);
            } catch (Throwable t) {
                cache.drop(key);
                throw t;
            }
            os.close();
        }
    }

    public final String serialize(Relationship r, Cache cache) throws Throwable {
        return serialize(r, cache, uuid(r));
    }

    public final String serialize(Relationship r, Cache cache, String uuid) throws Throwable {
        String key = key(uuid, ext);
        if (cache.available(key)) {
            return cache.get(key);
        } else {
            StringBuilder out = new StringBuilder(1024);
            OutputStream os = cache.stream(key, out);
            try {
                serialize(r, os);
                cache(r, cache, uuid);
            } catch (Throwable t) {
                cache.drop(key);
                throw t;
            }
            os.close();
            return out.toString();
        }
    }
    
    private void cache(final Relationship r, final Cache cache, final String uuid) throws Throwable {
        final String[] entities;
        String entity = entity(cache);
        if (CACHE.has(r)) {
            String[] e = (String[]) CACHE.get(r);
            entities = new String[e.length + 1];
            for (int i = 0; i < e.length; i++) {
                if (e[i] == entity) {
                    return;
                }
                entities[i] = e[i];
            }
            entities[e.length] = entity;
        } else {
            entities = new String[]{entity};
        }
        execute(new GraphOperation<Void>() {
            @Override
            public Void execute() throws Throwable {
                RUUID.set(r, uuid);
                CACHE.set(r, entities);
                return null;
            }
        });
    }
    
    private String entity(Cache cache) {
        return ext + "@" + cache.getClass().getName();
    }

    private static void drop(String uuid, String entity) throws IOException {
        String[] token = entity.split("@");
        try {
            Class<?> clazz = Class.forName(token[1]);
            Field field = clazz.getField("_");
            Cache cache = (Cache) field.get(clazz);
            StringBuilder s = new StringBuilder(2);
            cache.drop(key(uuid, token[0]));
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        } catch (NoSuchFieldException e) {
            e.printStackTrace();
        }
    }

    public static void drop(Relationship r) throws Throwable {
        if (CACHE.has(r)) {
            String uuid = (String) RUUID.get(r);
            try {
                for (String i : (String[]) CACHE.get(r)) {
                    drop(uuid, i);
                }
            } catch (IOException e) {
                throw e;
            } finally {
                CACHE.remove(r);
                RUUID.remove(r);
            }
        }
    }

}