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
package org.animotron.statement;

import javolution.util.FastList;
import javolution.util.FastMap;
import org.clapper.util.classutil.*;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;

import java.io.*;
import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Map;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Statements {

        private static boolean fast = true;
        private static boolean ready = false;
        private static boolean run = false;

        private static Map<String, Statement> statementsByName = new FastMap<String, Statement>();

        @SuppressWarnings("unchecked")
        private static void loadClass(String name) {
            Class<? extends Statement> clazz;
            try {
                clazz = (Class<? extends Statement>) Class.forName( name );

                try {
                    Field field = clazz.getField("_");
                    Statement statement = (Statement) field.get(clazz);
                    for (String i :statement.names()) {
                        statementsByName.put(i, statement);
                    }
                } catch (Throwable t) {
                    //TODO: log
                    t.printStackTrace();
                }
            } catch (ClassNotFoundException e) {
                //should not happen
                //TODO: log
                e.printStackTrace();
            }
        }

        protected static void scan() {
            Thread scanner = new Thread(new Runnable() {
                @Override
                public void run() {
                    //create class finder
                    ClassFinder finder = new ClassFinder();

                    //put class paths as searching place
                    finder.addClassPath();

                    //create filter for 'Quanta' implementations
                    ClassFilter filter =
                        new AndClassFilter(
                            // Must implement the Quanta interface
                            new SubclassClassFilter (Statement.class),

                            // Must not be an interface
                            new NotClassFilter (new InterfaceOnlyClassFilter()),

                            // Must not be abstract
                            new NotClassFilter (new AbstractClassFilter()));

                    Collection<ClassInfo> foundClasses = new FastList<ClassInfo>();

                    //searching
                    finder.findClasses(foundClasses, filter);

                    //scan classes
                    for (ClassInfo classInfo : foundClasses)
                        loadClass(classInfo.getClassName());

                    if (fast)
                        try {
                            File file = new File("quantas.ser");
                            file.delete();
                            BufferedWriter bw = new BufferedWriter(new FileWriter(file));
                            for (Statement s : statementsByName.values()) {
                                bw.write(s.getClass().getName());
                                bw.write("\n");
                            }
                            bw.close();

                        } catch (Throwable t) {
                        }

                    Statements.ready = true;
                    Statements.run = true;
                }
            });

            if (fast) {
                try {
                    //load classes
                    BufferedReader br = new BufferedReader(new FileReader("quantas.ser"));
                    String strLine;
                    while ((strLine = br.readLine()) != null)   {
                        loadClass(strLine);
                    }
                    br.close();

                    Statements.ready = true;
                } catch (Throwable t) {
                    //e.printStackTrace();
                }
            }

            scanner.start();
        }

        static {
            scan();
        }

        public static Statement name(String name) {
            ready();
            return statementsByName.get(name);
        }

        public static Statement clazz(Class<? extends Statement> clazz) {
            Field field;
            try {
                field = clazz.getField("_");
                return (Statement) field.get(clazz);
            } catch (Throwable t) {
                return null; //or RuntimeException?
            }
        }

        public static Statement relationshipType(Relationship r) {
            if (r == null) return null;
            return relationshipType(r.getType());
        }

        public static Statement relationshipType(RelationshipType type) {
            return relationshipType(type.name());
        }

        public static Statement relationshipType(String name) {
            ready();
            Statement s = statementsByName.get(name);
    //		if (s == null && run())
    //			s = statementsByRelationType.getDef(reference);

            return s;
        }

        public static void ready() {
            while (!ready) {
                try {
                    //TODO: put timeout
                    Thread.sleep(1000);
                    //System.out.println("ready?");
                } catch (InterruptedException e) {
                    return;
                }
            }
        }

        public static boolean run() {
            while (!run) {
                try {
                    //TODO: put timeout
                    Thread.sleep(1000);
                    //System.out.println("run?");
                } catch (InterruptedException e) {
                    return false;
                }
            }
            return true;
        }
}
