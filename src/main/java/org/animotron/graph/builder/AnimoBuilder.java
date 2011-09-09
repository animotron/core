/*
 *  Copyright (C) 2011 The Animo Project
 *  http://animotron.org
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 3
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package org.animotron.graph.builder;

import java.io.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class AnimoBuilder extends GraphBuilder {

    private Reader reader;

    public AnimoBuilder (InputStream stream) {
        reader = new InputStreamReader(stream);
    }

    public AnimoBuilder (String str) {
        reader = new StringReader(str);
    }

    public void build() throws IOException {

        int len;
        char[] buff = new char[4 * 1024];
        StringBuilder s = new StringBuilder();

        boolean text = false;
        char prev = '\0';

        while ((len=reader.read(buff))>0) {
            for (int i = 0; i < len; i++){
                char ch = buff[i];
                if (ch == '\"') {
                    if (text && prev == '\\') {
                        s.append(ch);
                    }
                    text = text ? prev == '\\' : true;
                } else {
                    if (text) {
                        if (ch != '\\') {
                            s.append(ch);
                        }
                    } else {
                        switch (ch) {
                            case ' '  :
                            case '\t' :
                            case '\n' :
                                if (s.length() > 0) {
                                    token(s, text);
                                    s = new StringBuilder();
                                }
                                break;
                            case '('  :
                                startList();
                                if (s.length() > 0) {
                                    token(s, text);
                                    s = new StringBuilder();
                                }
                                break;
                            case ')'  :
                                if (s.length() > 0) {
                                    token(s, text);
                                    s = new StringBuilder();
                                }
                                endList();
                                break;
                            default   : s.append(ch);
                        }
                    }
                }
                prev = ch;
            }
            if (s.length() > 0) {
                token(s, text);
            }
        }

    }

    private void token(StringBuilder s, boolean text) {
        System.out.print(s);
        System.out.println();
    }

    private void startList() {
    }

    private void endList() {
    }

}
