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
package org.animotron.games.tetris;

import java.util.LinkedHashMap;
import java.util.Map;

import javax.xml.stream.XMLStreamException;

import org.animotron.ATest;
import org.junit.Ignore;
import org.junit.Test;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class TetrisTest extends ATest {
	
	//players
	private static final String THE_PLAYER = 
		"<the:player "+ANIMO_NSs+">" +
		"	<have:keyboard/>" +
		"</the:player>";

	private static final String KEY = "key"; 

	private static final String KEY_UP = "key-up"; 
	private static final String KEY_DOWN = "key-down"; 
	private static final String KEY_LEFT = "key-left"; 
	private static final String KEY_RIGHT = "key-right"; 
	private static final String KEY_SPACE = "key-space"; 

	//keyboard
	private static final String THE_KEYBOARD = 
		"<the:keyboard "+ANIMO_NSs+">" +
		"	<have:key>" +
		"		<an:"+KEY_UP+"/>" +
		"		<an:"+KEY_DOWN+"/>" +
		"		<an:"+KEY_LEFT+"/>" +
		"		<an:"+KEY_RIGHT+"/>" +
		"		<an:"+KEY_SPACE+"/>" +
		"	</have:key>" +
		"</the:keyboard>";

	private static final String[] PLAYER_IDs = {"player1", "player2"};
	private static final String[] KEYBOARD_IDs = {"keyboard1", "keyboard2"};

	private static void playerAndKeyboard(Map<String, String> map, String payerID, String keyboardID) {
		map.put(keyboardID+".xml", 
				"<the:"+keyboardID+" "+ANIMO_NSs+">" +
				"	<is:keyboard/>" +
				"</the:"+keyboardID+">");

		map.put(payerID+".xml", 
			"<the:"+payerID+" "+ANIMO_NSs+">" +
			"	<is:player/>" +
			"	<have:keyboard>" +
			"		<an:"+keyboardID+"/>" +
			"	</have:keyboard>" +
			"</the:"+payerID+">");
	}
	
	//action
	private static final String ACTION = "action"; 

	private static final String LEFT = "left"; 
	private static final String RIGHT = "right"; 
	private static final String ROTATE = "rotate"; 
	private static final String SPEED_UP = "speed-up"; 
	private static final String DROP = "drop";
	
	//shapes
	private static final String SHAPE = "shape";
	
	//tetrominoes (shapes)
	private static final String TETROMINOES = "tetrominoes";
	
	// ░░░░
	private static final String I = "I-tetrominoes";
	
	// ░
	// ░░░
	private static final String J = "J-tetrominoes";
	
	//   ░
	// ░░░
	private static final String L = "L-tetrominoes";
	
	// ░░
	// ░░
	private static final String O = "O-tetrominoes";

	//  ░░
	// ░░
	private static final String S = "S-tetrominoes";
	
	//  ░
	// ░░░
	private static final String T = "T-tetrominoes";
	
	// ░░
	//  ░░
	private static final String Z = "Z-tetrominoes";
	
	//game
	private static final String GAME = "game"; 
	private static final String TETRIS = "tetris";
	private static final String GAME_FIELD = "game-field"; 
	
	
	private static final String THE_TETRIS = 
		"<the:tetris "+ANIMO_NSs+">" +
		//shapes
		"	<have:"+TETROMINOES+">" +
		"		<an:"+I+"/>" +
		"		<an:"+J+"/>" +
		"		<an:"+L+"/>" +
		"		<an:"+O+"/>" +
		"		<an:"+S+"/>" +
		"		<an:"+T+"/>" +
		"		<an:"+Z+"/>" +
		"	</have:"+TETROMINOES+">" +
		//game field
		"	<have:gamefield>" +
		"		<have:"+TETROMINOES+"/>" +
		"		<have:shape>" +
		"			<the:floor-shape/>" +
		"		</have:shape>" +
		"		<have:speed/>" +
		"		<have:tic>" +
		"			<have:seconds>" +
		"				<get:speed>" +
		"					<any:gamefield/>" +
		"				</get:speed>" +
		"			</have:seconds>" +
		"			<an:down/>" +
		"		</have:tic>" +
		"	</have:gamefield>" +
		//actions
		"	<have:action>" +
		"		<the:left>" +
		"			<op:enlarge>" +
		"				<get:X>" +
		"					<any:"+TETROMINOES+"/>" +
		"				</get:X>" +
		"				<Q:N-1/>" +
		"			</op:enlarge>" +
		"		</the:left>" +
		"		<the:right>" +
		"			<op:enlarge>" +
		"				<get:X>" +
		"					<any:"+TETROMINOES+"/>" +
		"				</get:X>" +
		"				<Q:N1/>" +
		"			</op:enlarge>" +
		"		</the:right>" +
		"		<the:down>" +
		"			<op:enlarge>" +
		"				<get:Y>" +
		"					<any:"+TETROMINOES+"/>" +
		"				</get:Y>" +
		"				<Q:N1/>" +
		"			</op:enlarge>" +
		"		</the:down>" +
		"	</have:action>" +
		"</the:tetris>";

	//@Test
	public void game() throws XMLStreamException {
		
		if (true) {
	        Map<String, String> data = new LinkedHashMap<String, String>();
	        
	        //keyboard
	        data.put("keyboard.xml", THE_KEYBOARD);
	        
	        Utils.the(data, KEY);
	        for (String key : new String[] {KEY_UP, KEY_DOWN, KEY_LEFT, KEY_RIGHT, KEY_SPACE} ) {
		        Utils.the(data, key, KEY);
	        }
	        
	        //player
	        data.put("player.xml", THE_PLAYER);
	        for (int i = 0; i < 2; i++) {
	        	playerAndKeyboard(data, PLAYER_IDs[i], KEYBOARD_IDs[i]);
	        }
	        
	        //game
	        Utils.the(data, GAME);
	        data.put(TETRIS+".xml", THE_TETRIS);
	        Utils.the(data, GAME_FIELD, TETRIS);
	        
	        //action
	        Utils.the(data, ACTION);
	        Utils.the(data, LEFT, ACTION);
	        Utils.the(data, RIGHT, ACTION);
	        Utils.the(data, ROTATE, ACTION);
	        Utils.the(data, SPEED_UP, ACTION);
	        Utils.the(data, DROP, ACTION);
	        
	    	//shape 
	        Utils.the(data, SHAPE);

	        //tetrominoes 
	        Utils.the(data, TETROMINOES);
	        Utils.the(data, I, TETROMINOES);
	        Utils.the(data, J, TETROMINOES);
	        Utils.the(data, L, TETROMINOES);
	        Utils.the(data, O, TETROMINOES);
	        Utils.the(data, S, TETROMINOES);
	        Utils.the(data, T, TETROMINOES);
	        Utils.the(data, Z, TETROMINOES);

	        store(data);
		}

//		http://tetrisapp.appspot.com/

		//degrees = random.choice([0,90,180,270])
		//BOARD_WIDTH = 10;
		//BOARD_HEIGHT = 20
		
		//ScoringRows	Score
		// 1	10
		// 2	25
		// 3	40
		// 4	55
		
//        <table border="0">
//        <tr><td valign="top">
//        <div style="width:240px; height:480px; background-color:Black; padding: 0px; border: solid 1px red; position:relative" id="divBoard">
//        <div class="row"></div>
//        <div class="row"></div>
//        <div class="row"></div>
//        <div class="row"></div>
//        <div class="row"></div>
//        <div class="row"></div>
//        <div class="row"></div>
//        <div class="row"></div>
//        <div class="row"></div>
//        <div class="row"></div>
//        <div class="row"></div>
//        <div class="row"></div>
//        <div class="row"></div>
//        <div class="row"></div>
//        <div class="row"></div>
//        <div class="row"></div>
//        <div class="row"></div>
//        <div class="row"></div>
//        <div class="row"></div>
//        <div class="row"></div>
//        </div>
//        </td>

//	    <p>
//        <img src="/static/l.png"/>
//        <img src="/static/j.png"/>
//        <img src="/static/o.png"/>
//        <img src="/static/i.png"/>
//        <img src="/static/t.png"/>
//        <img src="/static/s.png"/>
//        <img src="/static/z.png"/>
//        </p>		
	}
}
