package org.animotron.games.words;

import static org.junit.Assert.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;

import javolution.util.FastMap;
import javolution.util.FastSet;

import org.junit.Test;

public class SensorsTest {

	@Test
	public void test() throws IOException {
		
		char[] elements = new char[] {'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'};
		
		double[] sensor = new double[elements.length];
		double[][] connections = new double[elements.length][elements.length];
		
		Random random = new Random();
		
		int number = elements.length - 1;
		
		for (int i = 0; i < number; i++) {

			sensor[i] = random.nextDouble();
			
			for (int j = 0; j < number; j++) {
				connections[i][j] = random.nextDouble(); 
			}
		}

		double spaceEnergy = -random.nextDouble();
		
		InputStream stream = getClass().getResourceAsStream("words.txt");
		InputStreamReader reader = new InputStreamReader(stream);
		BufferedReader in = new BufferedReader(reader);
		
		Map<String, Double> words = new FastMap<String, Double>();

		String word; 
		int f, t;
		double energy;
		
		while ((word = in.readLine()) != null)   {
			word = word.trim();
			
			System.out.print(""+word+" = ");

			f = t = -1; energy = 0;
			for (int i = 0; i < word.length(); i++) {
				char ch = word.charAt(i);
				
				if (ch == ' ') {
					energy += spaceEnergy;
					continue;
				}
				
				t = ((int)ch)-((int)'a');
				
				energy += sensor[t];
				
				if (f >= 0) {
					energy += connections[f][t];
				}
				f = t;
			}
			
			System.out.println(energy);

			words.put(word, energy);
		}
		
		//check for same energy
		Set<Double> was = new FastSet<Double>();
		for (Entry<String, Double> entry : words.entrySet()) {
			if (was.contains(entry.getValue())) {
				fail("dublication!!! "+entry.getKey()+" = "+entry.getValue());
			}
			
			was.add(entry.getValue());
		}
	}

}
