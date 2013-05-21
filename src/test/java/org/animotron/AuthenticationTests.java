/*
 *  Copyright (C) 2012-2013 The Animo Project
 *  http://animotron.org
 *
 *  This file is part of Animi.
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
package org.animotron;

import static org.junit.Assert.*;

import java.io.IOException;

import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class AuthenticationTests extends ATest {

	@Test
	public void openid() throws IOException {
		__(
			"def OAuth-2.0" +
			"	(authorization).",
			
			//if have domain & support http service
			"def fetch-logo" +
			"	get protocol-https" +
			"	'://'" +
			"	get domain" +
			"	'/favicon.ico'" +
			".",
			
			"def linkedin-oauth" +
			"	(OAuth-2.0)" +
			"	(lang_en '')" +
			"	(domain 'linkedin.com')" +
			"	(authorize_url " + //redirect there
			"		'https://www.linkedin.com/uas/oauth2/authorization?response_type=code'" +
			"		'&client_id='" +
			"			get API_KEY" +
			"		'&scope=SCOPE'" +
			"		'&state='" +
			"			get STATE" +
			"		'&redirect_uri='" +
			"			get REDIRECT_URI" +
			"	)" + 
			//REDIRECT_URI/?code=AUTHORIZATION_CODE&state=STATE
			//REDIRECT_URI/?error=access_denied&error_description=the+user+denied+your+request&state=STATE
			
			"	(request_token_url" + //post
			"		'https://www.linkedin.com/uas/oauth2/accessToken?grant_type=authorization_code'" +
			"		'&code='" +
			"			get AUTHORIZATION_CODE" +
			"		'&redirect_uri='" +
			"			get REDIRECT_URI" +
			"		'&client_id='" +
			"			get API_KEY" +
			"		'&client_secret='" +
			"			get SECRET_KEY" +
			"	)" +
			//{"expires_in":5184000,"access_token":"AQXdSP_W41_UPs5ioT_t8HESyODB4FqbkJ8LrV_5mff4gPODzOYR"}
			
			".",
			
			"def authorization-process" +
			"	(process)" +
			"	(generate-state" +
			"		(redirect-to-get-AUTHORIZATION_CODE" +
			"			(check-that-STATE-equal"+
			"				(send-post-to-verfy-AUTHORIZATION_CODE" +
			"					(check-that-STATE-equal)" +
			"				)" +
			"			)" +
			"		)" +
			"	)"+
			".",
			
			"def animotron-site" +
			"	(server-name 'animotron.org' 'www.animotron.org')" +
			".",
			
			"def UUID" +
			"	(process-state)" +
			"	(authorization-process" +
			"		possition" +
			"			relation-id here)" +
			"."
		);
		
		
	}

}
