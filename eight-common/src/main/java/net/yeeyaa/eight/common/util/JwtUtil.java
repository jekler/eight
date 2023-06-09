package net.yeeyaa.eight.common.util;

import java.io.InputStream;
import java.security.Key;
import java.security.KeyStore;
import java.security.PublicKey;
import java.util.Date;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;
import net.yeeyaa.eight.core.resource.MemoryLResource;
import net.yeeyaa.eight.core.resource.ResourceMap;
import net.yeeyaa.eight.core.storage.Storage.Method;
import net.yeeyaa.eight.core.util.Interval;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwt;
import io.jsonwebtoken.JwtBuilder;
import io.jsonwebtoken.JwtParser;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;


public class JwtUtil<T, V, R> implements IProcessor<T, R>, IBiProcessor<T, V, R> {
	protected final Key key;
	protected final PublicKey publicKey;
	protected final byte[] shareKey;
	protected SignatureAlgorithm algorithm = SignatureAlgorithm.RS256;
	protected Long expire;
	protected V para;

	public JwtUtil(String shareKey){
		key = publicKey = null;
		this.shareKey = shareKey.getBytes();
	}

	public JwtUtil(String alias, String password, IExtendable<Object> keyfile){
		this(alias, password, KeyStore.getDefaultType(), keyfile, null);
	}
	
	public JwtUtil(String alias, String password, IExtendable<Object> keyfile, String storePassword){
		this(alias, password, KeyStore.getDefaultType(), keyfile, storePassword);
	}
	
	public JwtUtil(String alias, String password, String keystore, IExtendable<Object> keyfile, String storePassword) {
		InputStream is = null;
		try {
			KeyStore ks = KeyStore.getInstance(keystore);
			is = keyfile.<InputStream>extend(Method.input);
			ks.load(is, storePassword == null ? null : storePassword.toCharArray());
			key = ks.getKey(alias, password == null ? new char[0] : password.toCharArray());
			publicKey = ks.getCertificate(alias).getPublicKey();
			shareKey = null;
		} catch(Exception e) {
			throw new PlatformException(PlatformError.ERROR_IO, e);
		} finally {
			if (is != null) try {
				is.close();
			} catch(Exception e) {
				throw new PlatformException(PlatformError.ERROR_IO, e);
			}
		}	
	}

	public void setPara(V para) {
		this.para = para;
	}
	
	public void setAlgorithm(SignatureAlgorithm algorithm) {
		if (algorithm != null) this.algorithm = algorithm;
	}

	public void setExpire(String expire) {
		if (expire != null) this.expire = new Interval(expire).getTime(TimeUnit.MILLISECONDS);
	}
	
	@Override
	public R perform(T first, V second) {
		if (first instanceof String) {
			JwtParser parser = Jwts.parser();
			Jwt jwt = publicKey == null ? parser.setSigningKey(shareKey).parse((String)first) : parser.setSigningKey(publicKey).parse((String)first);
			if (second == null) return (R) jwt;
			else if (second.equals(0)) return (R) jwt.getBody();
			else if (second.equals(1)) return (R) jwt.getHeader();
			else if (second.equals(2)) return jwt.getBody() instanceof Map ? (R) new MemoryLResource((Map)jwt.getBody()) : (R) jwt.getBody();
			else if (second.equals(3)) return jwt.getHeader() instanceof Map ? (R) new MemoryLResource((Map)jwt.getHeader()) : (R) jwt.getHeader();		
			else return (R) ((Map<String, Object>)jwt.getBody()).get(second);
		} else {
			JwtBuilder builder = Jwts.builder();
			if (second instanceof Map) builder = builder.setHeader((Map<String, Object>)second);
			if (first instanceof Claims) builder = builder.setClaims((Claims)first);
			else if (first instanceof Map) builder = builder.setClaims((Map<String, Object>)first);
			else if (first instanceof IListableResource) builder = builder.setClaims(new ResourceMap((IListableResource)first));
			if (expire != null) builder = builder.setExpiration(new Date(System.currentTimeMillis() + expire));
			return key == null ? (R) builder.signWith(algorithm, shareKey).compact() : (R) builder.signWith(algorithm, key).compact();
		}
	}

	@Override
	public R process(T object) {
		return perform(object, para);
	}
}
