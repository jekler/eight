package net.yeeyaa.eight.client;

import java.util.Collections;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.PlatformException.DefaultError;
import net.yeeyaa.eight.core.util.Content.Couple;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class SsoClient implements IProcessor<Object, Object> {
	public static final String SSO_KEY = "sso.key";
	protected final Logger log;
	protected IProcessor<Object, Object> credential;
	protected IProcessor<Object, Object> ssoAuth;
	protected IProcessor<Entry<Object, Object>, Object> ssoToken;
	protected ThreadLocal<Object> token = new ThreadLocal<Object>();
	protected Map<Object, Object> store = Collections.synchronizedMap(new WeakHashMap<Object, Object>());
	protected DefaultError sessionout = new DefaultError("ServiceError", null, "SESSION_TIMEOUT");
	
	public SsoClient() {
		this.log = LoggerFactory.getLogger(SsoClient.class);
	}

	public SsoClient(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(SsoClient.class) : log;
	}
	
	public void setSessionout(DefaultError sessionout) {
		if(sessionout != null)this.sessionout = sessionout;
	}

	public void setCredential(IProcessor<Object, Object> credential) {
		this.credential = credential;
	}

	public void setSsoAuth(IProcessor<Object, Object> ssoAuth) {
		this.ssoAuth = ssoAuth;
	}

	public void setSsoToken(IProcessor<Entry<Object, Object>, Object> ssoToken) {
		this.ssoToken = ssoToken;
	}

	@Override
	public Object process(Object key) {
		Object ret = null;
		if(token.get() != null) try{
			Object tkey = store.get(key);
			if(tkey == null) tkey = ssoAuth.process(credential.process(key));
			if(tkey != null){
				Entry<Object, Object> cred = new Couple<Object, Object>(tkey, token.get());
				try{
					ret = ssoToken.process(cred);
				}catch(PlatformException ex){
					if(sessionout.equals(ex.getType()) && store.get(key) != null){
						tkey = ssoAuth.process(credential.process(key));
						if(tkey != null) ret = ssoToken.process(new Couple<Object, Object>(tkey, token.get()));
					}
				}
				if(tkey != null) store.put(key, tkey);
			}
		}catch(Exception e){
			log.error("SsoClient: cas login fail.", e);
		}
		return ret;
	}
	
	public class ExtHandler implements IProcessor<Map<Object, Object>, Map<Object, Object>>{
		@Override
		public Map<Object, Object> process(Map<Object, Object> ext) {
			if(ext != null) token.set(ext.get(SSO_KEY));
			else token.set(null);
			return ext;
		}
	}
}
