package net.yeeyaa.eight.aspect.auth.login;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Login implements IProcessor<Object, Boolean> {
	protected final Logger log;
	protected IProcessor<Object, Boolean> checkProcessor;
	protected IProcessor<Object, Void> creditProcessor;
	protected Object key = "userid";
	protected IResource<Object, Object> session;
	
	public Login() {
		this.log = LoggerFactory.getLogger(Login.class);
	}

	public Login(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(Login.class) : log;
	}
	
	public void setSession(IResource<Object, Object> session) {
		if(session != null) this.session = session;
	}
		
	public void setKey(Object key) {
		if(key != null) this.key = key;
	}
	
	public void setCreditProcessor(IProcessor<Object, Void> creditProcessor) {
		this.creditProcessor = creditProcessor;
	}

	public void setCheckProcessor(IProcessor<Object, Boolean> checkProcessor) {
		this.checkProcessor = checkProcessor;
	}

	@Override
	public Boolean process(Object credential) {
		if(credential != null) try{
			if(checkProcessor == null || Boolean.TRUE.equals(checkProcessor.process(credential))) {
				if(creditProcessor != null) creditProcessor.process(credential);
				else session.store(credential, key);
				return true;		
			}
		}catch(Exception e){
			log.error("Login: login error.", e);
		}
		return false;
	}
}