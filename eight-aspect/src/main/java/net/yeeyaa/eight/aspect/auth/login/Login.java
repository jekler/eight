package net.yeeyaa.eight.aspect.auth.login;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Login implements IProcessor<Object, Boolean> {
	protected final Logger log;
	protected IProcessor<Object, Object> checkProcessor;
	protected IProcessor<Object, Object> creditProcessor;
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

	public void setCheckProcessor(IProcessor<Object, Object> checkProcessor) {
		this.checkProcessor = checkProcessor;
	}

	public void setCreditProcessor(IProcessor<Object, Object> creditProcessor) {
		this.creditProcessor = creditProcessor;
	}

	@Override
	public Boolean process(Object credential) {
		if(credential != null) try{
			if(checkProcessor != null) credential = checkProcessor.process(credential);
			if(credential != null) {
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
