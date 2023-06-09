package net.yeeyaa.eight.aspect.auth.login;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;

public class Logout implements IProcessor<Object, Boolean> {
	protected final Logger log;
	protected IResource<Object, Object> session;
	
	public Logout() {
		this.log = LoggerFactory.getLogger(Login.class);
	}

	public Logout(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(Logout.class) : log;
	}
	
	public void setSession(IResource<Object, Object> session) {
		if(session != null) this.session = session;
	}
	
	@Override
	public Boolean process(Object instance) {
		if (instance != null) try{
			session.discard(instance);
			return true;
		}catch(Exception e){
			log.error("Logout: logout error.", e);
		}
		return false;
	}
}
