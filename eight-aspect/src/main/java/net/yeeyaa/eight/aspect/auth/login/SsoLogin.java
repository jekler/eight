package net.yeeyaa.eight.aspect.auth.login;

import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.aspect.auth.filter.SsoFilter;
import net.yeeyaa.eight.core.util.Content.Couple;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class SsoLogin implements IProcessor<Object, Boolean> {
	protected final Logger log;
	protected IProcessor<Entry<Object, Object>, Object> checkProcessor;
	protected IProcessor<Object, Void> creditProcessor;
	protected Object key = "userid";
	protected IResource<Object, Object> session;
	
	public SsoLogin() {
		this.log = LoggerFactory.getLogger(SsoLogin.class);
	}

	public SsoLogin(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(SsoLogin.class) : log;
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

	public void setCheckProcessor(IProcessor<Entry<Object, Object>, Object> checkProcessor) {
		this.checkProcessor = checkProcessor;
	}

	@Override
	public Boolean process(Object token) {
		if(token != null) try{
			Object key = session.find(SsoFilter.SSO_KEY);
			if(key != null) {
				Entry<Object, Object> credential = new Couple<Object, Object>(token, key);
				Object userid = checkProcessor.process(credential);
				if(userid != null) {
					if(creditProcessor != null) creditProcessor.process(userid);
					else session.store(userid, this.key);
					return true;
				}		
			}
		}catch(Exception e){
			log.error("CasLogin: login error.", e);
		}
		return false;
	}
}
