package net.yeeyaa.eight.aspect.auth.login;

import java.util.Map.Entry;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class SsoValidation implements IProcessor<Entry<Object, Object>, Object> {
	protected final Logger log;
	protected IProcessor<Object, Object> secProcessor;
	protected IInputResource<Object, Object> creditResource;
	protected IResource<Object, Entry<Object, Object>> tokenResource;
	protected IProcessor<Object, Boolean> checkProcessor;
	protected Boolean strict = true;

	public SsoValidation() {
		this.log = LoggerFactory.getLogger(SsoValidation.class);
	}

	public SsoValidation(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(SsoValidation.class) : log;
	}
	
	public void setStrict(Boolean strict) {
		if(strict != null) this.strict = strict;
	}

	public void setCheckProcessor(IProcessor<Object, Boolean> checkProcessor) {
		this.checkProcessor = checkProcessor;
	}

	public void setSecProcessor(IProcessor<Object, Object> secProcessor) {
		this.secProcessor = secProcessor;
	}

	public void setCreditResource(IInputResource<Object, Object> creditResource) {
		this.creditResource = creditResource;
	}

	public void setTokenResource(IResource<Object, Entry<Object, Object>> tokenResource) {
		this.tokenResource = tokenResource;
	}

	@Override
	public Object process(Entry<Object, Object> credential) {
		Object user = null;
		if(credential != null && credential.getKey() != null && credential.getValue() != null) try{
			Entry<Object, Object> cred = null;
			if(strict) cred = tokenResource.discard(credential.getKey());
			else cred = tokenResource.find(credential.getKey());
			if(cred != null){
				if(secProcessor != null) credential.setValue(secProcessor.process(credential.getValue()));
				if(credential.getValue().equals(cred.getValue())){
					Object tmp = creditResource.find(cred.getKey());
					if(checkProcessor == null || (tmp != null && checkProcessor.process(tmp))) user = tmp;
					if(!strict) tokenResource.discard(credential.getKey());
				}
			}
		}catch(Exception e){
			log.error("CasValidation: cas validate error.", e);
		}
		return user;
	}
}
