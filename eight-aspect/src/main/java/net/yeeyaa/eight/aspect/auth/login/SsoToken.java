package net.yeeyaa.eight.aspect.auth.login;

import java.util.Map.Entry;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.util.TypeConvertor;
import net.yeeyaa.eight.service.ServiceError;


public class SsoToken implements IProcessor<Entry<Object, Object>, Object> {
	protected IProcessor<Object, Object> genProcessor;
	protected IProcessor<Object, Boolean> checkProcessor;
	protected IInputResource<Object, Object> creditResource;
	protected IOutputResource<Object, Entry<Object, Object>> tokenResource;
	
	public void setGenProcessor(IProcessor<Object, Object> genProcessor) {
		this.genProcessor = genProcessor;
	}

	public void setCheckProcessor(IProcessor<Object, Boolean> checkProcessor) {
		this.checkProcessor = checkProcessor;
	}

	public void setCreditResource(IInputResource<Object, Object> creditResource) {
		this.creditResource = creditResource;
	}

	public void setTokenResource(IOutputResource<Object, Entry<Object, Object>> tokenResource) {
		this.tokenResource = tokenResource;
	}

	@Override
	public Object process(Entry<Object, Object> credential) {
		Object token = null;
		if(credential != null && credential.getKey() != null && credential.getValue() != null) {
			Object user = creditResource.find(credential.getKey());
			if(user != null && (checkProcessor == null || checkProcessor.process(user))){
				if(genProcessor != null) token = genProcessor.process(credential.getValue());
				else token = TypeConvertor.randomUuid();
				tokenResource.store(credential, token);
			} else if(user == null) throw new PlatformException(ServiceError.SESSION_TIMEOUT);
		}
		return token;
	}
}
