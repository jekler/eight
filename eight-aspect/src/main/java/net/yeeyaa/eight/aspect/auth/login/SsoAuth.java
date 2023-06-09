package net.yeeyaa.eight.aspect.auth.login;

import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.TypeConvertor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class SsoAuth implements IProcessor<Object, String> {
	protected final Logger log;
	protected IProcessor<Object, Boolean> checkProcessor;
	protected IProcessor<Object, Object> creditProcessor;
	protected IProcessor<Object, String> genProcessor;
	protected IOutputResource<String, Object> creditResource;
	
	public SsoAuth() {
		this.log = LoggerFactory.getLogger(SsoAuth.class);
	}

	public SsoAuth(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(SsoAuth.class) : log;
	}
	
	public void setGenProcessor(IProcessor<Object, String> genProcessor) {
		this.genProcessor = genProcessor;
	}

	public void setCreditProcessor(IProcessor<Object, Object> creditProcessor) {
		this.creditProcessor = creditProcessor;
	}

	public void setCheckProcessor(IProcessor<Object, Boolean> checkProcessor) {
		this.checkProcessor = checkProcessor;
	}

	public void setCreditResource(IOutputResource<String, Object> creditResource) {
		this.creditResource = creditResource;
	}

	@Override
	public String process(Object credential) {
		String token = null;
		if(credential != null) try{
			if(Boolean.TRUE.equals(checkProcessor.process(credential))) {
				if(creditProcessor != null) credential = creditProcessor.process(credential);
				if(genProcessor != null) token = genProcessor.process(credential);
				else token = TypeConvertor.randomUuid();
				if(token != null) creditResource.store(credential, token);
			}
		}catch(Exception e){
			log.error("CasAuth: cas auth error.", e);
		}
		return token;
	}
}
