package net.yeeyaa.eight.aspect.auth.filter;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.util.TypeConvertor;
import net.yeeyaa.eight.service.ServiceError;
import net.yeeyaa.eight.service.ServiceMsg;


public class SsoFilter implements IProcessor<ServiceMsg, ServiceMsg>{
	public static final String SSO_KEY = "sso.key";
	protected IProcessor<ServiceMsg, ServiceMsg> next;
	protected IProcessor<Object, String> secProcessor;
	protected IProcessor<Void, Object> genProcessor;
	protected Object idKey = "userid";
	protected IProcessor<Object, Object> idProcessor;
	protected IResource<Object, Object> session;
	
	public void setSession(IResource<Object, Object> session) {
		if(session != null) this.session = session;
	}
	
	public void setIdProcessor(IProcessor<Object, Object> idProcessor) {
		this.idProcessor = idProcessor;
	}

	public void setIdKey(Object idKey) {
		if(idKey != null) this.idKey = idKey;
	}

	public void setNext(IProcessor<ServiceMsg, ServiceMsg> next) {
		this.next = next;
	}

	public void setSecProcessor(IProcessor<Object, String> secProcessor) {
		this.secProcessor = secProcessor;
	}

	public void setGenProcessor(IProcessor<Void, Object> genProcessor) {
		this.genProcessor = genProcessor;
	}

	@Override
	public ServiceMsg process(ServiceMsg msg) {
		try{
			msg = next.process(msg);
			if(ServiceError.NO_RIGHT_TO_ACCESS_THE_RESOURCE.equals(msg.error)) {
				Object userid = null;
				if(idProcessor != null) userid = idProcessor.process(idKey);
				else userid = session.find(idKey);
				if(userid == null) {
					Object caskey = session.find(SSO_KEY);
					if(caskey == null){
						if(genProcessor != null) caskey = genProcessor.process(null);
						else caskey = TypeConvertor.randomUuid();
						session.store(caskey, SSO_KEY);
					}
					if(secProcessor != null) caskey = secProcessor.process(caskey);
					msg.setParameter(SSO_KEY, caskey);
				}
			}
			return msg;
		}catch(PlatformException e){
			if(ServiceError.NO_RIGHT_TO_ACCESS_THE_RESOURCE.equals(e.getType())){
				Object userid = null;
				if(idProcessor != null) userid = idProcessor.process(idKey);
				else userid = session.find(idKey);
				if(userid == null) {
					Object caskey = session.find(SSO_KEY);
					if(caskey == null){
						if(genProcessor != null) caskey = genProcessor.process(null);
						else caskey = TypeConvertor.randomUuid();
						session.store(SSO_KEY, caskey);
					}
					if(secProcessor != null) caskey = secProcessor.process(caskey);
					msg.setParameter(SSO_KEY, caskey);
					msg.content = null;
					msg.error = e.getType();
					return msg;
				}
			}
			throw e;
		}
	}
}
