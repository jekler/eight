package net.yeeyaa.eight.aspect.auth.filter;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.service.ServiceError;
import net.yeeyaa.eight.service.ServiceMsg;


public class AuthFilter implements IProcessor<ServiceMsg, ServiceMsg>{
	protected IProcessor<ServiceMsg, ServiceMsg> next;
	protected IResource<Object, UserInfo> cache;
	protected IProcessor<Void, Collection<Entry<Object, Entry<Map<Object, Object>, Collection<Object>>>>> serviceProcessor;
	protected IProcessor<Object, String[]> userProcessor;
	protected IProcessor<Object, Object> idProcessor;
	protected IProcessor<Object[], Collection<Object>> matchProcessor;
	protected Object idKey = "userid";
	protected Object defaultId;
	protected Integer authType = 0; 
	protected Integer refreshInterval = 0;
	protected volatile Map<Object, Entry<Map<Object, Object>, Collection<Object>>> config = new LinkedHashMap<Object, Entry<Map<Object, Object>, Collection<Object>>>(0);
	protected IResource<Object, Object> session;
	
	public class AuthLoader implements IProcessor<Void, Void>{
		public void initialize(){
			process(null);
		}
		
		@Override
		public Void process(Void instance) {
			Collection<Entry<Object, Entry<Map<Object, Object>, Collection<Object>>>> services = serviceProcessor.process(null);
			Map<Object, Entry<Map<Object, Object>, Collection<Object>>> config = new LinkedHashMap<Object, Entry<Map<Object, Object>, Collection<Object>>>(services == null ? 0 : services.size());
			if(services != null) for (Entry<Object, Entry<Map<Object, Object>, Collection<Object>>> service : services) if(service.getKey() != null) config.put(service.getKey(), service.getValue());
			AuthFilter.this.config = config;
			return null;
		}	
	}
	
	protected class UserInfo{
		protected String[] roles;
		protected Long timestamp;
	}
	
	public void setSession(IResource<Object, Object> session) {
		if(session != null) this.session = session;
	}
	
	public void setRefreshInterval(Integer refreshInterval) {
		if(refreshInterval != null && refreshInterval > 0) this.refreshInterval = refreshInterval * 1000;
	}

	public void setIdProcessor(IProcessor<Object, Object> idProcessor) {
		this.idProcessor = idProcessor;
	}
	
	public void setDefaultId(Object defaultId) {
		this.defaultId = defaultId;
	}

	public void setMatchProcessor(IProcessor<Object[], Collection<Object>> matchProcessor) {
		this.matchProcessor = matchProcessor;
	}

	public void setIdKey(Object idKey) {
		this.idKey = idKey;
	}

	public void setNext(IProcessor<ServiceMsg, ServiceMsg> next) {
		this.next = next;
	}

	public void setServiceProcessor(IProcessor<Void, Collection<Entry<Object, Entry<Map<Object, Object>, Collection<Object>>>>> serviceProcessor) {
		this.serviceProcessor = serviceProcessor;
	}

	public void setUserProcessor(IProcessor<Object, String[]> userProcessor) {
		this.userProcessor = userProcessor;
	}

	public void setAuthType(Integer authType) {
		if(authType != null && authType > 0) this.authType = authType;
	}
	
	public void setCache(IResource<Object, UserInfo> cache) {
		this.cache = cache;
	}

	@Override
	public ServiceMsg process(ServiceMsg msg) {
		try{
			Collection<Object> auths = null;
			if(matchProcessor != null) auths = matchProcessor.process(new Object[]{msg.name, config});
			else auths = config.get(msg.name) == null ? null : config.get(msg.name).getValue();
			if(auths == null && authType == 0) throw new PlatformException(ServiceError.NO_RIGHT_TO_ACCESS_THE_RESOURCE);
			if(auths != null && auths.size() == 0 && (authType == 0 || authType == 1 || authType == 2)) 
				throw new PlatformException(ServiceError.NO_RIGHT_TO_ACCESS_THE_RESOURCE);
			if((auths != null || (authType != 2 && authType != 4)) && (authType != 4 || auths.size() > 0)){
				Object userid = null;
				if(idProcessor != null) userid = idProcessor.process(idKey);
				else userid = session.find(idKey);
				if(userid == null) userid = defaultId;
				if(userid == null && ((auths != null && auths.size() > 0) || authType == 1 || authType == 3)) 
					throw new PlatformException(ServiceError.NO_RIGHT_TO_ACCESS_THE_RESOURCE);
				if(auths != null && auths.size() > 0){
					String[] roles = null;
					if (cache == null || refreshInterval == 0) roles = userProcessor.process(userid);
					else {
						UserInfo userInfo = cache.find(userid);
						long time = System.currentTimeMillis();
						if(userInfo == null || userInfo.timestamp + refreshInterval < time){
							userInfo = new UserInfo();
							userInfo.roles = userProcessor.process(userid);
							userInfo.timestamp = time;
							cache.store(userInfo, userid);
						}
						roles = userInfo.roles;
					}
					if(roles != null && roles.length > 0) {
						Boolean match = false;
						for(String role : roles) if(auths.contains(role)) {
							match = true;
							break;
						}
						if(!match) throw new PlatformException(ServiceError.NO_RIGHT_TO_ACCESS_THE_RESOURCE);
					}else throw new PlatformException(ServiceError.NO_RIGHT_TO_ACCESS_THE_RESOURCE);
				}
			}
		} catch(PlatformException e){
			throw e;
		} catch(Exception e){
			throw new PlatformException(ServiceError.NO_RIGHT_TO_ACCESS_THE_RESOURCE, e);
		}
		return next.process(msg);
	}
}
