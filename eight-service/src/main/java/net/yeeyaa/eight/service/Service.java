package net.yeeyaa.eight.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.common.util.CommonUtil;
import net.yeeyaa.eight.core.PlatformPool;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class Service implements IProcessor<ServiceMsg, ServiceMsg>{
	protected final Logger log;
	protected IBiProcessor<String, Object, Object> bi;
	protected IProcessor<Object, Object> processor;
	protected Boolean listener = false;
	
	public Service() {
		log = LoggerFactory.getLogger(Service.class);
	}

	public Service(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(Service.class) : log;
	}
	
	public void setListener(Boolean listener) {
		if (listener != null) this.listener = listener;
	}

	public void setBi(IBiProcessor<String, Object, Object> bi) {
		this.bi = bi;
		if (bi instanceof IProcessor) processor = (IProcessor<Object, Object>) bi;
	}
	
	@Override
	public ServiceMsg process(ServiceMsg msg) {
		if (msg != null && msg.name != null) {
			ServiceMsg ret = new ServiceMsg(msg.token, msg.name, msg.id, null, null);
			try{
				ret.content = bi.perform(msg.name, msg.content);
				if (listener) return null;
			}catch(PlatformException e){
				ret.error = e.getType();
				log.error(msg.name + ": service invoke error.", e);
			}catch(Exception e){
				ret.error = ServiceError.SERVICE_PERFORMING_FAIL;
				log.error(msg.name + ": service invoke error.", e);
			}
			return ret;
		} else throw new PlatformException(ServiceError.ERROR_QUERY_MSG);
	}
	
	public static class ServiceInfo{
		public Integer min;
		public Integer max;
		public Boolean overflow = false;
		public Integer type;
		public String name;
		public String token;
		
		@Override
		public boolean equals(Object obj) {
			if(this == obj) return true;
			else if(obj instanceof ServiceInfo){
				ServiceInfo other = (ServiceInfo) obj;
				return ((type == other.type ||(type != null && type.equals(other.type))) && 
						(name == other.name || (name != null && name.equals(other.name))) &&
						(token == other.token || (token != null && token.equals(other.token))));
			}
			return false;
		}
		
		@Override
		public int hashCode() {
			int hash = 0;
			if(type != null) hash = type.hashCode();
			if(name != null) hash = hash * 19 + name.hashCode();
			if(token != null) hash = hash * 13 + token.hashCode();
			return hash;
		}
	}
	
	public class Dock implements IProcessor<List<ServiceMsg>, List<ServiceMsg>>{
		protected LinkedHashMap<String, String> services;
		protected LinkedHashSet<ServiceInfo> serviceInfos;
		protected Map<String, Set<ServiceInfo>> tokenMap;
		protected Map<String, ServiceInfo> tokenScope;
		protected Map<ServiceInfo, Set<ServiceInfo>> tnMap;	
		protected Boolean ignoreError = true;	
		protected Boolean strictMode = false;
		protected Integer threadCount = 3;
		protected Integer threadMode = 0;
		protected ExecutorService executor;
		protected IProcessor<Object, ExecutorService> pool;
		
		public void setIgnoreError(Boolean ignoreError) {
			if(ignoreError != null) this.ignoreError = ignoreError;
		}

		public void setStrictMode(Boolean strictMode) {
			if(strictMode != null) this.strictMode = strictMode;
		}

		public void setThreadCount(Integer threadCount) {
			if(threadCount != null && threadCount >= 0) this.threadCount = threadCount;
		}

		public void setThreadMode(Integer threadMode) {
			if(threadMode != null) this.threadMode = threadMode;
		}

		public void setPool(IProcessor<Object, ExecutorService> pool) {
			this.pool = pool;
		}

		public void setExecutor(ExecutorService executor) {
			this.executor = executor;
		}

		public void setServices(LinkedHashMap<String, String> services) {
			this.services = services;
		}

		@Override
		public List<ServiceMsg> process(List<ServiceMsg> query) {
			if(serviceInfos != null && query != null && query.size() > 0){
				LinkedHashMap<ServiceInfo, LinkedList<ServiceMsg>> dowith = new LinkedHashMap<ServiceInfo, LinkedList<ServiceMsg>>();
				HashMap<ServiceInfo, ServiceInfo> kset = new HashMap<ServiceInfo, ServiceInfo>();
				for(ServiceInfo si : serviceInfos) {
					dowith.put(si, new LinkedList<ServiceMsg>());
					kset.put(si, si);
				}
				for(ServiceMsg bean : query) {
					Object sw = processor == null ? null : processor.process(bean.name);
					if(sw != null){
						Boolean loc = false;
						ServiceInfo key = new ServiceInfo();
						key.name = bean.name;
						key.token = bean.token;
						key.type = 0;
						if(key.token != null){
							if(tokenMap.containsKey(key.token)){
								Set<ServiceInfo> set = tokenMap.get(key.token);
								if(set.contains(key)) {
									LinkedList<ServiceMsg> ls = dowith.get(key);
									if(kset.get(key).max == null || ls.size() < kset.get(key).max) {
										ls.add(bean); 
										loc = true;
									}else if(!kset.get(key).overflow) if(strictMode) throw new PlatformException(ServiceError.ERROR_QUERY_MSG);
									else loc = true;
								}
								if(!loc){
									key.name = sw instanceof IBiProcessor ? ((IBiProcessor<Object, Object, String>)sw).perform(null, null) : CommonUtil.getRealClass(sw).getName();
									key.type = 1;
									if(set.contains(key)) {
										LinkedList<ServiceMsg> ls = dowith.get(key);
										if(kset.get(key).max == null || ls.size() < kset.get(key).max) {
											ls.add(bean); 
											loc = true;
										}else if(!kset.get(key).overflow) if(strictMode) throw new PlatformException(ServiceError.ERROR_QUERY_MSG);
										else loc = true;
									}
								}
							}
						}else{
							if(dowith.containsKey(key)) {
								LinkedList<ServiceMsg> ls = dowith.get(key);
								if(kset.get(key).max == null || ls.size() < kset.get(key).max) {
									ls.add(bean); 
									loc = true;
								}else if(!kset.get(key).overflow) if(strictMode) throw new PlatformException(ServiceError.ERROR_QUERY_MSG);
								else loc = true;
							}else if(tnMap.containsKey(key)) for(ServiceInfo si : tnMap.get(key)){
								LinkedList<ServiceMsg> ls = dowith.get(si);
								if(si.max == null || ls.size() < si.max) {
									ls.add(bean); 
									loc = true;
									break;
								}else if(!si.overflow) if(strictMode) throw new PlatformException(ServiceError.ERROR_QUERY_MSG);		
								else loc = true;
							}
							if(!loc){
								key.name = sw instanceof IBiProcessor ? ((IBiProcessor<Object, Object, String>)sw).perform(null, null) : CommonUtil.getRealClass(sw).getName();
								key.type = 1;
								if(dowith.containsKey(key)) {
									LinkedList<ServiceMsg> ls = dowith.get(key);
									if(kset.get(key).max == null || ls.size() < kset.get(key).max) {
										ls.add(bean); 
										loc = true;
									}else if(!kset.get(key).overflow) if(strictMode) throw new PlatformException(ServiceError.ERROR_QUERY_MSG);
									else loc = true;
								}else if(tnMap.containsKey(key)) for(ServiceInfo si : tnMap.get(key)){
									LinkedList<ServiceMsg> ls = dowith.get(si);
									if(si.max == null || ls.size() < si.max) {
										ls.add(bean); 
										loc = true;
										break;
									}else if(!si.overflow) if(strictMode) throw new PlatformException(ServiceError.ERROR_QUERY_MSG);		
									else loc = true;
								}
							}		
						}
						if(strictMode && !loc) throw new PlatformException(ServiceError.ERROR_QUERY_MSG);
					} else  if(strictMode) throw new PlatformException(ServiceError.ERROR_QUERY_MSG);
				}
				Map<String, Integer> m = new HashMap<String, Integer>();
				for(String token : tokenScope.keySet()) m.put(token, 0);
				for(Entry<ServiceInfo, LinkedList<ServiceMsg>> entry : dowith.entrySet()) {
					if(entry.getKey().min > entry.getValue().size()) throw new PlatformException(ServiceError.ERROR_QUERY_MSG);
					String token = entry.getKey().token;
					if(m.containsKey(token)) m.put(token, m.get(token) + 1);
				}
				for(Entry<String, Integer> entry : m.entrySet()) {
					ServiceInfo si = tokenScope.get(entry.getKey());
					if(si.max < entry.getValue() || si.min > entry.getValue()) throw new PlatformException(ServiceError.ERROR_QUERY_MSG);
				}
				List<ServiceMsg> ret = new ArrayList<ServiceMsg>(query.size());
				if(threadMode != 0) {
					Boolean newExecutor = false;
					ExecutorService exec = null;
					if(threadMode == 1) { 
						if(pool != null) {
							Object obj = pool.process(null);
							if(obj instanceof ExecutorService) exec = (ExecutorService)obj;
						}
					} else if(threadMode == 2) exec = executor;
	 				if(executor == null || executor.isShutdown()) {
	 					if (threadCount > 0) exec = PlatformPool.getPool(threadCount);
	 					else  exec = PlatformPool.getCachedPool();
	 					newExecutor = true;
	 				}
					CompletionService<ServiceMsg> completionService = new ExecutorCompletionService<ServiceMsg>(exec);
					int count = 0;
					for(Entry<ServiceInfo, LinkedList<ServiceMsg>> entry : dowith.entrySet()) for(final ServiceMsg qry : entry.getValue()){
						count ++;
		                completionService.submit(new Callable<ServiceMsg>(){
							@Override
							public ServiceMsg call() throws Exception {
								return Service.this.process(qry);
							}	                	
		                });  
					}
					if(newExecutor) exec.shutdown();
					for(int i = 0; i < count; i++) try{
						ret.add(completionService.take().get());
					}catch(Exception e){
						log.error("Service Dock: async fetch result error!");
						if(!ignoreError) if (e instanceof PlatformException) throw (PlatformException) e;
						else throw new PlatformException(ServiceError.SERVICE_PERFORMING_FAIL, e);
					}
				}else for (Entry<ServiceInfo, LinkedList<ServiceMsg>> entry : dowith.entrySet()) for(ServiceMsg qry : entry.getValue()) {
					ret.add(Service.this.process(qry));
				}
			    return ret;
			} else throw new PlatformException(ServiceError.ERROR_QUERY_MSG);
		}
		
		@PreDestroy
		public void destroy(){
			if(executor != null) executor.shutdown();
			executor = null;
		}
		
		public class Destroy implements IProcessor<Object, Object>{
			@Override
			public Object process(Object instance) {
				destroy();
				return instance;
			}			
		}
		
		@PostConstruct
		public void init(){
			if(services != null){
				LinkedHashSet<ServiceInfo> tmp = new LinkedHashSet<ServiceInfo>();
				Map<String, Set<ServiceInfo>> tokenMap = new HashMap<String, Set<ServiceInfo>>();
				Map<String, ServiceInfo> tokenScope = new HashMap<String, ServiceInfo>();
				Map<ServiceInfo, Set<ServiceInfo>> tnMap = new HashMap<ServiceInfo, Set<ServiceInfo>>();
				for(Entry<String, String> entry : services.entrySet()){
					String range = entry.getValue().trim();
					if (range.matches("(\\[|\\()\\d+,(INFINITE|\\d+)(\\]|\\))")){
						ServiceInfo si = new ServiceInfo();
						String[] nums = range.substring(1, range.length() - 1).split(",");
						si.min = new Integer(nums[0]);
						if(range.startsWith("(")) {
							si.min++;
						}
						if(si.min < 0) si.min = 0;
						if(!"INFINITE".equalsIgnoreCase(nums[1])){
							si.max = new Integer(nums[1]);
							if(range.endsWith(")")){
								si.max--;
							}
							if(si.max < si.min){
								si.max = si.min;
							}
						}
						String[] instr = entry.getKey().split(":");
						if("class".equals(instr[0])){
							si.type = 1;
							if(instr.length > 1) si.name = instr[1];
							if(instr.length > 2) for(int i = 2; i < instr.length; i++){
								if(!si.overflow && "@overflow".equals(instr[i])) si.overflow = true;
								else if(instr[i].indexOf('@') != -1) si.token = instr[i];
							}
						} else if(instr[0].indexOf("@") != -1){
							si.type = 99;
							si.name = instr[0];
							tokenScope.put(instr[0], si);
						}else {
							si.type = 0;
							si.name = instr[0];
							if(instr.length > 1) for(int i = 1; i < instr.length; i++){
								if(!si.overflow && "@overflow".equals(instr[i])) si.overflow = true;
								else if(instr[i].indexOf('@') != -1) si.token = instr[i];
							}					
						}
						if(si.name != null && si.name.length() > 0 && si.type != 99) {
							if(si.token != null) {
								ServiceInfo newsi = new ServiceInfo();
								newsi.name = si.name;
								newsi.type = si.type;
								if(tnMap.containsKey(newsi)) tnMap.get(newsi).add(si);
								else{
									Set<ServiceInfo> set = new LinkedHashSet<ServiceInfo>();
									set.add(si);
									tnMap.put(newsi, set);			
								}
								if(tokenMap.containsKey(si.token)) tokenMap.get(si.token).add(si);
								else {
									Set<ServiceInfo> set = new HashSet<ServiceInfo>();
									set.add(si);
									tokenMap.put(si.token, set);
								}
							}
							tmp.add(si); 
						}
					}
				}
				serviceInfos = tmp;
				this.tokenScope = tokenScope;
				this.tokenMap = tokenMap;
				this.tnMap = tnMap;
			}
		}
	}
}
