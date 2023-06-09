package net.yeeyaa.eight.core.event;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IListableTransaction;
import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;
import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.core.util.TypeConvertor;
import net.yeeyaa.eight.core.PlatformPool;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class PlatformEvent implements IProcessor<Object[], Collection<Object>>{
	protected final Logger log;
	protected IReadonlyListable<Object, Object> initResource;
	protected Object[] initParas;
	protected IListableTransaction<Object, Object, IResource<Object, Object>, Object> resource;
	protected Object[] paras;
	protected IProcessor<Object, Object> beanHolder;
	protected volatile ConcurrentHashMap<String, Collection<String>> beanMap = new ConcurrentHashMap<String, Collection<String>>();
	protected volatile ConcurrentHashMap<String, Collection<IProcessor<Object, Object>>> handlerMap = new ConcurrentHashMap<String,Collection<IProcessor<Object, Object>>>();
	protected IOutputResource<Object, Object> parent;
	protected Object key;
	protected Set<Object> eventPool;
	protected Integer threadMode = 0;
	protected Integer threadCount = 5;
	protected ExecutorService executor;
	protected String pool;	

	public PlatformEvent() {
		this.log = LoggerFactory.getLogger(PlatformEvent.class);
	}

	public PlatformEvent(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(PlatformEvent.class) : log;
	}
	
	public static class FireEvent implements IProcessor<Object, Collection<Object>>{
		protected Boolean cloneMsg = false;
		protected Boolean waitRet = false;
		protected IProcessor<Object[], Collection<Object>> agent;

		public void setAgent(IProcessor<Object[], Collection<Object>> agent) {
			this.agent = agent;
		}

		public void setCloneMsg(Boolean cloneMsg) {
			if(cloneMsg != null) this.cloneMsg = cloneMsg;
		}

		public void setWaitRet(Boolean waitRet) {
			if(waitRet != null) this.waitRet = waitRet;
		}

		@Override
		public Collection<Object> process(Object event) {
			return agent.process(new Object[]{cloneMsg, waitRet, event});
		}
	}
	
	protected static class Key{
		protected Object key;
		protected Object value;
		
		protected Key(Object key, Object value) {
			this.key = key;
			this.value = value;
		}

		public int hashCode() {
			int hash = 0;
			if(key != null) hash = key.hashCode();
			if(value != null) hash = hash * 17 + value.hashCode();
			return hash;
		}

		public boolean equals(Object obj) {
			if(this == obj) return true;
			else if(obj instanceof Key){
				Key other = (Key) obj;
				return ((key == other.key ||(key != null && key.equals(other.key))) && 
						(value == other.value || (value != null && value.equals(other.value))));
			}
			return false;
		}
	}
	
	protected static class Value{
		protected IProcessor<Object, Object> listener;
		protected Integer count = 1;
		
		protected Value(IProcessor<Object, Object> listener) {
			this.listener = listener;
		}
	}
	
	public class EventResource implements IOutputResource<Object, Object>{
		protected IProcessor<Object, IProcessor<Object, Object>> agentProcessor;
		protected HashMap<Key, Value> map = new  HashMap<Key, Value>();
		protected Boolean dupilcate = false;
		
		public void setAgentProcessor(IProcessor<Object, IProcessor<Object, Object>> agentProcessor) {
			this.agentProcessor = agentProcessor;
		}
		
		public void setDupilcate(Boolean dupilcate) {
			if(dupilcate != null) this.dupilcate = dupilcate;
		}

		@Override
		public <P> P store(Object value, Object... paras) {
			if(paras!= null && paras.length > 0 && value != null) if(value instanceof IProcessor) register(paras[0], (IProcessor<Object, Object>) value);
			else if(agentProcessor != null) synchronized(this) {
				Key key = new Key(paras[0],value);
				Value val = map.get(key);
				if(val == null) {
					IProcessor<Object, Object> processor = agentProcessor.process(value);
					if(processor != null){
						register(paras[0], processor);
						map.put(key,  new Value(processor));
					}
				}else if(dupilcate) {
					register(paras[0], val.listener);
					val.count++;
				}
			}else if(value instanceof String) register(paras[0], (String) value);
			return null;
		}

		@Override
		public <P> P discard(Object... paras) {
			if(paras!= null) if(paras.length > 1) {
				if(paras[1] instanceof IProcessor) unregister(paras[0], (IProcessor<Object, Object>) paras[1]);
				else if(agentProcessor != null) synchronized(this) {
					Key key = new Key(paras[0], paras[1]);
					Value value = map.get(key);
					if(value != null) {
						unregister(paras[0], value.listener);
						value.count--;
						if(value.count < 1) map.remove(key);
					}
				} else if(paras[1] instanceof String) unregister(paras[0], (String) paras[1]);
			}else if(paras.length > 0) unregister(paras[0]);
			return null;
		}

		@Override
		public <P> P empty(Object... paras) {
			PlatformEvent.this.clear();
			return null;
		}
	}

	public class ProxyResource implements IOutputResource<Object, Object>{
		@Override
		public <P> P store(Object value, Object... paras) {
			if(paras!= null && paras.length > 0 && value instanceof String) proxyRegister(paras[0], (String) value);
			return null;
		}

		@Override
		public <P> P discard(Object... paras) {
			if(paras!= null) if(paras.length > 1) {
				if(paras[1] instanceof String) proxyUnregister(paras[0], (String) paras[1]);
			}else if(paras.length > 0) unregister(paras[0]);
			return null;
		}

		@Override
		public <P> P empty(Object... paras) {
			proxyClear();
			return null;
		}
	}
	
	public void setPool(String pool) {
		this.pool = pool;
	}
	
	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}
	
	public void setBeanMap(ConcurrentHashMap<String, Collection<String>> beanMap) {
		this.beanMap = beanMap;
	}

	public void setInitResource(IReadonlyListable<Object, Object> initResource) {
		this.initResource = initResource;
	}

	public void setInitParas(Object[] initParas) {
		this.initParas = initParas;
	}

	public void setResource(IListableTransaction<Object, Object, IResource<Object, Object>, Object> resource) {
		this.resource = resource;
	}

	public void setParas(Object[] paras) {
		this.paras = paras;
	}

	public void setParent(IOutputResource<Object, Object> parent) {
		this.parent = parent;
	}

	public void setKey(Object key) {
		this.key = key;
	}

	public void setExecutor(ExecutorService executor) {
		this.executor = executor;
	}

	@PostConstruct
	public void initialize() {
		load();
		refreshParent();
	}

	public void refreshParent(){
		if(parent != null && key != null) synchronized(this){
			Set<Object> tmp = new HashSet<Object>();
			tmp.addAll(beanMap.keySet());
			tmp.addAll(handlerMap.keySet());
			if(resource != null) {
				Collection<Object[]> keys = paras == null ? resource.keys() : resource.keys(paras);
				if(keys != null) tmp.addAll(keys);
			}
			if(eventPool == null) {
				for(Object event : tmp) parent.store(key, event);
				eventPool = tmp;
			} else {
				Set<Object> backup = new HashSet<Object>(tmp);
				tmp.removeAll(eventPool);
				for(Object event : tmp) parent.store(key, event);
				tmp = eventPool;
				eventPool = backup;
				tmp.removeAll(eventPool);
				for(Object event : tmp) parent.discard(event, key);
			}
		}
	}
	
	protected void afterRegister(Object event){
		if(parent != null && key != null) synchronized(this){
			if(!eventPool.contains(event)) {
				parent.store(key, event);
				eventPool.add(event);
			}
		}
	}
	
	public class RefreshParent implements IProcessor<Object, Object>{
		@Override
		public Object process(Object instance) {
			refreshParent();
			return instance;
		}
	}
	
	protected void load(){
		if(initResource != null) {
			Map<Object[], Object> o = initParas == null ? initResource.all() : initResource.all(initParas);
			if(o != null) for(Entry<Object[], Object> entry : o.entrySet()) if(entry.getKey() != null && entry.getValue() != null && entry.getKey().length > 0 && entry.getKey()[0] != null){
				String event = entry.getValue().toString();
				Collection<String> list = beanMap.get(event);
				if(list == null) {
					list = new CopyOnWriteArrayList<String>();
					beanMap.put(event, list);
				}
				list.add(entry.getKey()[0].toString());
			}
		}
	}
	
	@PreDestroy
	public void destroy(){
		if(parent != null && key != null && eventPool != null && eventPool.size() > 0) for(Object o : eventPool) if(o != null) parent.discard(o, key);
		eventPool = null;
		if (executor != null) {
			executor.shutdown();
			executor = null;
		}
	}
	
	protected static class SubCaller implements Callable<Object>{
		protected IProcessor<Object, Object> processor;
		protected Object event;

		protected SubCaller(IProcessor<Object, Object> processor, Object event) {
			this.processor = processor;
			this.event = event;
		}
		
		@Override
		public Object call() {
			if(processor != null) return processor.process(event);
			else return null;
		}
	}
	
	public Collection<Object> fireEvent(Object event, Boolean cloneMsg, Boolean waitRet){
		if(event != null) try{
			String keyword = TypeConvertor.toString(event);
			Method m = null;
			if(Boolean.TRUE.equals(cloneMsg) && event instanceof Cloneable) m = event.getClass().getMethod("clone");
			LinkedList<SubCaller> ls = new LinkedList<SubCaller>();
			Collection<IProcessor<Object, Object>> listeners = handlerMap.get(keyword);
			if(listeners != null) for(IProcessor<Object, Object> processor : listeners) if(IProcessor.class.isInstance(processor)) 
				if(m != null) ls.add(new SubCaller(processor, m.invoke(event)));
				else ls.add(new SubCaller(processor, event));
			Collection<String> beans = beanMap.get(keyword);
			if(beans != null) for(String bean : beans) {
				Object processor = beanHolder.process(bean);
				if(processor instanceof IProcessor) if(m != null) ls.add(new SubCaller((IProcessor)processor, m.invoke(event)));
					else ls.add(new SubCaller((IProcessor)processor, event));
			}
			if(resource != null)if(paras != null && paras.length > 0){
				Object[] newparas = Arrays.copyOf(paras, paras.length + 1);
				newparas[paras.length] = keyword;
				Object o = resource.find(newparas);
				if(Collection.class.isInstance(o)) beans = (Collection) o;
				else beans = null;
			}else{
				Object o =  resource.find(keyword);
				if(o == null) o = new LinkedList<String>();
				if(Collection.class.isInstance(o)) beans = (Collection) o;
				else beans = null;
			}
			if(beans != null) for(String bean : beans) {
				Object processor = beanHolder.process(bean);
				if(IProcessor.class.isInstance(processor)) if(m != null) ls.add(new SubCaller((IProcessor)processor, m.invoke(event)));
					else ls.add(new SubCaller((IProcessor)processor, event));
			}
			if(ls.size() > 0){
				Boolean newExecutor = false;
				ExecutorService exec = null;
				if(threadMode == 1) { 
					if(pool != null) {
						Object o = beanHolder.process(pool);
						if(o instanceof ExecutorService) exec = (ExecutorService)o;
					} 
				}else if(threadMode == 2) exec = executor;
				else if(threadMode == 3) {
					if (threadCount > 0) exec = PlatformPool.getPool(threadCount);
					else  exec = PlatformPool.getCachedPool();
					newExecutor = true;
				}
				LinkedList<Object> ret = new LinkedList<Object>();
				LinkedList<Future<Object>> futures = new LinkedList<Future<Object>>();
				for(SubCaller runner : ls)try{
		            if(exec != null && !exec.isShutdown()) futures.add(exec.submit(runner));
		            else ret.add(runner.call());
		        } catch (Exception e) {   
		            log.error("PlatformEvent: event failed.", e);            
				}
				if(newExecutor) exec.shutdown();
				if(Boolean.TRUE.equals(waitRet)) for(Future<Object> future : futures) try{
					ret.add(future.get());
				}catch(Exception e){
		            log.error("PlatformEvent: event failed.", e); 		
				}
				if(Boolean.TRUE.equals(waitRet)) return ret;
			}
		}catch(Exception e){
            log.error("PlatformEvent: event failed.", e); 
		}
		return null;
	}
	
	public void register(Object event, IProcessor<Object, Object> handler){
		if(event != null && handler != null) {
			String keyword = TypeConvertor.toString(event);
			Collection<IProcessor<Object, Object>> ls = handlerMap.get(keyword);
			if (ls == null) {
				ls = new CopyOnWriteArrayList<IProcessor<Object, Object>>();
				handlerMap.put(keyword, ls);
			}
			ls.add(handler);
			afterRegister(event);
		}
	}
	
	public void unregister(Object event, IProcessor<Object, Object> handler){
		if(event != null && handler != null){
			String keyword = TypeConvertor.toString(event);
			Collection<IProcessor<Object, Object>> ls = handlerMap.get(keyword);
			if(ls != null){
				ls.remove(handler);
				if(ls.size() == 0) handlerMap.remove(keyword);
			}
		}
	}	
	
	public void register(Object event, String handler){
		if(event != null && handler != null){
			String keyword = TypeConvertor.toString(event);
			Collection<String> ls = beanMap.get(keyword);
			if (ls == null) {
				ls = new CopyOnWriteArrayList<String>();
				beanMap.put(keyword, ls);
			}
			ls.add(handler);
			afterRegister(event);
		}
	}
	
	public void unregister(Object event, String handler){
		if(event != null && handler != null){
			String keyword = TypeConvertor.toString(event);
			Collection<String> ls = beanMap.get(keyword);
			if(ls != null){
				ls.remove(handler);
				if(ls.size() == 0) beanMap.remove(keyword);
			}
		}
	}
	
	public void proxyRegister(final Object event, final String handler){
		if(event != null && handler != null && resource != null){
			String keyword = TypeConvertor.toString(event);
			Object[] newparas = null;
			if(paras != null && paras.length > 0){
				newparas = Arrays.copyOf(paras, paras.length + 1);
				newparas[paras.length] = keyword;
			}else newparas = new Object[]{keyword};
			final Object[] paras = newparas;
			resource.execute(new IProcessor<IResource<Object, Object>, Object>(){
				@Override
				public Object process(IResource<Object, Object> instance) {
					Object o = instance.find(paras);
					if(o == null) o = new LinkedList<String>();
					if(Collection.class.isInstance(o)){
						((Collection)o).add(handler);
						instance.store(o, paras);
						afterRegister(event);
					}
					return null;
				}				
			});
		}
	}
	
	public void proxyUnregister(Object event, final String handler){
		if(event != null && handler != null && resource != null){
			String keyword = TypeConvertor.toString(event);
			Object[] newparas = null;
			if(paras != null && paras.length > 0){
				newparas = Arrays.copyOf(paras, paras.length + 1);
				newparas[paras.length] = keyword;
			}else newparas = new Object[]{keyword};
			final Object[] paras = newparas;
			resource.execute(new IProcessor<IResource<Object, Object>, Object>(){
				@Override
				public Object process(IResource<Object, Object> instance) {
					Object o = instance.find(paras);
					if(o == null) o = new LinkedList<String>();
					if(Collection.class.isInstance(o)){
						((Collection)o).remove(handler);
						if(((Collection)o).size() == 0) instance.discard(paras);
						else instance.store(o, paras);
					}
					return null;
				}				
			});
		}
	}
	
	public void unregister(Object event){
		if(event != null){
			String keyword = TypeConvertor.toString(event);
			if(beanMap != null) beanMap.remove(keyword);
			if(handlerMap != null) handlerMap.remove(keyword);
			if(resource != null) if (paras != null && paras.length > 0){
				Object[] newparas = Arrays.copyOf(paras, paras.length + 1);
				newparas[paras.length] = keyword;
				resource.discard(newparas);
			}else resource.discard(keyword);
		}
	}
	
	public void clear() {
		if(beanMap != null) beanMap = new ConcurrentHashMap<String, Collection<String>>();
		if(handlerMap != null) handlerMap = new ConcurrentHashMap<String,Collection<IProcessor<Object, Object>>>();
	}
	
	public void proxyClear(){
		if(resource != null){
			if(paras != null) resource.empty(paras);
			else resource.empty();
		}
	}

	public class Destroy implements IProcessor<Object, Object>{
		public Object process(Object in) {
			destroy();
			return in;
		}
	}

	@Override
	public Collection<Object> process(Object[] instance) {
		if(instance != null && instance.length > 2 && instance[0] instanceof Boolean && instance[1] instanceof Boolean && instance[2] != null) 
			return fireEvent(instance[2], (Boolean)instance[0], (Boolean)instance[1]);
		else return null;
	}
}
