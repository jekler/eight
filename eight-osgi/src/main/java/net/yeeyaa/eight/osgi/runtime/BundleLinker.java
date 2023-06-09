package net.yeeyaa.eight.osgi.runtime;

import groovy.lang.GroovyClassLoader;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Dictionary;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IUniversal;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.resource.MemoryLResource;
import net.yeeyaa.eight.core.storage.ByteWStorage;
import net.yeeyaa.eight.core.util.MemoryClassLoader;
import net.yeeyaa.eight.osgi.IBundleLinker;
import net.yeeyaa.eight.osgi.IBundleService;
import net.yeeyaa.eight.osgi.loader.config.ConfigHandler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

 
public class BundleLinker implements IBundleLinker, IProcessor<Object, Object>, Comparable<BundleLinker>{	
	protected final Object linker = new IUniversal<Object, Object, IListableResource<Object, Object>, Object, Object>(){
		@Override
		public Object process(Object object) {
			return getService().service(bean, "process", object);
		}

		@Override
		public Object perform(Object first, Object second) {
			return getService().service(bean, "perform", new Object[]{first, second});
		}

		@Override
		public Object operate(Object first, Object second, Object third) {
			return getService().service(bean, "operate", new Object[]{first, second, third});
		}

		@Override
		public Object find(Object... paras) {
			return getService().service(bean, "find", paras);
		}

		@Override
		public <P> P store(Object value, Object... paras) {
			return (P) getService().service(bean, "store", new Object[]{value, paras});
		}

		@Override
		public <P> P discard(Object... paras) {
			return (P) getService().service(bean, "delete", paras);
		}

		@Override
		public <P> P empty(Object... paras) {
			return (P) getService().service(bean, "empty", paras);
		}

		@Override
		public Collection<Object[]> keys(Object... paras) {
			return (Collection<Object[]>) getService().service(bean, "keys", paras);
		}

		@Override
		public Map<Object[], Object> all(Object... paras) {
			return (Map<Object[], Object>) getService().service(bean, "all", paras);
		}

		@Override
		public Object execute(IProcessor<IListableResource<Object, Object>, Object> processor) {
			return getService().service(bean, "execute", processor);
		}

		@Override
		public Collection<Object> methods() {
			return (Collection<Object>) getService().service(bean, "methods", null);
		}

		@Override
		public <N> N extend(Object method) {
			return (N) getService().service(bean, "extend", method);
		}

		@Override
		public <L> L present(Class<L> clazz) {
			return (L) getService().service(bean, "present", clazz);
		}

		@Override
		public <O> O realObject() {
			return (O) getService().service(bean, "realObject", null);
		}
	};
	protected final Logger logger;
	protected final MemoryLResource<Object, Object> storage = new MemoryLResource<Object, Object>();
	protected volatile CountDownLatch startSignal = new CountDownLatch(1);
	protected volatile IBundleService service;
	protected String name;
	protected String bean;
	protected int mode; 
 	protected Long wait = 5000L;
	protected int rank;
	protected volatile IProcessor<Dictionary<String, Object> , Object> updater;
	protected volatile IBiProcessor<IBundleService, Map<Object, Object> , Object> binder;
	protected volatile IBiProcessor<IBundleService, Map<Object, Object> , Object> unbinder;	
	protected volatile Object proxy = linker;

	public BundleLinker() {
		logger  = LoggerFactory.getLogger(BundleLinker.class);
	}

	public BundleLinker(Logger logger) {
		this.logger = logger == null ? LoggerFactory.getLogger(BundleLinker.class) : logger;
	}
    
    protected void updated(Dictionary<String, Object> d) {
    	if (updater == null || !Boolean.FALSE.equals(updater.process(d))) {
    		if (proxy != linker && d.get("proxy") == null) proxy = linker; 
    		if (updater != null && d.get("updater") == null) updater = null; 
    		if (binder != null && d.get("binder") == null) binder = null; 
    		if (unbinder != null && d.get("unbinder") == null) unbinder = null; 
    	}
    }
	
    protected void setUpdater(String code) {
    	Object updater = parse(code, "eight.linker.Updater", logger, this);
    	if (updater instanceof IProcessor) this.updater = (IProcessor<Dictionary<String, Object> , Object>) updater;
    }
    
    protected void setBinder(String code) {
    	Object binder = parse(code, "eight.linker.Binder", logger, this);
    	if (binder instanceof IBiProcessor) this.binder = (IBiProcessor<IBundleService, Map<Object, Object> , Object>) binder;
    }

    protected void setUnbinder(String code) {
    	Object unbinder = parse(code, "eight.linker.Unbinder", logger, this);
    	if (unbinder instanceof IBiProcessor) this.unbinder = (IBiProcessor<IBundleService, Map<Object, Object> , Object>) unbinder;	
    }

    protected void setProxy(String code) {
    	this.proxy = parse(code, "eight.linker.Proxy", logger, this);
    }
    
    protected static Object parse(String code, String entry, Logger logger, Object target) {
    	Object ret = null;
    	if (code != null && code.length() > 0) try {
    		Boolean[] flag = new Boolean[1];
	    	byte[] bin = ConfigHandler.decode(code, flag);
	    	Class<?> linker;
	    	if (flag[0] == null) {
	    		ByteWStorage jar = new ByteWStorage(bin, "code.jar");
	    		MemoryClassLoader loader = new MemoryClassLoader(BundleLinker.class.getClassLoader(), logger);
	    		loader.new AddResource().process(jar);
	    		linker = loader.loadClass(entry);
	    	} else {
	    		if (flag[0]) {
	    			ByteWStorage clz = new ByteWStorage(bin, entry + ".class");
		    		MemoryClassLoader loader = new MemoryClassLoader(BundleLinker.class.getClassLoader(), logger);
		    		loader.new AddResource().process(clz);
		    		linker = loader.loadClass(entry);
	    		} else {
	    			GroovyClassLoader loader = new GroovyClassLoader(BundleLinker.class.getClassLoader());
	    			linker = loader.parseClass(new String(bin, "UTF-8"));
	    		}
	    	}
	    	ret = linker.newInstance();
	    	try {
		    	Field f = linker.getDeclaredField("context");
		    	if(!f.isAccessible()) f.setAccessible(true);
				f.set(ret, target);
	    	} catch (Exception e) {
	    		logger.debug("BundleLinker: cannot find context property.", e);
	    	}
    	} catch (Exception e) {
    		logger.error("BundleLinker: init code fail.", e);
    	}
		return ret;
	}

	protected void setWait(Long sec){
    	if(sec != null && sec >= 0) wait = sec;
    }
    
    protected void setRank(int rank) {
		this.rank = rank;
	}

	protected void setName(String name) {
		this.name = name;
		if (bean == null) bean = name;
	}
    
    @Override
    public String getName() {
		return name;
	}
    
    protected void setBean(String bean) {
		this.bean = bean;
	}

    protected void setMode(int mode) {
		this.mode = mode;
	}
	
    protected void bindService(IBundleService service, Map<Object, Object> properties){
		if(binder == null || !Boolean.FALSE.equals(binder.perform(service, properties))) {
			this.service = service;
			if (service != null) startSignal.countDown();
		}
	}
	
    protected synchronized void unbindService(IBundleService service, Map<Object, Object> properties){
    	if (unbinder == null || !Boolean.FALSE.equals(unbinder.perform(service, properties))) {
			if(startSignal.getCount() == 0)	startSignal = new CountDownLatch(1);
			this.service = null;
    	}
	}

    protected IBundleService getService(){
		if(service != null) return service;
		else if(this.mode == 0) try{
			if(wait == 0) startSignal.await();
			else startSignal.await(wait, TimeUnit.MILLISECONDS);
		}catch(Exception e){
			logger.error("BundleLinker: performing fail.", e);
		} finally{
			if(service != null) return service;
			else throw new PlatformException(BundleError.SERVICE_UNAVAILABLE);
		}else throw new PlatformException(BundleError.SERVICE_UNAVAILABLE);
	}

	@Override
	public int compareTo(BundleLinker o) {
		return rank - o.rank;
	}
	
	@Override
	public Object getLinker() {
		return proxy;
	}

	@Override
	public String toString() {
		return "BundleLinker [name=" + name + ", bean=" + bean + ", rank=" + rank + "]";
	}

	@Override
	public Object process(Object object) {
		if (object == null) return storage;
		else switch (object.hashCode()) {
			case 3377907 : return linker; 
			case -1097337456 : return logger;
			case 3373707 : return name;
			case 3019696 : return bean;
			case 3357091 : return mode;
			case 3641717 : return wait;
			case 3492908 : return rank;
			default : return service;
		}
	}
}
