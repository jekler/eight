package net.yeeyaa.eight.osgi.runtime;

import groovy.lang.GroovyClassLoader;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Dictionary;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IUniversal;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.storage.ByteWStorage;
import net.yeeyaa.eight.core.util.MemoryClassLoader;
import net.yeeyaa.eight.core.util.TypeConvertor;
import net.yeeyaa.eight.osgi.IBundleProxy;
import net.yeeyaa.eight.osgi.IBundleService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class BundleLinker implements IBundleProxy, Comparable<BundleLinker>{
	protected static final byte GZIP_MAGIC = 31;
	protected static final byte JAR_MAGIC = 80;
	protected static final byte CLZ_MAGIC = -54;	
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
	protected final Logger log;
	protected volatile CountDownLatch startSignal = new CountDownLatch(1);
	protected volatile IBundleService service;
	protected String name;
	protected String bean;
	protected String entry = "eight.link.Linker";
	protected String field = "service";
	protected int mode; //0 wait 1 no wait
 	protected Long wait = 5000L;
	protected int rank;
	protected volatile Object proxy = linker;

	public BundleLinker() {
		log  = LoggerFactory.getLogger(BundleLinker.class);
	}

	public BundleLinker(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(BundleLinker.class) : log;
	}
    
    protected void updated(Dictionary<String, Object> d) {
    	if (proxy != linker && d.get("proxy") == null) proxy = linker; 
    }
	
    public synchronized void setProxy(String code) {
    	if (code != null && code.length() > 0) try {
	    	byte[] bin = TypeConvertor.decode(code);
	    	Class<?> linker;
	    	if (JAR_MAGIC == bin[0]) {
	    		ByteWStorage jar = new ByteWStorage(bin, "link.jar");
	    		MemoryClassLoader loader = new MemoryClassLoader(getClass().getClassLoader(), log);
	    		loader.new AddResource().process(jar);
	    		linker = loader.loadClass(entry);
	    	} else {
	    		if (GZIP_MAGIC == bin[0]) bin = TypeConvertor.decompress(bin, -1024, null);
	    		if (CLZ_MAGIC == bin[0]) {
	    			ByteWStorage clz = new ByteWStorage(bin, entry + ".class");
		    		MemoryClassLoader loader = new MemoryClassLoader(getClass().getClassLoader(), log);
		    		loader.new AddResource().process(clz);
		    		linker = loader.loadClass(entry);
	    		} else {
	    			GroovyClassLoader loader = new GroovyClassLoader(getClass().getClassLoader());
	    			linker = loader.parseClass(new String(bin, "UTF-8"));
	    		}
	    	}
	    	final Object proxy = linker.newInstance();
	    	Field f = linker.getDeclaredField(field);
	    	if(!f.isAccessible()) f.setAccessible(true);
			f.set(proxy, this.linker);
			this.proxy = proxy;
    	} catch (Exception e) {
    		log.error("BundleLinker: init proxy fail.", e);
    	}
	}

	public void setEntry(String entry) {
		if (entry != null) this.entry = entry;
	}

	public void setField(String field) {
		if (field != null) this.field = field;
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
	
    protected void bindService(IBundleService service){
		if(service != null) {
			this.service = service;
			startSignal.countDown();
		}
	}
	
    protected synchronized void unbindService(){
		if(startSignal.getCount() == 0)	startSignal = new CountDownLatch(1);
		this.service = null;
	}

    protected IBundleService getService(){
		if(service != null) return service;
		else if(this.mode == 0) try{
			if(wait == 0) startSignal.await();
			else startSignal.await(wait, TimeUnit.MILLISECONDS);
		}catch(Exception e){
			log.error("BundleLinker: performing fail.", e);
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
	public Object getProxy() {
		return proxy;
	}
}
