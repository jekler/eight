package net.yeeyaa.eight.osgi.runtime;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.lang.ref.WeakReference;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IListable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IThing;
import net.yeeyaa.eight.ITriProcessor;
import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.ITransaction;
import net.yeeyaa.eight.IUniversal;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.resource.MemoryLResource;
import net.yeeyaa.eight.core.util.MapperSet;
import net.yeeyaa.eight.core.util.PlatformUtil;
import net.yeeyaa.eight.core.util.TypeConvertor;
import net.yeeyaa.eight.osgi.IBundleLinker;
import net.yeeyaa.eight.osgi.IBundleService;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleException;
import org.osgi.framework.BundleListener;
import org.osgi.framework.Constants;
import org.osgi.framework.Filter;
import org.osgi.framework.FrameworkListener;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceListener;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.support.AbstractApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.context.support.FileSystemXmlApplicationContext;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.context.support.GenericXmlApplicationContext;
import org.springframework.context.support.StaticApplicationContext;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;


public class BundleCenter implements IBundleService, IProcessor<Object, Object>, ITriProcessor<String, String, Object, Object>, IListableResource<Object, String>, IExtendable<Object>, BundleContext {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected static enum MethodType{execute, find, store, delete, empty, keys, all, process, perform, operate, realObject, extend, methods, present;}
	protected final Logger logger;
	protected final BundleContext bundleContext;
	protected final MemoryLResource<Object, Object> storage = new MemoryLResource<Object, Object>();
	protected final Map<String, List<IBundleLinker>> serviceMap = new ConcurrentHashMap<String, List<IBundleLinker>>();
	protected final Map<Task, Object> tasks = new ConcurrentHashMap<Task, Object>();
	protected final Set<String> unavailable = Collections.newSetFromMap(new ConcurrentHashMap<String, Boolean>());
	protected volatile Map<String, Object> properties = new ConcurrentHashMap<String, Object>();
	protected volatile Map<String, String> config = new ConcurrentHashMap<String, String>();
	protected volatile Map<String, Set<String>> permit = new ConcurrentHashMap<String, Set<String>>(); 
	protected volatile CountDownLatch startSignal = new CountDownLatch(1);
	protected volatile BeanHolder beanHolder;
	protected volatile Map<String, Object> paras;
	protected volatile ConcurrentLinkedQueue<WeakReference<Object[]>> cache;
	protected volatile Object[] tmp;
	protected volatile boolean state; 
	protected Integer trace = 0;
	protected Boolean clone;
	protected Long wait = 5000L;
	protected ExecutorService executor; 
	protected Boolean thread;
	protected BundleService<Object> hook;
	protected String resource;
	protected int mode; 
	protected String path;
	protected String pattern;
	protected Boolean recurse;
	protected String log;
	protected String holder;
	protected String context;
	protected String begin;
	protected String close;
	protected Boolean reload; 
	protected Boolean readonly;
	protected String hookid;
	protected String key;
	protected volatile IProcessor<Dictionary<String, Object> , Object> updater;
	protected volatile ITriProcessor<String, String, Object, Object> proxy;
	protected volatile IBiProcessor<ExecutorService, Map<Object, Object> , Object> executorBinder;
	protected volatile IBiProcessor<ExecutorService, Map<Object, Object> , Object> executorUnbinder;
	protected volatile IBiProcessor<IBundleLinker, Map<Object, Object> , Object> binder;
	protected volatile IBiProcessor<IBundleLinker, Map<Object, Object> , Object> unbinder;
	protected volatile IProcessor<Void, Object> validater;
	protected volatile IProcessor<Void, Object> invalidater;
	protected volatile IProcessor<ServiceReference<?>, Object> register;
	protected volatile IProcessor<ServiceReference<?>, Object> unregister;

	protected final IProcessor<Object, Object> count = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object paras) {
			return new Long(config.size());
		}
	};
	protected final IProcessor<Object, Object> info = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object object) {
			if (object == null) return storage;
			else switch(object.hashCode()) {
				case -991722469 : return permit;
				case 3377907 : return BundleCenter.this; 
				case 1576232653 : return bundleContext;
				case -926053069 : return properties;
				case -1354792126 : return config;
				case -1724780953 : return serviceMap;
				case -1097337456 : return logger;
				case 110132110 : return tasks;
				case -665462704 : return unavailable;
				case 106436755 : return paras;
				case 94416770 : return cache;
				case 114967 : return tmp;
				case 107332 : return log;
				case 3357091 : return mode;
				case 3641717 : return wait;
				case 109757585 : return state;
				case 110620997 : return trace;
				case 94756189 : return clone;
				case 2043017427 : return executor;
				case -874443254 : return thread;
				case 3208483 : return hook;
				case -341064690 : return resource;
				case 3433509 : return path;
				case -791090288 : return pattern;
				case 951530927 : return context;
				case -1211707988 : return holder;
				case 93616297 : return begin;
				case 94756344 : return close;
				case -934641255 : return reload;
				case -866730430 : return readonly;
				case -1211611778 : return hookid;
				case 106079 : return key;
				default : return beanHolder;
			}
		}
	};
	
	
	protected class BeanHolder implements IProcessor<Object, Object>{
		protected AbstractApplicationContext applicationContext;
		protected IProcessor<Object, Object> proxy;
		
		protected BeanHolder(AbstractApplicationContext applicationContext, IProcessor<Object, Object> proxy) {
			this.applicationContext = applicationContext;
			this.proxy = proxy;
		}

		@Override
		public Object process(Object name) {
			try{
				if(name != null) if (proxy != null) return proxy.process(name);
				else if (applicationContext.containsBean(name.toString())) return applicationContext.getBean(name.toString());
			}catch(Exception e){
				getLogger().error("BundleCenter: no sunch bean.", e);
			}
			return null;
		}
		
		protected void destroy(){
			try {
				applicationContext.stop();
			} finally {
				applicationContext.close();
			}
		}
	}

	public BundleCenter(BundleContext bundleContext) {
		this.bundleContext = bundleContext;
		this.logger = LoggerFactory.getLogger(BundleCenter.class);
	}

	public BundleCenter(BundleContext bundleContext, Logger logger) {
		this.bundleContext = bundleContext;
		this.logger = logger == null ? LoggerFactory.getLogger(BundleCenter.class) : logger;
	}
	
    public static String decompress(String src) throws IOException {
        return new String(TypeConvertor.decompress(TypeConvertor.decode(src), -1024, null), "UTF-8");
    }
	
	protected void setTrace(Integer trace) {
		if (trace != null) this.trace = trace;
		if (trace == 0) cache = null;
	}

	protected void setClone(Boolean clone) {
		this.clone = clone;
	}
	
	protected void setWait(Integer sec){
    	if(sec != null && sec >= 0) wait = sec * 1000L;
    }

	protected Logger getLogger() {
		if (log != null && beanHolder != null) try {
			Object logger = beanHolder.process(log);
			if (logger instanceof Logger) return (Logger) logger;
		} catch (Exception e) {
			logger.error("BundleCenter: log fail.", e);
		}
		return logger;
	}

	protected void setThread(Boolean thread) {
		this.thread = thread;
	}

	protected void setLog(String log) {
		this.log = log;
	}

	protected void setResource(String resource) {
		this.resource = resource;
	}

	protected void setMode(Integer mode) {
		this.mode = mode;
	}

	protected void setPath(String path) {
		this.path = path;
	}

	protected void setPattern(String pattern) {
		this.pattern = pattern;
	}

	protected void setRecurse(Boolean recurse) {
		this.recurse = recurse;
	}

	protected void setHolder(String holder) {
		this.holder = holder;
	}

	protected void setContext(String context) {
		this.context = context;
	}

	protected void setBegin(String begin) {
		this.begin = begin;
	}

	protected void setClose(String close) {
		this.close = close;
	}

	protected void setReload(Boolean reload) {
		this.reload = reload;
	}

	protected Map<String, List<IBundleLinker>> getServiceMap() {
		return serviceMap;
	}

	protected void setReadonly(Boolean readonly) {
		this.readonly = readonly;
	}

	protected void setHookid(String hookid) {
		this.hookid = hookid;
	}

	protected void setKey(String key) {
		this.key = key;
	}
	
	
	protected void bindExecutor(ExecutorService executor, Map<Object, Object> properties) {
		if (executorBinder == null || !Boolean.FALSE.equals(executorBinder.perform(executor, properties))) this.executor = executor;
	}

	protected void unbindExecutor(ExecutorService executor, Map<Object, Object> properties) {
		if (executorUnbinder == null || !Boolean.FALSE.equals(executorUnbinder.perform(executor, properties))) this.executor = null;
	}
	
    protected void bindLinker(IBundleLinker service, Map<Object, Object> properties){
		if (binder == null || !Boolean.FALSE.equals(binder.perform(service, properties))) if(service != null && service.getName() != null) synchronized(serviceMap) {
			List list = serviceMap.get(service.getName());
			if(list == null) {
				list = new CopyOnWriteArrayList<IBundleLinker>();
				serviceMap.put(service.getName(), list);
			}
			if(service instanceof Comparable) {
				int index = Collections.binarySearch(list, service);
				if(index < 0) list.add(- index - 1, service);  
			} else if(!list.contains(service)) list.add(service);
		}
	}
	
    protected void unbindLinker(IBundleLinker service, Map<Object, Object> properties){
    	if (unbinder == null || !Boolean.FALSE.equals(unbinder.perform(service, properties))) if(service != null && service.getName() != null) synchronized(serviceMap) {
			unavailable.remove(service.getName());
			List<IBundleLinker> list = serviceMap.get(service.getName());
			if(list != null) {
				list.remove(service);
				if(list.isEmpty()) serviceMap.remove(service.getName());
			}
		}
	}
	
    protected void setConfig(String property){
    	ConcurrentHashMap config = new ConcurrentHashMap<String, String>();
    	if (property != null && property.length() > 0) if (property.indexOf('#') != -1) for(String p : property.split("##")){
    		String[] ps = p.split("#");
    		if(ps.length > 1){ 
    			String key = ps[0].replace("`b", "#").replace("`c", "`").trim();
    			String value = ps[1].replace("`b", "#").replace("`c", "`").trim();
    			if(key.length() > 0 && value.length() > 0) config.put(key, value);
    		}
    	} else try {
    		Properties properties = new Properties();
    		properties.load(new StringReader(decompress(property)));
    		config.putAll(properties);
    	} catch (Exception e) {
    		getLogger().error("BundleCenter: init config fail.", e);
    	}
    	this.config = config;
    }
 
    protected void setPermit(String property){
    	ConcurrentHashMap<String, Set<String>> permit = new ConcurrentHashMap<String, Set<String>>();
    	if (property != null && property.length() > 0) try {
    		if (property.indexOf('|') == -1) property = decompress(property);
	    	String[] properties = property.split("\\|\\|\\|");
	    	for(String p : properties) if (p.trim().length() > 0) {
	    		String[] ps = p.split("\\|\\|");
				String key = ps[0].trim();
				if(key.length() > 0){
	    			Set<String> set = new HashSet<String>();
	    			permit.put(key, set);
	    			if (ps.length > 1) for(String v : ps[1].split("\\|")) {
	    				String value = v.trim();
	    				if(value.length() > 0) set.add(value);
	    			}
				}
	    	}
    	} catch (Exception e) {
    		getLogger().error("BundleCenter: init permit fail.", e);
    	}
    	this.permit = permit;
    }
	
    protected void setLogger(String logger){
    	if (logger == null || logger.trim().length() == 0) this.properties = new ConcurrentHashMap<String, Object>();
    	else try {
    		if (logger.indexOf('|') == -1) logger = decompress(logger);
	    	String[] props = logger.split("\\|\\|");
	    	for(String p : props) if (p.trim().length() > 0) {
	    		String[] ps = p.split("\\|");
				String key = ps[0].trim();
				if(key.length() > 0 && ps.length > 1) properties.put(key, ps[1].trim());
	    	}
    	} catch (Exception e) {
    		getLogger().error("BundleCenter: init logger fail.", e);
    	}
    }
    
    protected void setUpdater(String code) {
    	Object updater = BundleLinker.parse(code, "eight.service.Updater", logger, info);
    	if (updater instanceof IProcessor) this.updater = (IProcessor<Dictionary<String, Object> , Object>) updater;
    }
    
    protected void setExecutorBinder(String code) {
    	Object executorBinder = BundleLinker.parse(code, "eight.service.ExecutorBinder", logger, info);
    	if (executorBinder instanceof IBiProcessor) this.executorBinder = (IBiProcessor<ExecutorService, Map<Object, Object> , Object>) executorBinder;
    }

    protected void setExecutorUnbinder(String code) {
    	Object executorUnbinder = BundleLinker.parse(code, "eight.service.ExcutorUnbinder", logger, info);
    	if (executorUnbinder instanceof IBiProcessor) this.executorUnbinder = (IBiProcessor<ExecutorService, Map<Object, Object> , Object>) executorUnbinder;	
    }

    protected void setValidater(String code) {
    	Object validater = BundleLinker.parse(code, "eight.service.Validater", logger, info);
    	if (validater instanceof IProcessor) this.validater = (IProcessor<Void, Object>) validater;
    }

    protected void setInvalidater(String code) {
    	Object invalidater = BundleLinker.parse(code, "eight.service.Invalidater", logger, info);
    	if (invalidater instanceof IProcessor) this.invalidater = (IProcessor<Void , Object>) invalidater;
    }
    
    protected void setBinder(String code) {
    	Object binder = BundleLinker.parse(code, "eight.service.Binder", logger, info);
    	if (binder instanceof IBiProcessor) this.binder = (IBiProcessor<IBundleLinker, Map<Object, Object> , Object>) binder;
    }

    protected void setUnbinder(String code) {
    	Object unbinder = BundleLinker.parse(code, "eight.service.Unbinder", logger, info);
    	if (unbinder instanceof IBiProcessor) this.unbinder = (IBiProcessor<IBundleLinker, Map<Object, Object> , Object>) unbinder;	
    }

    protected void setProxy(String code) {
    	Object proxy = BundleLinker.parse(code, "eight.service.Proxy", logger, info);
    	if (proxy instanceof ITriProcessor) this.proxy = (ITriProcessor<String, String, Object, Object>) proxy;	
    }
    
    protected void setRegister(String code) {
    	Object register = BundleLinker.parse(code, "eight.service.Register", logger, info);
    	if (register instanceof IProcessor) this.register = (IProcessor<ServiceReference<?>, Object>) register;
    }

    protected void setUnregister(String code) {
    	Object unregister = BundleLinker.parse(code, "eight.service.Unregister", logger, info);
    	if (unregister instanceof IProcessor) this.unregister = (IProcessor<ServiceReference<?>, Object>) unregister;	
    }

	protected Object trace(Integer trace, Boolean mode) {
		if (trace == null) return tmp == null;
		else if (trace >= 0) {
			Object[] tmp = this.tmp;
			ConcurrentLinkedQueue<WeakReference<Object[]>> cache = this.cache;
			Object[] arr= tmp == null ? cache == null ? new Object[0] : cache.toArray() : tmp;
			if (arr.length > trace) {
				Object[] ret = ((WeakReference<Object[]>) arr[trace]).get();
				if (ret != null && mode != null) {
					ret = ret.clone();
					if (mode) ret[3] = null;
					else ret[2] = null;
				}
				return ret;
			}
		} else switch(trace) {
			case -1 : ConcurrentLinkedQueue<WeakReference<Object[]>> cache = this.cache;
				tmp = cache == null ? null : cache.toArray();
				if (trace == 0) this.cache = null;
				else this.cache = new ConcurrentLinkedQueue<WeakReference<Object[]>>();
				break;
			case -2 : tmp = null;
				break;
			case -3 : ConcurrentLinkedQueue<WeakReference<Object[]>> c = this.cache;
				if (c == null) return null;
				else {
					LinkedList<Object[]> ls = new LinkedList<Object[]>();
					for(WeakReference<Object[]> ref : c) {
						Object[] ret = ref.get();
						if (ret != null && mode != null) {
							ret = ret.clone();
							if (mode) ret[3] = null;
							else ret[2] = null;
						}
						ls.add(ret);
					}
					return ls;
				}
			case -4 : Object[] t = this.tmp;
				if (t == null) return null;
				else {
					LinkedList<Object[]> ls = new LinkedList<Object[]>();
					for (Object ref : t) {
						Object[] ret = ((WeakReference<Object[]>)ref).get();
						if (ret != null && mode != null) {
							ret = ret.clone();
							if (mode) ret[3] = null;
							else ret[2] = null;
						}
						ls.add(ret);
					}
					return ls;
				}
			default: int index = - trace - 10;
				if (index >= 0) {
					Object[] tmp = this.tmp;
					ConcurrentLinkedQueue<WeakReference<Object[]>> cc = this.cache;
					Object[] arr= tmp == null ? cc == null ? new Object[0] : cc.toArray() : tmp;
					if (arr.length > index) {
						Object[] paras = ((WeakReference<Object[]>)arr[index]).get();
						if (paras != null && paras.length > 2) {
							paras = paras.clone();
							try {
								paras[3] = service((String)paras[0], (String)paras[1], paras[2]);
							} catch (Exception e) {
								paras[3] = e;
							}
							if (mode != null) if (mode) paras[3] = null;
							else paras[2] = null;
							return paras;
			}}}}
		return null;
	}
	
    protected void start(){
    	if (beanHolder == null) {
			BundleClassLoader cl = new BundleClassLoader();
			cl.setContext(bundleContext);
			AbstractApplicationContext applicationContext = null;
			String r = resource;
			if(r != null) {
				String[] rs = r.split(":");
				if(rs.length > 1 && "sys".equals(rs[0])) r = bundleContext.getProperty(rs[1]);
			}
			GenericApplicationContext parentContext = new StaticApplicationContext();
			parentContext.getBeanFactory().registerSingleton(context == null ? "context" : context, this);
			parentContext.refresh(); 
			switch(mode) {
				case 0: if(r != null){
							ClassPathXmlApplicationContext ac = new ClassPathXmlApplicationContext(parentContext);
							ac.setClassLoader(cl);
							ac.setConfigLocations(r.split(";"));
							ac.refresh();
							ac.start();
							applicationContext = ac;
						}else {
							Enumeration<URL> e = bundleContext.getBundle().findEntries(path, pattern, recurse);
							LinkedList<Resource> resources = new LinkedList<Resource>();
							if(e != null) while(e.hasMoreElements()) resources.add(new UrlResource(e.nextElement()));
							if(resources.size() > 0) {
								GenericXmlApplicationContext ac = new GenericXmlApplicationContext();
								ac.setParent(parentContext);
								ac.setClassLoader(cl);
								ac.load(resources.toArray(new Resource[resources.size()]));
								ac.refresh();
								ac.start();
								applicationContext = ac;
							}
						}
						break;
				case 1: if(r != null){
							FileSystemXmlApplicationContext ac = new FileSystemXmlApplicationContext(parentContext);
							ac.setClassLoader(cl);
							ac.setConfigLocations(r.split(";"));
							ac.refresh();
							ac.start();
							applicationContext = ac;
						}else {
							Enumeration<URL> e = bundleContext.getBundle().findEntries(path, pattern, recurse);
							LinkedList<Resource> resources = new LinkedList<Resource>();
							if(e != null) while(e.hasMoreElements()) resources.add(new UrlResource(e.nextElement()));
							if(resources.size() > 0) {
								GenericXmlApplicationContext ac = new GenericXmlApplicationContext();
								ac.setParent(parentContext);
								ac.setClassLoader(cl);
								ac.load(resources.toArray(new Resource[resources.size()]));
								ac.refresh();
								ac.start();
								applicationContext = ac;
							}
						}
						break;	
				case 2: if(r != null){
							ClassPathXmlApplicationContext ac = new ClassPathXmlApplicationContext(parentContext);
							ac.setClassLoader(cl);
							ac.setConfigLocations(r.split(";"));
							ac.refresh();
							ac.start();
							applicationContext = ac;
						}
						break;
				case 3: if(r != null){
							FileSystemXmlApplicationContext ac = new FileSystemXmlApplicationContext(parentContext);
							ac.setClassLoader(cl);
							ac.setConfigLocations(r.split(";"));
							ac.refresh();
							ac.start();
							applicationContext = ac;
						}
				case 4: break;	
				default :   Enumeration<URL> e = bundleContext.getBundle().findEntries(path, pattern, recurse);
							LinkedList<Resource> resources = new LinkedList<Resource>();
							if(e != null) while(e.hasMoreElements()) resources.add(new UrlResource(e.nextElement()));
							if(resources.size() > 0) {
								GenericXmlApplicationContext ac = new GenericXmlApplicationContext();
								ac.setParent(parentContext);
								ac.setClassLoader(cl);
								ac.load(resources.toArray(new Resource[resources.size()]));
								ac.refresh();
								ac.start();
								applicationContext = ac;
							}
			}
			if(applicationContext != null) try {
				if(begin != null && applicationContext.containsBean(begin)) {
					Object b = applicationContext.getBean(begin);
					if(b instanceof IProcessor) ((IProcessor<Object, Object>)b).process(this);
				}
				if(holder != null && applicationContext.containsBean(holder)) {
					Object h = applicationContext.getBean(holder);
					if(h instanceof IProcessor) beanHolder = new BeanHolder(applicationContext, (IProcessor<Object, Object>)h);
				}
				if(beanHolder == null) beanHolder = new BeanHolder(applicationContext, null);	
			} catch (Exception e) {
				getLogger().error("BundleCenter: init context fail.", e);
				try {
					applicationContext.stop();
				} finally {
					applicationContext.close();
				}
			}
    	}
    }
    
    protected void stop(){
    	if(beanHolder !=  null) try {
    		if (close != null ) {
				Object c = beanHolder.process(close);
				if(c instanceof IProcessor) ((IProcessor<Object, Object>)c).process(this);
    		} 
		} finally {
			try {
				beanHolder.destroy();
			} finally {
				beanHolder =  null;
			}
		}
    }
    
    protected class Task implements Runnable, IBiProcessor<Integer, Object, Object> {
    	protected final Boolean mode;
    	protected final Boolean status;

		public Task(Boolean mode, Boolean status) {
			this.mode = mode;
			this.status = status;
		}

		@Override
		public void run() {
			synchronized(tasks) {
				try {
					if (status == null) {	
			    		boolean flag = false;
			    		try {	
			    			if (startSignal.getCount() == 0){
				    			startSignal = new CountDownLatch(1);
				    			flag = true;
			    			}
			    			if (!Boolean.TRUE.equals(mode)) stop();
			    		} finally {
							try {
								if (!Boolean.FALSE.equals(mode)) start();
							} finally {
								if (flag) startSignal.countDown();
							}
			    		}
			    	} else if (status) try {
			    		if (startSignal.getCount() == 0) startSignal = new CountDownLatch(1);
			    		if (!Boolean.TRUE.equals(mode)) stop();
			    	} finally {
			    		if (!Boolean.FALSE.equals(mode)) try {
							start();
						} finally {
							startSignal.countDown();
						}
					} else if (mode == null) try{
						stop();
					} finally{
						start();
					} else if (mode) start();
					else stop();
				} finally {
					tasks.remove(this);
				}
			}
		}

		@Override
		public Object perform(Integer map, Object name) {
			return info(map, name);
		}
    }
    
    protected Object info(Integer map, Object name) {
		if (map == null && name == null) return properties;
		else if (map == null) return config;
		else if (name == null) return paras;
		else switch(map) {
			case 0 : return properties.get(name);
			case 1 : return config.get(name);
			case 2 : return paras == null ? null : paras.get(name);
			case 3 : return permit.get(name);
			default : return BundleCenter.this;
		}
	}

    protected Boolean control (final Boolean mode, final Boolean status, Boolean flag) {
    	if (flag == null) {
    		String f = System.getProperty("framework.center.useThread");
    		if (f != null) flag = "true".equals(f);
    	}
    	if (flag == null) if (executor == null) {
    		if (thread == null || !thread) flag = false;
    	} else {
    		if (thread == null || thread) flag = true;
    	} else if (flag && executor == null) flag = null;
    	Task task = new Task(mode, status);
    	if (flag == null) {
    		Thread t = new Thread(task);
    		tasks.put(task, t);
    		t.start();
    	} else if (flag) {
    		tasks.put(task, executor.submit(task));
    	}	else {
    		tasks.put(task, Thread.currentThread());
    		task.run();
    	}
    	return flag;
    }

	protected Boolean state(Boolean state, Boolean kill) {
		if (kill == null) if (state == null) return this.state; 
		else {
			this.state = state;
			return startSignal.getCount() > 0; 
		} else { 
			Boolean ret = tasks.size() > 0;
			if (kill && state != null) if (state) synchronized(tasks) {
				if (startSignal.getCount() == 0) startSignal = new CountDownLatch(1);
			} else  startSignal.countDown();
			else if (ret) {
				Iterator<Entry<Task, Object>> itr = tasks.entrySet().iterator();
				while (itr.hasNext()) try {
					Entry<Task, Object> entry = itr.next();
					if (kill || (state == null ?  entry.getKey().mode == null : state.equals(entry.getKey().mode))) { 
						itr.remove();																	
						if (entry.getValue() instanceof Thread) ((Thread)entry.getValue()).interrupt();
						else if (entry.getValue() instanceof Future) ((Future<?>)entry.getValue()).cancel(true);
					}
				} catch (Exception e) {
					getLogger().error("BundleCenter: init context fail.", e);
				}
			}
			return ret;
		}
	}
	
    protected void validate(){
    	if (validater == null || !Boolean.FALSE.equals(validater.process(null))) if (beanHolder == null) synchronized(tasks) {
    		if (beanHolder == null) control(true, true, null);
    	}
    }

    protected void invalidate(){
    	if (invalidater == null || !Boolean.FALSE.equals(invalidater.process(null))) if (beanHolder != null) synchronized(tasks) {
    		if (beanHolder != null) control(false, true, null);
    	}
    }
    
    protected synchronized void registered(ServiceReference<?> ref) {
    	if (unregister == null || !Boolean.FALSE.equals(unregister.process(ref))) if(hookid != null) {
			if (hook != null) hook.destroy();
			Hashtable<String, Object> table = new Hashtable<String, Object>();
			if(key == null) table.put(Constants.SERVICE_DESCRIPTION, hookid);
			else table.put(key, hookid);
			Object r = ref.getProperty(Constants.SERVICE_RANKING);
			if(r instanceof Integer) table.put(Constants.SERVICE_RANKING, r);
			hook = new BundleService<Object>(bundleContext, table, new IProcessor<Object[], Object>(){
				@Override
				public Object process(Object[] instance) {
					if(instance != null && instance.length > 1 && instance[0] instanceof String && instance[1] instanceof String) 
						return service((String)instance[0], (String)instance[1], instance.length > 2 ? instance[2] : null);
					else return null;
				}	
			}, new String[]{IProcessor.class.getName()});
			hook.initialize();
		}
	}

    protected synchronized void unregistered(ServiceReference<?> ref) {
    	if (register == null || !Boolean.FALSE.equals(register.process(ref))) if(hook != null){
    		hook.destroy();
	    	hook = null;
    	}
	}
    
    protected void updated(Dictionary<String, Object> d) {
    	if (updater == null || !Boolean.FALSE.equals(updater.process(d))) if(d != null) synchronized (unavailable) {
    		if (proxy != null && d.get("proxy") == null) proxy = null; 
    		if (updater != null && d.get("updater") == null) updater = null; 
    		if (binder != null && d.get("binder") == null) binder = null; 
    		if (unbinder != null && d.get("unbinder") == null) unbinder = null; 
    		if (executorBinder != null && d.get("executorBinder") == null) executorBinder = null; 
    		if (executorUnbinder != null && d.get("executorUnbinder") == null) executorUnbinder = null; 
    		if (validater != null && d.get("validater") == null) validater = null; 
    		if (invalidater != null && d.get("invalidater") == null) invalidater = null; 
    		if (register != null && d.get("register") == null) register = null; 
    		if (unregister != null && d.get("unregister") == null) unregister = null; 
    		if (d instanceof Comparator) {
    			Object[] in= new Object[1];
    			if (((Comparator<Object[]>)d).compare(in, in) == 0 && in[0] instanceof Map) this.properties.putAll((Map<String, Object>) in[0]);
    		}
    		ConcurrentHashMap<String, Object> all = new ConcurrentHashMap<String, Object>(d.size() * 2);
    		Enumeration<String> e = d.keys();
			while(e.hasMoreElements()) {
				String key = e.nextElement();
				all.put(key, d.get(key));
			}
	    	if(!Boolean.FALSE.equals(reload) && paras != null && !paras.equals(all)) {
	    		paras = all;
	    		control(null, reload, null);
	    	} else paras = all;	    	
    	}
    }
    
    protected Object debug(String name, String type, String factory, Object paras) {
    	try {
	    	if (paras instanceof Collection) paras = ((Collection<Object>)paras).toArray();
	    	if (factory == null || factory.length() == 0) return service(name, type, paras);
	    	else {
	    		Object stub = beanHolder.process(factory);
	    		if (stub instanceof IProcessor) {
	    			paras = ((IProcessor<Object, Object>) stub).process(paras);
	    			return service(name, type, paras);
	    		}
	    	}
    	} catch (Exception e) {
    		return e;
    	}
    	return null;
    }
    
	@Override
	public Object operate(String name, String type, Object paras) {
		try{		
			startSignal.await();
			if(beanHolder != null && (permit.size() == 0 || (permit.get(name) != null && (permit.get(name).size() == 0 || permit.get(name).contains(type))))){
				MethodType method = MethodType.valueOf(type);
				Object service = beanHolder.process(name);
				if(service != null) {
					Object key = trace == 0 ? null : clone == null ? paras : PlatformUtil.copy(paras);
					Object ret = null;
					try {
						switch (method) {
							case process: if(service instanceof IProcessor) return (ret = ((IProcessor)service).process(paras));
							break;
							case find: if(service instanceof IInputResource) {
								if(paras instanceof Object[]) return (ret = ((IInputResource)service).find((Object[])paras));
								else return (ret = ((IInputResource)service).find(paras));
							}
							break;
							case store: if(service instanceof IOutputResource && paras instanceof Object[]) {
								Object[] p = (Object[])paras;
								if(p.length > 1) return (ret = ((IOutputResource)service).store(p[0], (Object[])p[1]));
							}
							break;
							case delete: if(service instanceof IOutputResource) {
								if(paras instanceof Object[]) return (ret = ((IOutputResource)service).discard((Object[])paras));
								else return (ret = ((IOutputResource)service).discard(paras));
							}
							break;
							case empty: if(service instanceof IOutputResource) {
								if(paras == null) return (ret = ((IOutputResource)service).empty());
								else if(paras instanceof Object[]) return (ret = ((IOutputResource)service).empty((Object[])paras));
								else return (ret = ((IOutputResource)service).empty(paras));
							}
							break;
							case keys: if(service instanceof IListable){
								if(paras == null) return (ret = ((IListable)service).keys());
								else if(paras instanceof Object[]) return (ret = ((IListable)service).keys((Object[])paras));
								else return (ret = ((IListable)service).keys(paras));
							}
							break;
							case all: if(service instanceof IListable){
								if(paras == null) return (ret = ((IListable)service).all());
								else if(paras instanceof Object[]) return (ret = ((IListable)service).all((Object[])paras));
								else return (ret = ((IListable)service).all(paras));
							}
							break;
							case execute: if(service instanceof ITransaction) return (ret = ((ITransaction)service).execute((IProcessor)paras));
							break;				
							case perform: if(service instanceof IBiProcessor && paras instanceof Object[] && ((Object[])paras).length > 1) 
								return (ret = ((IBiProcessor)service).perform(((Object[])paras)[0], ((Object[])paras)[1]));
							break;
							case operate: if(service instanceof ITriProcessor && paras instanceof Object[] && ((Object[])paras).length > 2) 
								return (ret = ((ITriProcessor)service).operate(((Object[])paras)[0], ((Object[])paras)[1], ((Object[])paras)[2]));
							break;
							case extend: if(service instanceof IExtendable) return (ret = ((IExtendable)service).extend(paras));
							break;
							case methods: if(service instanceof IExtendable) return (ret = ((IExtendable)service).methods());
							break;
							case present: if(service instanceof IThing) return (ret = ((IThing)service).present((Class<?>)paras));
							break;
							case realObject: if(service instanceof IUniversal) return (ret = ((IUniversal)service).realObject());
						}
					} catch(Exception e){
						ret = e;
						throw e;
					} finally {
						if (trace == 0) cache = null;
						else {
							ConcurrentLinkedQueue<WeakReference<Object[]>> cache = this.cache;
							if (cache == null) synchronized(count) {
								if (this.cache == null) this.cache = new ConcurrentLinkedQueue<WeakReference<Object[]>>();
								cache = this.cache;
							}
							if ((ret instanceof Throwable || trace < 0) && cache.offer(new WeakReference<Object[]>(new Object[]{name, type, key, Boolean.TRUE.equals(clone) ? PlatformUtil.copy(ret) : ret}))) {
								int overflow = cache.size() - Math.abs(trace);
								if (overflow > 0) {
									for (int i = 0; i < overflow; i++) cache.poll();
									WeakReference<Object[]> r = cache.peek();
									while(r != null && r.get() == null) {
										cache.poll();
										r = cache.peek();
									}
								}
							}
						}
					}
				}
			}
		} catch(Exception e){
			getLogger().error("BundleCenter: performing fail.", e);
			if(e instanceof PlatformException) throw (PlatformException)e;
			else throw new PlatformException(BundleError.SERVICE_INVOKE_ERROR, e);
		}
		throw new PlatformException(BundleError.SERVICE_CANNOT_FIND);
	}
    
	@Override
	public Object service(String name, String type, Object paras) {
		return proxy == null ? operate(name, type, paras) : proxy.operate(name, type, paras);
	}

	protected Object getService(String name) {
		IBundleLinker service = null;
		List<IBundleLinker> list = serviceMap.get(name);
		if(list != null) try{
			int length = list.size();
			if(length > 0) service = list.get(length - 1);
		}catch(Exception e){}
    	if(service == null) {
    		if(this.wait > 0 && !unavailable.contains(name))try{
    			Thread.sleep(this.wait);
    		}catch(Exception e){
    			getLogger().error("BundleCenter: invoke fail.", e);
    		} finally{
    			list = serviceMap.get(name);
    			if(list != null) try{
    				int length = list.size();
    				if(length > 0) service = list.get(length - 1);
    			}catch(Exception e){}
    			if(service == null) unavailable.add(name);
    		}
    		if(service == null) throw new PlatformException(BundleError.SERVICE_NOT_EXIST);
    	}
    	return service.getLinker();
    }

	@Override
	public Object process(Object key) {
		if(beanHolder != null) return beanHolder.process(key);
		return null;
	}
	
	@Override
	public String find(Object ... paras) {
		if(paras != null && paras.length > 0) return config.get(paras[0]);
		else return null;
	}

	@Override
	public <P> P store(String value, Object ... paras) {
		if(Boolean.FALSE.equals(readonly) && paras != null && paras.length > 0 && paras[0] != null && value != null) config.put(paras[0].toString(), value);
		return null;
	}

	@Override
	public <P> P discard(Object ... paras) {
		if(Boolean.FALSE.equals(readonly) && paras != null && paras.length > 0) config.remove(paras[0]);
		return null;
	}

	@Override
	public <P> P empty(Object... paras) {
		if(Boolean.FALSE.equals(readonly)) config = new ConcurrentHashMap<String, String>();
		return null;
	}

	@Override
	public Collection<Object[]> keys(Object... paras) {
		Collection<String> c = config.keySet();
		Collection<Object[]> ls = new ArrayList<Object[]>(c.size());
		for(String o : c) {
			String[] key = new String[1];
			key[0] = o;
			ls.add(key);
		}
		return ls;
	}

	@Override
	public Map<Object[], String> all(Object... paras) {
		Map<Object[], String> map = new HashMap<Object[], String>(config.size());
		for(Entry<String, String> o : config.entrySet()) {
			String[] key = new String[1];
			key[0] = o.getKey();
			map.put(key, o.getValue());
		}
		return map;
	}

	@Override
	public <N> N extend(Object object) {
		if (object != null) {
			Object method = object instanceof ResourceMethod ? object : methods.process(object);
			if (method!= null) switch((ResourceMethod) method) {
				case count : return (N) count;
			}
		}
		return null;
	}
	
	@Override
	public Collection<Object> methods() {
		return methods;
	}
	
	@Override
	public String getProperty(String key) {
		return bundleContext.getProperty(key);
	}

	@Override
	public Bundle getBundle() {
		return bundleContext.getBundle();
	}

	@Override
	public Bundle installBundle(String location, InputStream input) throws BundleException {
		return bundleContext.installBundle(location, input);
	}

	@Override
	public Bundle installBundle(String location) throws BundleException {
		return bundleContext.installBundle(location);
	}

	@Override
	public Bundle getBundle(long id) {
		return bundleContext.getBundle(id);
	}

	@Override
	public Bundle[] getBundles() {
		return bundleContext.getBundles();
	}

	@Override
	public void addServiceListener(ServiceListener listener, String filter)	throws InvalidSyntaxException {
		bundleContext.addServiceListener(listener, filter);
	}

	@Override
	public void addServiceListener(ServiceListener listener) {
		bundleContext.addServiceListener(listener);
	}

	@Override
	public void removeServiceListener(ServiceListener listener) {
		bundleContext.removeServiceListener(listener);
	}

	@Override
	public void addBundleListener(BundleListener listener) {
		bundleContext.addBundleListener(listener);
	}

	@Override
	public void removeBundleListener(BundleListener listener) {
		bundleContext.removeBundleListener(listener);
	}

	@Override
	public void addFrameworkListener(FrameworkListener listener) {
		bundleContext.addFrameworkListener(listener);
	}

	@Override
	public void removeFrameworkListener(FrameworkListener listener) {
		bundleContext.removeFrameworkListener(listener);
	}

	@Override
	public ServiceRegistration<?> registerService(String[] clazzes, Object service, Dictionary<String, ?> properties) {
		return bundleContext.registerService(clazzes, service, properties);
	}

	@Override
	public ServiceRegistration<?> registerService(String clazz, Object service,	Dictionary<String, ?> properties) {
		return bundleContext.registerService(clazz, service, properties);
	}

	@Override
	public <S> ServiceRegistration<S> registerService(Class<S> clazz, S service, Dictionary<String, ?> properties) {
		return bundleContext.registerService(clazz, service, properties);
	}

	@Override
	public ServiceReference<?>[] getServiceReferences(String clazz,	String filter) throws InvalidSyntaxException {
		return bundleContext.getServiceReferences(clazz, filter);
	}

	@Override
	public ServiceReference<?>[] getAllServiceReferences(String clazz, String filter) throws InvalidSyntaxException {
		return bundleContext.getAllServiceReferences(clazz, filter);
	}

	@Override
	public ServiceReference<?> getServiceReference(String clazz) {
		return bundleContext.getServiceReference(clazz);
	}

	@Override
	public <S> ServiceReference<S> getServiceReference(Class<S> clazz) {
		return bundleContext.getServiceReference(clazz);
	}

	@Override
	public <S> Collection<ServiceReference<S>> getServiceReferences(Class<S> clazz, String filter) throws InvalidSyntaxException {
		return bundleContext.getServiceReferences(clazz, filter);
	}

	@Override
	public <S> S getService(ServiceReference<S> reference) {
		return bundleContext.getService(reference);
	}

	@Override
	public boolean ungetService(ServiceReference<?> reference) {
		return bundleContext.ungetService(reference);
	}

	@Override
	public File getDataFile(String filename) {
		return bundleContext.getDataFile(filename);
	}

	@Override
	public Filter createFilter(String filter) throws InvalidSyntaxException {
		return bundleContext.createFilter(filter);
	}

	@Override
	public Bundle getBundle(String location) {
		return bundleContext.getBundle(location);
	}

	@Override
	public String toString() {
		return "BundleCenter [hookid=" + hookid + "]";
	}
}
