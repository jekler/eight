package net.yeeyaa.eight.core.processor;

import java.util.Comparator;
import java.util.HashMap;
import java.util.regex.Pattern;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;
import net.yeeyaa.eight.core.util.TypeConvertor;


public class ClassAppendProcessor extends ClassLoader implements IProcessor<String, Object>, IBiProcessor<Object, Object, Object>, Comparator<Object[]> {
    protected IProcessor<String, String> realnameProcessor;
    protected IBiProcessor<String, Object, byte[]> classProcessor; 
	protected IProcessor<Class<?>, Object> newInstance;	
    protected Pattern ignore;
    protected Boolean needPackage = false;
    protected IBiProcessor<Object, Object, Object> beanHolder;
    
    public ClassAppendProcessor(){}
    
	public ClassAppendProcessor(ClassLoader parent) {
		super(parent);
	}

	public void setNeedPackage(Boolean needPackage) {
		if(needPackage != null) this.needPackage = needPackage;
	}

	public void setClassProcessor(IBiProcessor<String, Object, byte[]> classProcessor) {
		this.classProcessor = classProcessor;
	}

	public void setNewInstance(IProcessor<Class<?>, Object> newInstance) {
		this.newInstance = newInstance;
	}
	
	public void setRealnameProcessor(IProcessor<String, String> realnameProcessor) {
		this.realnameProcessor = realnameProcessor;
	}

	public void setBeanHolder(IBiProcessor<Object, Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}

	public void setIgnore(String ignore) {
		if(ignore != null) this.ignore = Pattern.compile(ignore);
	}

	@Override
	public Object perform(Object name, Object bean) {
		if (beanHolder == null) return null;
		else return beanHolder.perform(name, bean);
	}
	
	@Override
	public int compare(Object[] ret, Object[] para) {
		if (beanHolder == null || ret== null || ret.length < 1 || para == null || para.length < 2) return -1;
		else {
			ret[0] = beanHolder.perform(para[0], para[1]);
			return 0;
		}
	}
	
	@Override
	public Object process(String instance) {
		if(instance != null) try{
			Class<?> clz = loadClass(instance);
			if(clz != null) if (newInstance == null) return clz.newInstance();
			else return newInstance.process(clz);
		}catch(Exception e){
    		throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, "ClassAppendProcessor : get object error.", e);   	
		}
		return null;
	}
	
    @Override
	protected Class<?> loadClass(String name, boolean resolve)	throws ClassNotFoundException {
    	Class<?> c = null;
    	try {
    		ClassLoader parent = super.getParent();
    		if (parent == null) c = super.loadClass(name, false);
    		else c = parent.loadClass(name);
	    } catch (ClassNotFoundException e) {}
    	if(c == null) {
	    	String realname = realnameProcessor == null ? name : realnameProcessor.process(name);
	    	if (realname != null) {
		    	c = findLoadedClass(name);
		    	if(c == null && (ignore == null || !ignore.matcher(realname).matches())) synchronized(this) {
		    		c = findLoadedClass(name);
		    		if(c == null) try {
		    			HashMap<String, byte[]> superClass = new HashMap<String, byte[]>();
		    			superClass.put(realname, TypeConvertor.inputToBytes(getResourceAsStream(realname.replace('.', '/').concat(".class")), 8192, -1L));
		    			byte[] bytecode = classProcessor.perform(name, superClass);
	                	c = defineClass(name, bytecode, 0, bytecode.length);
		            	if(needPackage){
		    	            int i = name.lastIndexOf('.');
		    	            if (i != -1 ) try{
		    	                definePackage(name.substring(0, i), null, null, null, null, null, null, null);
		    	            } catch (IllegalArgumentException iae) {}
		            	}
		    		} catch (Throwable e) {
		    			throw new ClassNotFoundException("cannot find " + realname, e);
		    		}
		    	}
	    	}
    	}
    	if(c == null) throw new ClassNotFoundException(name);
    	else if (resolve) resolveClass(c);
		return c;
	}

    public class LoadClass implements IProcessor<String, Class<?>>{
    	protected Boolean resolve = false;
    	
		public void setResolve(Boolean resolve) {
			if (resolve != null) this.resolve = resolve;
		}

		@Override
		public Class<?> process(String instance) {
			if(instance != null) try{
				return loadClass(instance, resolve);
			} catch(Exception e) {
				throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, "ClassAppendProcessor : find class error.", e);
			}
			return null;
		}
    }
    
	public static class DefaultNameProcessor implements IProcessor<String, String>{
    	protected String prefix;
    	protected String suffix;
    	protected IProcessor<String, String> proxy;
    	
		public void setProxy(IProcessor<String, String> proxy) {
			this.proxy = proxy;
		}

		public void setPrefix(String prefix) {
			this.prefix = prefix;
		}

		public void setSuffix(String suffix) {
			this.suffix = suffix;
		}

		@Override
		public String process(String name) {
			StringBuilder sb = new StringBuilder();
			if(prefix != null) sb.append(prefix);
			if(proxy != null) name = proxy.process(name);
			else if(name != null) {
				int point = name.indexOf("$$");
				if(point > -1) name = name.substring(0, point);
				else return null;
			}
	        if(name != null) sb.append(name);
	        if(suffix != null) sb.append(suffix);
			return sb.toString();
		}  	
    }
}
