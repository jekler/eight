package net.yeeyaa.eight.common.aop;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;
import net.yeeyaa.eight.ITriProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ByteClassProcessor extends ClassLoader implements IProcessor<byte[], Class<?>>, IReadonlyListable<Object, Class<?>>, IBiProcessor<Object, Object, Object>, ITriProcessor<byte[], Object, Object, Class<?>>, Comparator<Object[]> {
	protected final Logger log;
    protected Boolean needPackage = false;
    protected final ConcurrentHashMap<String, Class<?>> classes = new ConcurrentHashMap<String, Class<?>>();
    protected IBiProcessor<Object, Object, Object> beanHolder;
    
    public ByteClassProcessor(){
    	this.log = LoggerFactory.getLogger(ByteClassProcessor.class);
    }
    
	public ByteClassProcessor(ClassLoader parent, Logger log) {
		super(parent);
		this.log = log == null ? LoggerFactory.getLogger(ByteClassProcessor.class) : log;
	}

	public void setNeedPackage(Boolean needPackage) {
		if(needPackage != null) this.needPackage = needPackage;
	}

	public void setBeanHolder(IBiProcessor<Object, Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}
	
	@Override
	public synchronized Class<?> operate(byte[] data, Object name, Object needPackage) {
		if (needPackage == null) needPackage = this.needPackage;
		Class<?> c = null;
		if(data != null) try{
            c = defineClass(name == null ? null : name.toString(), data, 0, data.length);
    		String realname = c.getName();
            classes.put(realname, c);
        	if(Boolean.TRUE.equals(needPackage)){
	            int i = realname.lastIndexOf('.');
	            if (i != -1 ) try{
	                definePackage(realname.substring(0, i), null, null, null, null, null, null, null);
	            } catch (IllegalArgumentException iae) {}
        	}
		} catch(Throwable e) {
			log.error("ByteClassProcessor : load class error.", e);
		}
		return c;
	}
	
	@Override
	public Class<?> process(byte[] data) {
		return operate(data, null, needPackage);
	}

	@Override
	public Class<?> find(Object... paras) {
		if(paras != null && paras.length > 0 && paras[0] instanceof String) return classes.get(paras[0]);
		else return null;
	}

	@Override
	public Collection<Object[]> keys(Object... paras) {
		Collection<Object[]> ret = new ArrayList<Object[]>(classes.size());
		for(String name : classes.keySet()) ret.add(new String[]{name});
		return ret;
	}

	@Override
	public Map<Object[], Class<?>> all(Object... paras) {
		Map<Object[], Class<?>> ret = new HashMap<Object[], Class<?>>(classes.size());
		for(Entry<String, Class<?>> entry : classes.entrySet()) ret.put(new String[]{entry.getKey()}, entry.getValue());
		return ret;
	}

	public class GetClass implements IProcessor<String, Class<?>>{
		@Override
		public Class<?> process(String name) {
			try{
				return loadClass(name);
			}catch(Exception e){
				return null;
			}
		}	
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
}
