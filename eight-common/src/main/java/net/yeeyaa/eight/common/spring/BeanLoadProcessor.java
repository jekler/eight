package net.yeeyaa.eight.common.spring;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.common.util.CommonUtil;


public class BeanLoadProcessor extends ClassLoader implements IProcessor<String, Class<?>> {
    protected IProcessor<String, Class<?>> classLoader;
	protected IProcessor<Object, Object> beanHolder; 
    protected Boolean retry = false;
    
	public BeanLoadProcessor() {}

	public BeanLoadProcessor(ClassLoader parent) {
		super(parent);
	}
    
	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}

	public void setRetry(Boolean retry) {
		if(retry != null) this.retry = retry;
	}

	public void setClassLoader(IProcessor<String, Class<?>> classLoader) {
		this.classLoader = classLoader;
	}
	
	@Override
	public Class<?> process(String className) {
		Class<?> ret = null;
		try{
			try{
				ret = super.loadClass(className, false);
			}catch(Exception e){}
			if(ret == null) {
				String[] names = className.split("\\.");
				String beanname = names[names.length - 1];
				names = beanname.split("\\$");
				if(names.length > 1) beanname = names[0];
				if(beanname.length() > 0) beanname = new StringBuilder().append(beanname.substring(0,1).toLowerCase()).append(beanname.substring(1)).toString();
				Object o = beanHolder.process(beanname);
				if(o != null) if(o.getClass().getName().contains(".$Proxy")) ret = CommonUtil.getRealObject(o).getClass();
				else ret = o.getClass();
				if(names.length > 1 && ret != null) ret = ret.getClassLoader().loadClass(className);
			}
		}catch(Exception e){
			return null;
		}
		return ret;
	}

	@Override
	protected Class<?> loadClass(String className, boolean resolveClass) throws ClassNotFoundException {
		Class<?> ret = null;
		if(classLoader != null) ret = classLoader.process(className);
		else ret = process(className);
		if(ret == null && retry) ret = super.loadClass(className, resolveClass);
		if(ret == null) throw new ClassNotFoundException(className);
		return ret;
	}
}
