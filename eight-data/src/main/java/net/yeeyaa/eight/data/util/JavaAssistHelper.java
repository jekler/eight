package net.yeeyaa.eight.data.util;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import net.yeeyaa.eight.IProcessor;

import javassist.util.proxy.ProxyFactory;
import javassist.util.proxy.ProxyFactory.ClassLoaderProvider;

public class JavaAssistHelper implements IProcessor<Object, ClassLoaderProvider>{
	protected IProcessor<Map<String, Object>, ClassLoader> classLoader;
	
	public void setClassLoader(IProcessor<Map<String, Object>, ClassLoader> classLoader) {
		this.classLoader = classLoader;
	}

	public void initialize() {
		ProxyFactory.classLoaderProvider = process(null);
	}

	@Override
	public ClassLoaderProvider process(Object instance) {
		return new ProxyFactory.ClassLoaderProvider() {
			@Override
			public ClassLoader get(ProxyFactory pf) {
		    	Class<?> superClass = pf.getSuperclass();
		    	Class<?>[] interfaces = pf.getInterfaces();
		        HashSet<ClassLoader> set = new HashSet<ClassLoader>();
		        if (superClass != null && !superClass.getName().equals("java.lang.Object")) set.add(superClass.getClassLoader());
		        if (interfaces != null && interfaces.length > 0) for (Class<?> itfc : interfaces) set.add(itfc.getClassLoader());
		        ClassLoader[] loaders = set.toArray(new ClassLoader[set.size()]);
	            ClassLoader selfLoader = pf.getClass().getClassLoader();
	            if (selfLoader == null) {
	            	selfLoader = Thread.currentThread().getContextClassLoader();
	                if (selfLoader == null) selfLoader = ClassLoader.getSystemClassLoader();
	            }
				if (classLoader == null) return new ProxyClassLoader(selfLoader, loaders);
				else {
					Map<String, Object> map = new HashMap<String, Object>();
					map.put("loaders", loaders);
					map.put("backup", selfLoader);
					return classLoader.process(map);
				}
			}
		};
	}
	
	protected static class ProxyClassLoader extends ClassLoader{
		protected ClassLoader[] proxy;
		
		public ProxyClassLoader(ClassLoader parent, ClassLoader[] proxy) {
			super(parent);
			this.proxy = proxy;
		}

		@Override
		protected Class<?> loadClass(String className, boolean resolveClass) throws ClassNotFoundException {
			if(proxy != null && proxy.length > 0) for (ClassLoader cl : proxy) if(cl != null) try {
				return cl.loadClass(className);
			} catch (Exception e) {}
			return super.loadClass(className, resolveClass);
		}
	}
}
