package net.yeeyaa.eight.osgi.runtime;

import java.io.IOException;
import java.net.URL;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleReference;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;


public class BundleClassLoader extends ClassLoader implements IProcessor<String, Class<?>>, BundleReference {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected BundleContext context;
	protected Bundle bundle; 
	protected Boolean mode;
	
	public BundleClassLoader() {}

	public BundleClassLoader(ClassLoader parent) {
		super(parent);
	}

	public void setContext(BundleContext context) {
		this.context = context;
		if (context != null) bundle = context.getBundle();
 	}

	public void setMode(Boolean mode) {
		this.mode = mode;
	}

	@Override
	public URL getResource(final String name) {
		URL resource = AccessController.doPrivileged(new PrivilegedAction<URL>() {
			@Override
			public URL run() {
				return getBundle().getResource(name);
			}
		});
		if (resource == null) resource = super.getResource(name);
		return resource;
	}
	
	@Override
	 protected Enumeration<URL> findResources(final String name) throws IOException {
		Enumeration<URL> urls;
		try {
			urls = AccessController.doPrivileged(new PrivilegedExceptionAction<Enumeration<URL>>() {
				@Override
				public Enumeration<URL> run() throws IOException {
					return getBundle().getResources(name);
				}
			});
		} catch (PrivilegedActionException e) {
		      Exception cause = e.getException();
		      if (cause instanceof IOException) throw (IOException) cause;
		      throw new PlatformException(BundleError.CLASS_LOAD_ERROR, cause);
		}
		if (urls == null) urls = Collections.enumeration(Collections.<URL>emptySet());
		return urls;
	}
	
	@Override
	protected Class<?> loadClass(final String name, boolean resolve) throws ClassNotFoundException {
		Class<?> clazz;
		try {
			clazz = AccessController.doPrivileged(new PrivilegedExceptionAction<Class<?>>() {
				@Override
				public Class<?> run() throws ClassNotFoundException {
					return getBundle().loadClass(name);
				}
			});
		} catch (PrivilegedActionException cnfe) {
			clazz = super.loadClass(name, false);
		}
		if (resolve) resolveClass(clazz);
		return clazz;
	}

	@Override
	public Class<?> process(String name) {
		try {
			return loadClass(name, false);
		} catch (ClassNotFoundException e) {
		    throw new PlatformException(BundleError.CLASS_LOAD_ERROR, e);
		}
	}
	
	public BundleContext getContext(ClassLoader classLoader){
		BundleContext context = this.context;
		while(classLoader != null) {
			if(classLoader instanceof BundleClassLoader) {
				if(((BundleClassLoader)classLoader).context != null) context = ((BundleClassLoader)classLoader).context;
			}else if(classLoader instanceof BundleReference){
				Bundle bundle = ((BundleReference)classLoader).getBundle();
				if(bundle != null) context = bundle.getBundleContext();
			}
			if(context != null) break;
			classLoader = classLoader.getParent();
		}
		return context;
	}
	
	public class GetContext implements IProcessor<ClassLoader, BundleContext>{
		@Override
		public BundleContext process(ClassLoader instance) {
			return getContext(instance);
		}		
	}

	@Override
	public Bundle getBundle() {
		return getBundle(mode);
	}

	public Bundle getBundle(Boolean mode) {
		if (mode == null) return bundle;
		else if (mode) return context.getBundle();
		else {
			bundle = context.getBundle();
			return bundle;
		}
	}
	
	public class FindEntries implements IProcessor<Object[], URL[]>{
		@Override
		public URL[] process(Object[] paras) {
			if(paras.length > 1 && paras[0] instanceof String && paras[1] instanceof String) {
				Enumeration<URL> e;
				if(paras.length > 2 && paras[2] instanceof Boolean) e = getBundle().findEntries((String) paras[0], (String) paras[1], (Boolean) paras[2]); 
				else e = getBundle().findEntries((String) paras[0], (String) paras[1], false);
				LinkedList<URL> resources = new LinkedList<URL>();
				if(e != null) while(e.hasMoreElements()) resources.add(e.nextElement());
				return resources.toArray(new URL[resources.size()]);
			} else return null;
		}	
	}
	
	public class BundleResource implements IReadonlyListable<Object, IExtendable<Object>>, IExtendable<Object> {
		protected IProcessor<Resource, IExtendable<Object>> mockStorage;
		protected String filePattern;
		protected Boolean recurse = false;
		protected Boolean dir; 
		protected String base;
		protected final IProcessor<Object[], Object> count = new IProcessor<Object[], Object>(){
			@Override
			public Object process(Object[] paras) {
				StringBuilder path = new StringBuilder();
				if(base != null) path.append(base);
				if(paras != null && paras.length > 0 && paras[0] != null) path.append(paras[0].toString());
			    if(path.length() > 0 && '/' != path.charAt(path.length() - 1)) path.append('/');
				Enumeration<URL> e = getBundle().findEntries(path.toString(), filePattern, recurse);
				Long count = 0L;
				if(e != null) while(e.hasMoreElements()) if(dir == null || dir.equals(e.nextElement().getPath().endsWith("/"))) count ++;
				return count;
			}
		};
		
		public void setMockStorage(IProcessor<Resource, IExtendable<Object>> mockStorage) {
			this.mockStorage = mockStorage;
		}
		
		public void setBase(String base) {
			this.base = base;
		}

		public void setDir(Boolean dir) {
			this.dir = dir;
		}
		
		public void setFilePattern(String filePattern) {
			if(filePattern != null) this.filePattern = filePattern;
		}

		public void setRecurse(Boolean recurse) {
			if(recurse != null) this.recurse = recurse;
		}
		
		@Override
		public IExtendable<Object> find(Object... paras) {
			if(paras != null && paras.length > 0 && paras[0] != null && paras[0] instanceof URL) return mockStorage.process(new UrlResource((URL) paras[0]));
			else {
				StringBuilder p = new StringBuilder();
				if(base != null) p.append(base);
				if(paras != null) for (Object para : paras) if (para != null) p.append(para.toString());
				return mockStorage.process(new UrlResource(getBundle().getEntry(p.toString())));
			}
		}	

		@Override
		public Collection<Object[]> keys(Object... paras) {
			StringBuilder path = new StringBuilder();
			if(base != null) path.append(base);
			if(paras != null && paras.length > 0 && paras[0] != null) path.append(paras[0].toString());
		    if(path.length() > 0 && '/' != path.charAt(path.length() - 1)) path.append('/');
			Enumeration<URL> e = getBundle().findEntries(path.toString(), filePattern, recurse);
			Collection<Object[]> ret = new LinkedList<Object[]>();
			if(e != null) while(e.hasMoreElements()) {
				URL url = e.nextElement();
				if(dir == null || dir.equals(url.getPath().endsWith("/"))) ret.add(new Object[]{url});
			}
			return ret;
		}

		@Override
		public Map<Object[], IExtendable<Object>> all(Object... paras) {
			StringBuilder path = new StringBuilder();
			if(base != null) path.append(base);
			if(paras != null && paras.length > 0 && paras[0] != null) path.append(paras[0].toString());
		    if(path.length() > 0 && '/' != path.charAt(path.length() - 1)) path.append('/');
			Enumeration<URL> e = getBundle().findEntries(path.toString(), filePattern, recurse);
			Map<Object[], IExtendable<Object>> ret = new HashMap<Object[], IExtendable<Object>>();
			if(e != null) while(e.hasMoreElements()) {
				URL url = e.nextElement();
				if(dir == null || dir.equals(url.getPath().endsWith("/"))) ret.put(new Object[]{url}, mockStorage.process(new UrlResource(url)));
			}
			return ret;
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
	}
}
