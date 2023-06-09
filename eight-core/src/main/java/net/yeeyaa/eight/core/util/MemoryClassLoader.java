package net.yeeyaa.eight.core.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.zip.ZipEntry;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.storage.Storage.Method;
import net.yeeyaa.eight.core.util.MapperSet;
import net.yeeyaa.eight.core.util.FlaterStreamPool.JarInputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class MemoryClassLoader extends ClassLoader implements IProcessor<Collection<IExtendable<Object>>, Collection<IExtendable<Object>>>, IBiProcessor<Object, Object, Object>, Comparator<Object[]> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected final Logger log;
	protected Map<String, byte[]> classes = new ConcurrentHashMap<String, byte[]>();
    protected Set<String> classnames = Collections.newSetFromMap(new ConcurrentHashMap<String, Boolean>());
    protected Map<String, byte[]> others = new ConcurrentHashMap<String, byte[]>();
    protected URLStreamHandler streamHandler = new ResourceStreamHadnler();
    protected String protocol = "beanprotocol://memoryclassloader/";
    protected IProcessor<String, Void> classCallback;
    protected IProcessor<String, Void> resourceCallback;
    protected IProcessor<Object, String> nameProcessor;
    protected IProcessor<byte[], byte[]> otherProcessor;
    protected IProcessor<byte[], byte[]> classProcessor;
    protected Integer buffer = 8192;
    protected IBiProcessor<Object, Object, Object> beanHolder;
    protected IProcessor<InputStream, Comparator<Object[]>> jarInput;
    
    public void setOtherProcessor(IProcessor<byte[], byte[]> otherProcessor) {
		this.otherProcessor = otherProcessor;
	}

	public void setClassProcessor(IProcessor<byte[], byte[]> classProcessor) {
		this.classProcessor = classProcessor;
	}

	public void setJarInput(IProcessor<InputStream, Comparator<Object[]>> jarInput) {
		this.jarInput = jarInput;
	}

	public void setBeanHolder(IBiProcessor<Object, Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}

	public void setNameProcessor(IProcessor<Object, String> nameProcessor) {
		this.nameProcessor = nameProcessor;
	}

	public void setClassCallback(IProcessor<String, Void> classCallback) {
		this.classCallback = classCallback;
	}

	public void setProtocol(String protocol) {
		if (protocol != null) this.protocol = protocol;
	}

	public void setStreamHandler(URLStreamHandler streamHandler) {
		if (streamHandler != null) this.streamHandler = streamHandler;
	}

	public void setResourceCallback(IProcessor<String, Void> resourceCallback) {
		this.resourceCallback = resourceCallback;
	}

	public void setBuffer(Integer buffer) {
		if(buffer != null && buffer > 0) this.buffer = buffer;
	}
	
	public String[] getClassnames() {
		return classnames.toArray(new String[classnames.size()]);
	}

	public String[] getResourcenames() {
		Set<String> set = others.keySet();
		return set.toArray(new String[set.size()]);
	}

	public MemoryClassLoader() {
		this.log = LoggerFactory.getLogger(MemoryClassLoader.class);
	}

	public MemoryClassLoader(ClassLoader parent, Logger log) {
		super(parent);
		this.log = log == null ? LoggerFactory.getLogger(MemoryClassLoader.class) : log;
	}

	@Override
	protected URL findResource(String name) {
		byte[] bs = others.get(name);
		if(bs != null) try {
			return new URL(null, protocol + name, streamHandler);
		} catch (Exception e) {
        	log.error("MemoryClassLoader : find resource error.", e);
		}
		return null;
	}

	@Override
	protected Enumeration<URL> findResources(String name) throws IOException {
		URL url = findResource(name);
		if(url != null) return Collections.enumeration(Collections.singleton(url));
		else return Collections.enumeration(Collections.<URL>emptySet());
	}

	public InputStream getResourceAsStream(String name) {
        InputStream stream = super.getResourceAsStream(name);
        if (stream == null) {
            byte[] buf = others.get(name);
            if (buf != null) stream = new ByteArrayInputStream(buf);
        }
        return stream;
    }

    public boolean equals(Object o) {
        if (o instanceof MemoryClassLoader) {
            return ((MemoryClassLoader) o).getParent() == getParent();
        }
        return false;
    }

    public int hashCode() {
        return getParent().hashCode();
    }

    protected Class<?> findClass(String name) throws ClassNotFoundException {
        byte[] data = classes.remove(name);
        if (data != null) try{
            Class<?> clz = defineClass(name, data, 0, data.length);
            int i = name.lastIndexOf('.');
            if (i != -1 ) try{
                definePackage(name.substring(0, i), null, null, null, null, null, null, null);
            } catch (IllegalArgumentException iae) {}
            return clz;
        }catch(Throwable e){
        	log.error("MemoryClassLoader : find class error.", e);
        } 
        throw new ClassNotFoundException();
    }

    public byte[] fetchResource(String name){
    	return others.get(name);
    }
    
	protected void addJar(IExtendable<Object> storage) {
        byte[] buf = new byte[buffer];
        int count;
        Comparator<Object[]> stream = null;
        try {
        	stream = jarInput == null ? new JarInputStream(storage.<InputStream>extend(Method.input)) : jarInput.process(storage.<InputStream>extend(Method.input));
        	Object[] ret = new Object[1];
        	ZipEntry entry = null;
        	if (stream.compare(ret, new Object[]{"getNextJarEntry"}) == 0) entry = (ZipEntry)ret[0];
            while (entry != null) {
                String name = entry.getName();
                if (name.endsWith(".class")) {
                	name = getClassName(name);
                	if(!classnames.contains(name)) {
                        int size = (int) entry.getSize();
                        ByteArrayOutputStream out = size >= 0 ? new ByteArrayOutputStream(size) : new ByteArrayOutputStream(buffer);
                        while ((count = stream.compare(ret, new Object[]{"read", buf}) == 0 ? (Integer) ret[0] : -1) > -1) out.write(buf, 0, count);
	                	classes.put(name, classProcessor == null ? out.toByteArray() : classProcessor.process( out.toByteArray()));
	                	classnames.add(name);
	                	if(classCallback != null) classCallback.process(name);
                	}
                }  else {
                    int size = (int) entry.getSize();
                    ByteArrayOutputStream out = size >= 0 ? new ByteArrayOutputStream(size) : new ByteArrayOutputStream(buffer);
                    while ((count = stream.compare(ret, new Object[]{"read", buf}) == 0 ? (Integer) ret[0] : -1) > -1) out.write(buf, 0, count);
                	others.put(name, otherProcessor == null ? out.toByteArray() :otherProcessor.process(out.toByteArray()));
                	if(resourceCallback != null) resourceCallback.process(name);
                }
                entry = stream.compare(ret, new Object[]{"getNextJarEntry"}) == 0 ? (ZipEntry) ret[0] : null;
            }
        } catch (IOException e) {
        	log.error("MemoryClassLoader : load jar error.", e);
        }finally{
    		if(stream!=null) {try{stream.compare(null, new Object[]{"close"});} catch(Exception e){log.error("MemoryClassLoader : load jar error.", e);} }
    	}
    }
    
    protected void addClass(IExtendable<Object> storage, String name){
    	InputStream is = null;
    	if(!classnames.contains(name)) try{
    		is = storage.<InputStream>extend(Method.input);
    		int count;
    		byte[] buf = new byte[buffer];
            ByteArrayOutputStream out = new ByteArrayOutputStream(buffer);
            while ((count = is.read(buf)) > -1) out.write(buf, 0, count);
            classes.put(name, classProcessor == null ? out.toByteArray() : classProcessor.process( out.toByteArray()));
            classnames.add(name);
            if(classCallback != null) classCallback.process(name);
    	}catch(Exception e){
    		log.error("MemoryClassLoader : load class error.", e);
    	}finally{
    		if(is!=null) {try{is.close();} catch(Exception e){log.error("MemoryClassLoader : load class error.", e);} }
    	}
    }

    protected void addResource(IExtendable<Object> storage, String name){
    	InputStream is = null;
    	try{
    		is = storage.<InputStream>extend(Method.input);
    		int count;
    		byte[] buf = new byte[buffer];
            ByteArrayOutputStream out = new ByteArrayOutputStream(buffer);
            while ((count = is.read(buf)) > -1) out.write(buf, 0, count);
            others.put(name, otherProcessor == null ? out.toByteArray() :otherProcessor.process(out.toByteArray()));
            if(resourceCallback != null) resourceCallback.process(name);
    	}catch(Exception e){
    		log.error("MemoryClassLoader : load resource error.", e);
    	}finally{
    		if(is!=null) {try{is.close();} catch(Exception e){log.error("MemoryClassLoader : load resource error.", e);} }
    	}
    }

    protected String getClassName(String fileName) {
        return fileName.substring(0, fileName.length() - 6).replace('/', '.');
    }

    protected String getName(IExtendable<Object> storage){
    	if(storage != null) if(nameProcessor == null) {
    		Object key = storage.extend(Method.key);
    		if(key instanceof String) return (String)key;
    		else if(key instanceof Object[] && ((Object[])key).length > 0 && ((Object[])key)[((Object[])key).length - 1] instanceof String)  
    			return (String)((Object[])key)[((Object[])key).length - 1];
    		else if(key instanceof Collection){
    			Object[] ks = ((Collection<?>) key).toArray();
    			if( ((Object[])ks).length > 0 && ((Object[])ks)[((Object[])ks).length - 1] instanceof String) return (String)((Object[])ks)[((Object[])ks).length - 1];
    		}else if(key != null) return key.toString();
    	}else return nameProcessor.process(storage.extend(Method.key));
    	return null;
    }
    
	@Override
	public Collection<IExtendable<Object>> process(Collection<IExtendable<Object>> storages) {
		if(storages != null && storages.size() > 0) for(IExtendable<Object> storage : storages) if(storage != null && storage.<Boolean>extend(Method.exists)){
			String name = getName(storage);
			if(name != null) if(name.endsWith(".jar")) addJar(storage);
			else if(name.endsWith(".class")) addClass(storage, getClassName(name));
			else addResource(storage, name);
		}
		return storages;
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
	
	public class AddResource implements IProcessor<IExtendable<Object>, IExtendable<Object>>{
		@Override
		public IExtendable<Object> process(IExtendable<Object> storage) {
			if(storage != null && storage.<Boolean>extend(Method.exists)){
				String name = getName(storage);
				if(name != null) if(name.endsWith(".jar")) addJar(storage);
				else if(name.endsWith(".class")) addClass(storage, getClassName(name));
				else addResource(storage, name);
			}
			return storage;
		}	
	}
	
	public class GetResource implements IProcessor<String, byte[]>{
		@Override
		public byte[] process(String name) {
			return fetchResource(name);
		}	
	}

	public class GetClass implements IProcessor<String, Class<?>>{
		@Override
		public Class<?> process(String name) {
			try{
				return loadClass(name);
			}catch(Exception e){
				log.error("MemoryClassLoader : fetch class error.", e);
			}
			return null;
		}	
	}
	
	public class ListClass implements IReadonlyListable<String, Class<?>>, IExtendable<Object> {
		protected final IProcessor<String[], Object> count = new IProcessor<String[], Object>(){
			@Override
			public Object process(String[] paras) {
				return new Long(getClassnames().length);
			}
		};
		
		@Override
		public Class<?> find(String... paras) {
			try{
				return loadClass(paras[0]);
			}catch(Exception e){
				log.error("MemoryClassLoader : fetch class error.", e);
			}
			return null;
		}

		@Override
		public Collection<String[]> keys(String... paras) {
			List<String[]> ret = new ArrayList<String[]>();
			for(String name : getClassnames())ret.add(new String[]{name});
			return ret;
		}

		@Override
		public Map<String[], Class<?>> all(String... paras) {
			Map<String[], Class<?>> ret = new HashMap<String[], Class<?>>();
			for(String name : getClassnames()) try{
				ret.put(new String[]{name}, loadClass(name));
			}catch(Exception e){
				log.error("MemoryClassLoader : fetch class error.", e);
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
	
	public class ListResource implements IReadonlyListable<String, byte[]>, IExtendable<Object> {
		protected final IProcessor<String[], Object> count = new IProcessor<String[], Object>(){
			@Override
			public Object process(String[] paras) {
				return new Long(getResourcenames().length);
			}
		};
		
		@Override
		public byte[] find(String... paras) {
			return fetchResource(paras[0]);
		}

		@Override
		public Collection<String[]> keys(String... paras) {
			List<String[]> ret = new ArrayList<String[]>();
			for(String name : getResourcenames())ret.add(new String[]{name});
			return ret;
		}

		@Override
		public Map<String[], byte[]> all(String... paras) {
			Map<String[], byte[]> ret = new HashMap<String[], byte[]>();
			for(String name : getResourcenames()) ret.put(new String[]{name}, fetchResource(name));
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
	
	public class ResourceStreamHadnler extends URLStreamHandler implements IProcessor<URL, URLConnection>{
		@Override
		public URLConnection openConnection(URL url) throws IOException {
			URLConnection conn = process(url);
			if (conn == null) throw new IOException("MemoryResourceStreamHadnler: no such resource." + url);
			else return conn;
		}

		@Override
		public URLConnection process(URL url) {
			if (url != null) {
				String name = url.toExternalForm();
				if (name != null) {
					name = name.substring(protocol.length());
					final byte[] bytes = fetchResource(name);
					if (bytes != null) return new URLConnection(url){      	 
			    	    public void connect() {} 
			       	 
			    	    public InputStream getInputStream() { 
			    	        return new ByteArrayInputStream(bytes); 
			    	    }
			        };
				}
			}
			return null;
		}
	}
}
