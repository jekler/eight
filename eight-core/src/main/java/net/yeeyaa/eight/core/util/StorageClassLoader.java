package net.yeeyaa.eight.core.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
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


public class StorageClassLoader extends ClassLoader implements IProcessor<Collection<IExtendable<Object>>, Collection<IExtendable<Object>>>, IBiProcessor<Object, Object, Object>, Comparator<Object[]> {
	protected final Logger log;
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected URLStreamHandler streamHandler = new StorageStreamHadnler();
    protected String protocol = "beanprotocol://storageclassloader/";
    protected Map<String, Map<String, IExtendable<Object>[]>> files = new ConcurrentHashMap<String, Map<String, IExtendable<Object>[]>>();
    protected Map<IExtendable<Object>, Map<String, byte[]>> cache = new ConcurrentHashMap<IExtendable<Object>, Map<String, byte[]>>();
    protected Map<IExtendable<Object>, Long> order = new LinkedHashMap<IExtendable<Object>, Long>();
    protected volatile long total = 0L;
    protected Long size = 10485760L;
    protected Integer buffer = 8192;
    protected IProcessor<String, Void> classCallback;
    protected IProcessor<String, Void> resourceCallback;
    protected IProcessor<Object, String> nameProcessor;
    protected IProcessor<byte[], byte[]> resourceProcessor;
    protected IBiProcessor<Object, Object, Object> beanHolder;
    protected IProcessor<InputStream, Comparator<Object[]>> jarInput;

	public StorageClassLoader() {
		this.log = LoggerFactory.getLogger(StorageClassLoader.class);
	}
	
	public StorageClassLoader(ClassLoader parent, Logger log) {
		super(parent);
		this.log = log == null ? LoggerFactory.getLogger(StorageClassLoader.class) : log;
	}
    
	public void setResourceProcessor(IProcessor<byte[], byte[]> resourceProcessor) {
		this.resourceProcessor = resourceProcessor;
	}

	public void setJarInput(IProcessor<InputStream, Comparator<Object[]>> jarInput) {
		this.jarInput = jarInput;
	}
    
	public void setBeanHolder(IBiProcessor<Object, Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}

	public void setSize(Long size) {
		if (size != null && size > 0) this.size = size;
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
	
	@Override
	protected URL findResource(String name) {
    	String[] path = splitName(name);
    	if (path != null) {
    		Map<String, IExtendable<Object>[]> map = files.get(path[0]);
    		if (map != null) {
    			IExtendable<Object>[] storages = map.get(path[1]);
    			if (storages != null && storages.length > 0) try {
    				return new URL(null, protocol + name + '|' + getName(storages[0]), streamHandler);
    			} catch (Exception e) {
    	        	log.error("StorageClassLoader : find resource error.", e);
    			}
    		}
    	}
    	return null;
	}

	@Override
	protected Enumeration<URL> findResources(String name) throws IOException {
    	String[] path = splitName(name);
    	Collection<URL> urls = new HashSet<URL>();
    	if (path != null) {
    		Map<String, IExtendable<Object>[]> map = files.get(path[0]);
    		if (map != null) {
    			IExtendable<Object>[] storages = map.get(path[1]);
    			if (storages != null && storages.length > 0) for (IExtendable<Object> storage : storages) try {
    				urls.add(new URL(null, protocol + name + '|' + getName(storage), streamHandler));
    			} catch (Exception e) {
    	        	log.error("StorageClassLoader : find resource error.", e);
    			}
    		}
    	}
		return Collections.enumeration(urls);
	}

	public InputStream getResourceAsStream(String name) {
        InputStream stream = super.getResourceAsStream(name);
        if (stream == null) {
            byte[] buf = fetchResource(name);
            if (buf != null) stream = new ByteArrayInputStream(buf);
        }
        return stream;
    }

    public boolean equals(Object o) {
        if (o instanceof StorageClassLoader) {
            return ((StorageClassLoader) o).getParent() == getParent();
        }
        return false;
    }

    public int hashCode() {
        return getParent().hashCode();
    }
    
    protected Class<?> findClass(String name) throws ClassNotFoundException {
        byte[] data = fetchResource(name.replace('.', '/').concat(".class"));
        if (data != null) try{
            Class<?> clz = defineClass(name, data, 0, data.length);
            int i = name.lastIndexOf('.');
            if (i != -1 ) try{
                definePackage(name.substring(0, i), null, null, null, null, null, null, null);
            } catch (IllegalArgumentException iae) {}
            return clz;
        }catch(Throwable e){
        	log.error("StorageClassLoader : find class error.", e);
        } 
        throw new ClassNotFoundException();
    }
    
    public byte[] fetchResource(String name){
    	return fetchResource(name, null);
    }

    public byte[] fetchResource(String name, String storage){
    	String[] path = splitName(name);
    	if (path != null) {
    		Map<String, IExtendable<Object>[]> map = files.get(path[0]);
    		if (map != null) {
    			IExtendable<Object>[] storages = map.get(path[1]);
    			IExtendable<Object> s = null;
    			if (storages != null && storages.length > 0) if (storage == null) s = storages[0];
    			else for (IExtendable<Object> ss : storages) if (storage.equals(getName(ss))) {
    				s = ss;
    				break;
    			}
    			if (s != null) {
    				Map<String, byte[]> resources = cache.get(s);
    				if (resources == null) synchronized(this) {
    					resources = cache.get(s);
    					if (resources == null) {
    						resources = new HashMap<String, byte[]>();
    				        long length = 0;
    						String storagename = getName(s);
    						if(storagename != null) {
	    				        byte[] buf = new byte[buffer];
	    				        int count;
    							if(storagename.endsWith(".jar")){
    								Comparator<Object[]> stream = null;
		    				        try {
		    				        	stream = jarInput == null ? new JarInputStream(s.<InputStream>extend(Method.input)) : jarInput.process(s.<InputStream>extend(Method.input));
		    				        	Object[] ret = new Object[1];
		    				        	ZipEntry entry = null;
		    				        	if (stream.compare(ret, new Object[]{"getNextJarEntry"}) == 0) entry = (ZipEntry)ret[0];
		    				            while (entry != null) {
		    				                String entryname = entry.getName();
						                    int size = (int) entry.getSize();
						                    ByteArrayOutputStream out = size >= 0 ? new ByteArrayOutputStream(size) : new ByteArrayOutputStream(buffer);
						                    while ((count = stream.compare(ret, new Object[]{"read", buf}) == 0 ? (Integer) ret[0] : -1) > -1) out.write(buf, 0, count);
						                    byte[] value = resourceProcessor == null ? out.toByteArray() : resourceProcessor.process(out.toByteArray());
						                    length += value.length;
						                    resources.put(entryname, value);
		    				                entry = stream.compare(ret, new Object[]{"getNextJarEntry"}) == 0 ? (ZipEntry) ret[0] : null;
		    				            }
		    				        } catch (IOException e) {
		    				        	log.error("StorageClassLoader : load jar error.", e);
		    				        }finally{
		    				    		if(stream!=null) {try{stream.compare(null, new Object[]{"close"});} catch(Exception e){log.error("StorageClassLoader : load jar error.", e);} }
		    				    	}
	    						} else {
	    							InputStream is = null;
	    							try{
	    					    		is = s.<InputStream>extend(Method.input);
	    					            ByteArrayOutputStream out = new ByteArrayOutputStream(buffer);
	    					            while ((count = is.read(buf)) > -1) out.write(buf, 0, count);
	    					            byte[] value = resourceProcessor == null ? out.toByteArray() : resourceProcessor.process(out.toByteArray());
	    					            resources.put(storagename, value);
	    					            length += value.length;
	    					    	}catch(Exception e){
	    					    		log.error("StorageClassLoader : load class error.", e);
	    					    	}finally{
	    					    		if(is!=null) {try{is.close();} catch(Exception e){log.error("StorageClassLoader : load class error.", e);} }
	    					    	}
	    						}
    						}
    				        cache.put(s, resources);
    				        long overflow = length + total - size;
    				        if (overflow > 0) {
    				        	Iterator<Entry<IExtendable<Object>, Long>> itr = order.entrySet().iterator();
    				        	while (itr.hasNext()) {
    				        		Entry<IExtendable<Object>, Long> entry = itr.next();
    				        		cache.remove(entry.getKey());
    				        		overflow -= entry.getValue();
    				        		itr.remove();
    				        		if (overflow < 1) break;
    				        	}
    				        	total = size + overflow;
    				        } else total += length;
							order.put(s, length);
    					} else {
        					Long value = order.remove(s);
        					if (value != null) order.put(s, value);
    					}
    				} else synchronized(this) {
    					Long value = order.remove(s);
    					if (value != null) order.put(s, value);
    				}
    				if (resources != null) return resources.get(name);
    			}
    		}
    	}
    	return null;
    }
   
	protected void addJar(IExtendable<Object> storage) {
		Comparator<Object[]> stream = null;
        try {
        	stream = jarInput == null ? new JarInputStream(storage.<InputStream>extend(Method.input)) : jarInput.process(storage.<InputStream>extend(Method.input));
        	Object[] ret = new Object[1];
        	ZipEntry entry = null;
        	if (stream.compare(ret, new Object[]{"getNextJarEntry"}) == 0) entry = (ZipEntry)ret[0];
            while (entry != null) {
                String name = entry.getName();
                addResource(storage, name);
                try{
			        if (name.endsWith(".class")) {
			        	if(classCallback != null) classCallback.process(getClassName(name));
			        } else if(resourceCallback != null) resourceCallback.process(name);
                } catch (Exception e) {
                	log.error("StorageClassLoader : load jar error.", e);
                }
                entry = stream.compare(ret, new Object[]{"getNextJarEntry"}) == 0 ? (ZipEntry) ret[0] : null;
            }
        } catch (IOException e) {
        	log.error("StorageClassLoader : load jar error.", e);
        }finally{
    		if(stream!=null) {try{stream.compare(null, new Object[]{"close"});} catch(Exception e){log.error("StorageClassLoader : load jar error.", e);} }
    	}
    }

    protected void addResource(IExtendable<Object> storage, String name){
    	String[] path = splitName(name);
    	if (path != null) synchronized (files) {
    		Map<String, IExtendable<Object>[]> map = files.get(path[0]);
    		if (map == null) {
    			map = new ConcurrentHashMap<String, IExtendable<Object>[]>();
    			files.put(path[0], map);
    		}
    		IExtendable<Object>[] storages = map.get(path[1]);
    		if (storages == null) storages = new IExtendable[1];
    		else storages = Arrays.copyOf(storages, storages.length + 1);
    		storages[storages.length - 1] = storage;
    		map.put(path[1], storages);
    	}
    }

    protected String[] splitName(String fileName) {
    	if (fileName != null && fileName.trim().length() > 0) {
	    	String[] ret = new String[2];
	    	fileName = fileName.trim();
	    	int index = fileName.lastIndexOf('/');
	    	if (index == -1) {
	    		ret[0] = "";
	    		ret[1] = fileName;
	    	} else {
	    		ret[0] = fileName.substring(0, index);
	    		ret[1] = fileName.substring(index + 1);
	    	}
	    	return ret;
    	}
    	return null;
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
		if(storages != null && storages.size() > 0) for(IExtendable<Object> storage : storages) try {
			if(storage != null && storage.<Boolean>extend(Method.exists)) {
				String name = getName(storage);
				if(name != null) if(name.endsWith(".jar")) addJar(storage);
				else {
					addResource(storage, name);
			        if (name.endsWith(".class")) {
			        	if(classCallback != null) classCallback.process(getClassName(name));
			        } else if(resourceCallback != null) resourceCallback.process(name);
				} 
			}
		}catch (Exception e) {
        	log.error("StorageClassLoader : load jar error.", e);
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
	
	public class AddResource implements IProcessor<IExtendable<Object>, Object>{
		@Override
		public Object process(IExtendable<Object> storage) {
			if(storage != null && storage.<Boolean>extend(Method.exists)) {
				String name = getName(storage);
				if(name != null) if(name.endsWith(".jar")) addJar(storage);
				else {
					addResource(storage, name);
			        if (name.endsWith(".class")) {
			        	if(classCallback != null) classCallback.process(getClassName(name));
			        } else if(resourceCallback != null) resourceCallback.process(name);
				} 
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
				log.error("StorageClassLoader : fetch class error.", e);
			}
			return null;
		}	
	}
	
	public class ListResource implements IReadonlyListable<String, byte[]>, IExtendable<Object> {
		protected final IProcessor<String[], Object> count = new IProcessor<String[], Object>(){
			@Override
			public Object process(String[] paras) {
				Long size = 0L;
				for(Entry<String, Map<String, IExtendable<Object>[]>> entry : files.entrySet()) size += entry.getValue().size();
				return size;
			}
		};
		
		@Override
		public byte[] find(String... paras) {
			return fetchResource(paras[0]);
		}

		@Override
		public Collection<String[]> keys(String... paras) {
			List<String[]> ret = new ArrayList<String[]>();
			for(Entry<String, Map<String, IExtendable<Object>[]>> entry : files.entrySet()) for (String name : entry.getValue().keySet()) ret.add(new String[]{entry.getKey() + '/' + name});
			return ret;
		}

		@Override
		public Map<String[], byte[]> all(String... paras) {
			Map<String[], byte[]> ret = new HashMap<String[], byte[]>();
			for(String[] name : keys()) ret.put(name, fetchResource(name[0]));
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
	
	public class StorageStreamHadnler extends URLStreamHandler implements IProcessor<URL, URLConnection>{
		@Override
		public URLConnection openConnection(URL url) throws IOException {
			URLConnection conn = process(url);
			if (conn == null) throw new IOException("StorageResourceStreamHadnler: no such resource." + url);
			else return conn;
		}

		@Override
		public URLConnection process(URL url) {
			if (url != null) {
				String name = url.toExternalForm();
				if (name != null) {
					String[] keys = name.substring(protocol.length()).split("\\|");
					final byte[] bytes = keys.length > 1 ? fetchResource(keys[0], keys[1]) : fetchResource(name);
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
