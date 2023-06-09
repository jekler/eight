package net.yeeyaa.eight.core.processor;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.zip.ZipEntry;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.core.storage.Storage;
import net.yeeyaa.eight.core.storage.Storage.Method;
import net.yeeyaa.eight.core.util.FlaterStreamPool.ZipInputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ZipStorageProcessor implements IProcessor<Object, Collection<IExtendable<Object>>>{
	protected final Logger log;
    protected Map<String, Map<String, IExtendable<Object>[]>> files = new ConcurrentHashMap<String, Map<String, IExtendable<Object>[]>>();
    protected Map<IExtendable<Object>, Map<String, byte[]>> cache = new ConcurrentHashMap<IExtendable<Object>, Map<String, byte[]>>();
    protected Map<IExtendable<Object>, Long> order = new LinkedHashMap<IExtendable<Object>, Long>();
    protected volatile long total = 0L;
    protected Long size = 10485760L;
    protected Integer buffer = 8192;
    protected Boolean compatible; 
    protected IProcessor<InputStream, ZipInputStream> zipInput;

	public ZipStorageProcessor() {
		this.log = LoggerFactory.getLogger(ZipStorageProcessor.class);
	}
	
	public ZipStorageProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ZipStorageProcessor.class) : log;
	}
    
	public void setZipInput(IProcessor<InputStream, ZipInputStream> zipInput) {
		this.zipInput = zipInput;
	}
	
	public void setCompatible(Boolean compatible) {
		this.compatible = compatible;
	}

	public void setSize(Long size) {
		if (size != null && size > 0) this.size = size;
	}
	
	public void setBuffer(Integer buffer) {
		if(buffer != null && buffer > 0) this.buffer = buffer;
	}
	
	@Override
	public Collection<IExtendable<Object>> process(Object instance) {
		if (instance instanceof IExtendable || instance instanceof Collection) {
			Collection<IExtendable<Object>> storages;
			if (instance instanceof Collection) storages = (Collection<IExtendable<Object>>) instance;
			else {
				storages = new HashSet<IExtendable<Object>>();
				storages.add((IExtendable<Object>) instance);
			}
			HashSet<IExtendable<Object>> ret = new HashSet<IExtendable<Object>>();
			for (IExtendable<Object> storage : storages) if (storage != null) {
		        ZipInputStream stream = null;
		        boolean flag = true;
		        try {
		        	stream = zipInput == null ? new ZipInputStream(storage.<InputStream>extend(Method.input)) : zipInput.process(storage.<InputStream>extend(Method.input));
		        	ZipEntry entry = stream.getNextEntry();
			        HashSet<IExtendable<Object>> tmp = new HashSet<IExtendable<Object>>();
			        flag = false;
		            while (entry != null) {
		                String name = entry.getName();
		            	String[] path = splitName(name);
		            	if (path != null) synchronized (files) {
		            		Map<String, IExtendable<Object>[]> map = files.get(path[0]);
		            		if (map == null) {
		            			map = new ConcurrentHashMap<String, IExtendable<Object>[]>();
		            			files.put(path[0], map);
		            		}
		            		IExtendable<Object>[] ss = map.get(path[1]);
		            		if (ss == null) ss = new IExtendable[1];
		            		else ss = Arrays.copyOf(ss, ss.length + 1);
		            		ss[ss.length - 1] = storage;
		            		map.put(path[1], ss);
		            	}
                        if (Boolean.TRUE.equals(compatible)) ret.add(new ZipStorage(storage, name));
                        else tmp.add(new ZipStorage(storage, name));
		                entry = stream.getNextEntry();
		            }
		            ret.addAll(tmp);
		        } catch (IOException e) {
		        	if (compatible == null || (compatible && flag)) ret.add(storage);
		        	log.error("ZipStorageProcessor : load zip error.", e);
		        }finally{
		    		if(stream!=null) {try{stream.close();} catch(Exception e){log.error("ZipStorageProcessor : load zip error.", e);} }
		    	}
			}
			return ret;
		}
		return null;
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
	
	protected byte[] getContent(String key, IExtendable<Object> storage) {
    	String[] path = splitName(key);
    	if (path != null) {
    		Map<String, IExtendable<Object>[]> map = files.get(path[0]);
    		if (map != null) {
    			IExtendable<Object>[] storages = map.get(path[1]);
    			IExtendable<Object> s = null;
    			if (storages != null && storages.length > 0) if (storage == null) s = storages[0];
    			else for (IExtendable<Object> ss : storages) if (storage.equals(ss)) {
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
    				        byte[] buf = new byte[buffer];
    				        int count;
    				        ZipInputStream stream = null;
    				        try {
    				        	stream = zipInput == null ? new ZipInputStream(s.<InputStream>extend(Method.input)) : zipInput.process(s.<InputStream>extend(Method.input));
    				        	ZipEntry entry = stream.getNextEntry();
    				            while (entry != null) {
    				                String entryname = entry.getName();
				                    int size = (int) entry.getSize();
				                    ByteArrayOutputStream out = size >= 0 ? new ByteArrayOutputStream(size) : new ByteArrayOutputStream(buffer);
				                    while ((count = stream.read(buf)) > -1) out.write(buf, 0, count);
				                    byte[] value = out.toByteArray();
				                    length += value.length;
				                    resources.put(entryname, value);
    				                entry = stream.getNextEntry();
    				            }
    				        } catch (IOException e) {
    				        	log.error("ZipStorageProcessor : load jar error.", e);
    				        }finally{
    				    		if(stream!=null) {try{stream.close();} catch(Exception e){log.error("ZipStorageProcessor : load jar error.", e);} }
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
    				if (resources != null) return resources.get(key);
    			}
    		}
    	}
    	return null;
	}
	
	protected Boolean isExist(String key, IExtendable<Object> storage) {
    	String[] path = splitName(key);
    	if (path != null) {
    		Map<String, IExtendable<Object>[]> map = files.get(path[0]);
    		if (map != null) {
    			IExtendable<Object>[] storages = map.get(path[1]);
    			if (storages != null && storages.length > 0) if (storage == null) return true;
    			else for (IExtendable<Object> ss : storages) if (storage.equals(ss)) return true;
    		}
    	}
		return false;
	}
	
	public class ZipStorage extends Storage<Object> {
		protected Long lastModified = Calendar.getInstance().getTimeInMillis();
		protected IExtendable<Object> storage;
		protected String key;
		
		public ZipStorage(String key) {
			this.key = key;
		}

		public ZipStorage(IExtendable<Object> storage, String key) {
			this.storage = storage;
			this.key = key;
		}

		public InputStream input() {
			byte[] content = getContent(key, storage);
			if (content == null) return null;
			else return new ByteArrayInputStream(content);
		}

		public Boolean exists() {
			return isExist(key, storage);
		}

		public Object key() {
			return key;
		}

		public Long modified() {
			return lastModified;
		}
	}
}
