package net.yeeyaa.eight.core.processor;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.HashSet;
import java.util.zip.ZipEntry;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.core.storage.ByteWStorage;
import net.yeeyaa.eight.core.storage.Storage.Method;
import net.yeeyaa.eight.core.util.FlaterStreamPool.ZipInputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ZipMemoryProcessor implements IProcessor<Object, Collection<IExtendable<Object>>>{
	protected final Logger log;
    protected Integer buffer = 8192;
    protected Boolean compatible; 
    protected IProcessor<InputStream, ZipInputStream> zipInput;

	public ZipMemoryProcessor() {
		this.log = LoggerFactory.getLogger(ZipMemoryProcessor.class);
	}
	
	public ZipMemoryProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ZipMemoryProcessor.class) : log;
	}
    
	public void setZipInput(IProcessor<InputStream, ZipInputStream> zipInput) {
		this.zipInput = zipInput;
	}

	public void setCompatible(Boolean compatible) {
		this.compatible = compatible;
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
	        byte[] buf = new byte[buffer];
	        int count;
			for (IExtendable<Object> storage : storages) if (storage != null) {
		        ZipInputStream stream = null;
		        boolean flag = true;
		        try {
		        	stream = zipInput == null ? new ZipInputStream(storage.<InputStream>extend(Method.input)) : zipInput.process(storage.<InputStream>extend(Method.input));
		        	ZipEntry entry = stream.getNextEntry();
			        HashSet<IExtendable<Object>> tmp = new HashSet<IExtendable<Object>>();
			        flag = false;
		            while (entry != null) {
		            	int size = (int) entry.getSize();
                        ByteArrayOutputStream out = size >= 0 ? new ByteArrayOutputStream(size) : new ByteArrayOutputStream(buffer);
                        while ((count = stream.read(buf)) > -1) out.write(buf, 0, count);
                        if (Boolean.TRUE.equals(compatible)) ret.add(new ByteWStorage(out.toByteArray(), entry.getName()));
                        else tmp.add(new ByteWStorage(out.toByteArray(), entry.getName()));
		                entry = stream.getNextEntry();
		            }
		            ret.addAll(tmp);
		        } catch (IOException e) {
		        	if (compatible == null || (compatible && flag)) ret.add(storage);
		        	log.error("ZipMemoryProcessor : load zip error.", e);
		        }finally{
		    		if(stream!=null) {try{stream.close();} catch(Exception e){log.error("ZipMemoryProcessor : load zip error.", e);} }
		    	}
			}
			return ret;
		}
		return null;
	}
}
