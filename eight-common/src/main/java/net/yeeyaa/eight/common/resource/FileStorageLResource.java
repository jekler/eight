package net.yeeyaa.eight.common.resource;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.storage.Storage.Method;
import net.yeeyaa.eight.core.util.MapperSet;
import net.yeeyaa.eight.core.util.PlatformUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;

public class FileStorageLResource<K, V extends IExtendable<Object>> implements IListableResource<K, V>, IExtendable<Object> {	
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected final Logger log;
	protected IProcessor<Resource, V> mockStorage;
	protected String base;
	protected FilenameFilter filter;
	protected Boolean dir; 
	protected Integer buffer = 8192;
	protected Long max = 0L;
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(K[] paras) {
			StringBuilder path = new StringBuilder();
			if(base != null) path.append(base);
			if (paras != null) path.append(paras.toString()).append(File.separator);
			if(path.length() > 0) try{
				File f = new File(path.length() == 0 ? "" : path.substring(0, path.length() - 1).toString());
				File[] files;
				if(filter == null) files = f.listFiles();
				else files = f.listFiles(filter);
				Long ret = new Long(files == null ? 0 : files.length);
				if(dir != null && files != null && files.length > 0) for(File file : files) try{
					if(!dir.equals(file.isDirectory())) ret--;
				}catch(Exception e){
					log.error(paras[0] + ":list resource error", e);
				}
				return ret;
			}catch(Exception e){
				log.error(paras[0] + ":list resource error", e);
			}
			return null;
		}
	};
	
	public FileStorageLResource() {
		this.log = LoggerFactory.getLogger(FileStorageLResource.class);
	}

	public FileStorageLResource(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(FileStorageLResource.class) : log;
	}
	
	public void setMax(Long max) {
		if(max != null && max > 0) this.max = max;
	}

	public void setMockStorage(IProcessor<Resource, V> mockStorage) {
		this.mockStorage = mockStorage;
	}
	
	public void setBuffer(Integer buffer) {
		if(buffer != null && buffer > 0) this.buffer = buffer;
	}
	
	public void setDir(Boolean dir) {
		this.dir = dir;
	}
	
	public void setFilter(FilenameFilter filter) {
		this.filter = filter;
	}

	public void setBase(String base) {
		if (base != null) this.base = base + File.separator;
	}

	@Override
	public V find(K ... paras) {
		StringBuilder p = new StringBuilder();
		if(base != null) p.append(base);
		if(paras != null) for (K para : paras) if (para != null) p.append(para.toString()).append(File.separator);
		return mockStorage.process(new FileSystemResource(p.length() == 0 ? "" : p.substring(0, p.length() - 1).toString()));
	}

	@Override
	public <P> P store(V resource, K ... paras) {
		if(resource != null) {
			StringBuilder p = new StringBuilder();
			if(base != null) p.append(base);
			if(paras != null) for (K para : paras) if (para != null) p.append(para.toString()).append(File.separator);
			FileOutputStream fos = null;
			InputStream in = null;
			try {
				in = resource.extend(Method.input);
				fos = new FileOutputStream(p.length() == 0 ? "" : p.substring(0, p.length() - 1).toString());
				byte[] b = new byte[buffer];
				int bytesRead;
				Long count = 0L;
				while ((bytesRead = in.read(b)) != -1) {
					if (max > 0) {
						count += bytesRead;
						if (count > max) throw new IOException("Data overflow. The maxLength is " + max);
					}
					fos.write(b, 0, bytesRead);
				}
			} catch (Exception e) {
				log.error("FileStorageLResource : transaction perform error.", e);
			} finally {
	            if(fos!=null) {try{fos.close();} catch(Exception e){log.error("FileStorageLResource : transaction perform error.", e);} }
	            if(in!=null) {try{in.close();} catch(Exception e){log.error("FileStorageLResource : transaction perform error.", e);} }
	        }
		}
		return null;
	}

	@Override
	public <P> P discard(K ... paras) {
		StringBuilder p = new StringBuilder();
		if(base != null) p.append(base);
		if(paras != null) for (K para : paras) if (para != null) p.append(para.toString()).append(File.separator);
		String path = p.length() == 0 ? "" : p.substring(0, p.length() - 1).toString();
		mockStorage.process(new FileSystemResource(path.toString()));
		PlatformUtil.deleteFiles(new File(path.toString()));
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		StringBuilder path = new StringBuilder();
		if(base != null) path.append(base);
		if(paras != null) for (K para : paras) if (para != null) path.append(para.toString()).append(File.separator);
		PlatformUtil.deleteFiles(new File(path.length() == 0 ? "" : path.substring(0, path.length() - 1).toString()));
		return null;
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		StringBuilder path = new StringBuilder();
		ArrayList ret = new ArrayList<K[]>(0);
		if(base != null) path.append(base);
		if(paras != null) for (K para : paras) if (para != null) path.append(para.toString()).append(File.separator);
		if(path.length() > 0) try{
			File f = new File(path.length() == 0 ? "" : path.substring(0, path.length() - 1).toString());
			File[] files;
			if(filter == null) files = f.listFiles();
			else files = f.listFiles(filter);
			if (files != null && files.length > 0) {
				ret = new ArrayList<K[]>(files.length);
				for(File file : files) try{
					if(dir == null || dir.equals(file.isDirectory())){
						LinkedList<K> fs = new LinkedList<K>(Arrays.asList(paras));
						fs.add((K)file.getName());
						ret.add(fs.toArray());
					}
				}catch(Exception e){
					log.error(paras[0] + ":list resource error", e);
				}
			}
		}catch(Exception e){
			log.error(paras[0] + ":list resource error", e);
		}
		return ret;
	}

	@Override
	public Map<K[], V> all(K... paras) {
		Collection<K[]> keys = keys(paras);
		Map<K[], V> map = new HashMap<K[], V>(keys.size());
		for(K[] key : keys) map.put(key, find(key));
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
}
