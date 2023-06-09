package net.yeeyaa.eight.common.resource;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.common.util.CommonUtil;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;
import net.yeeyaa.eight.core.util.PlatformUtil;
import net.yeeyaa.eight.core.util.TypeConvertor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class FileStringLResource<K> implements IListableResource<K, String>, IExtendable<Object> {	
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected final Logger log;
	protected String base;
	protected FilenameFilter filter;
	protected Boolean dir; 
	protected Integer buffer = 8192;
	protected String charset;
	protected Boolean auto = false;
	protected Long max = -1L;
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(K[] paras) {
			StringBuilder path = new StringBuilder();
			if (base != null) path.append(base);
			if (paras != null) path.append(paras.toString()).append(File.separator);
			if (path.length() > 0) try{
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
	
	public FileStringLResource() {
		this.log = LoggerFactory.getLogger(FileStringLResource.class);
	}

	public FileStringLResource(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(FileStringLResource.class) : log;
	}
	
	public void setMax(Long max) {
		if(max != null && max > 0) this.max = max;
	}

	public void setBuffer(Integer buffer) {
		if(buffer != null && buffer > 0) this.buffer = buffer;
	}

	public void setCharset(String charset) {
		this.charset = charset;
	}

	public void setAuto(Boolean auto) {
		if(auto != null) this.auto = auto;
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
	public String find(K ... paras) {
		try{
			StringBuilder p = new StringBuilder();
			if(base != null) p.append(base);
			if(paras != null) for (K para : paras) if (para != null) p.append(para.toString()).append(File.separator);
			if(auto) return CommonUtil.urlToStr(TypeConvertor.strToUrl(p.length() == 0 ? "" : p.substring(0, p.length() - 1).toString()), buffer, max);
			else return TypeConvertor.bytesToStr(TypeConvertor.urlToBytes(TypeConvertor.strToUrl(p.length() == 0 ? "" : p.substring(0, p.length() - 1).toString()), buffer, max), charset);
		} catch(Exception e){
			log.error(paras[0] + ":find resource error", e);
		}
		return null;
	}

	@Override
	public <P> P store(String resource, K ... paras) {
		if(resource != null) try{
			StringBuilder p = new StringBuilder();
			if(base != null) p.append(base);
			if(paras != null) for (K para : paras) if (para != null) p.append(para.toString()).append(File.separator);
			TypeConvertor.bytesToFile(TypeConvertor.strToBytes(resource, charset), p.length() == 0 ? "" : p.substring(0, p.length() - 1).toString());
		} catch(Exception e){
			log.error(paras[0] + ":find resource error", e);
		}
		return null;
	}

	@Override
	public <P> P discard(K ... paras) {
		try{
			StringBuilder p = new StringBuilder();
			if(base != null) p.append(base);
			if(paras != null) for (K para : paras) if (para != null) p.append(para.toString()).append(File.separator);
			String path = p.length() == 0 ? "" : p.substring(0, p.length() - 1).toString();
			PlatformUtil.deleteFiles(new File(path));
		} catch(Exception e){
			log.error(paras[0] + ":find resource error", e);
		}
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		StringBuilder p = new StringBuilder();
		if(base != null) p.append(base);
		if(paras != null) for (K para : paras) if (para != null) p.append(para.toString()).append(File.separator);
		PlatformUtil.deleteFiles(new File(p.length() == 0 ? "" : p.substring(0, p.length() - 1).toString()));
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
	public Map<K[], String> all(K... paras) {
		Collection<K[]> keys = keys(paras);
		Map<K[], String> map = new HashMap<K[], String>(keys.size());
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
