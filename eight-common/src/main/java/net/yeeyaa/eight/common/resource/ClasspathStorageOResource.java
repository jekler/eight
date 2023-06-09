package net.yeeyaa.eight.common.resource;

import java.io.File;
import java.io.FilenameFilter;
import java.net.JarURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;


public class ClasspathStorageOResource<K> implements IReadonlyListable<K, IExtendable<Object>>, IExtendable<Object> {	
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected final Logger log;
	protected String base;
	protected ClassLoader classLoader = getClass().getClassLoader();
	protected Boolean overlap = false;
	protected FilenameFilter filter;
	protected Boolean dir;  
	protected IProcessor<Resource, IExtendable<Object>> mockStorage;
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(K[] paras) {
			StringBuilder path = new StringBuilder();
			HashSet<String> store = new HashSet<String>();
			if(base != null) path.append(base);
			if(paras != null && paras.length > 0 && paras[0] != null) path.append(paras[0].toString());
		    if(path.length() > 0 && '/' != path.charAt(path.length() - 1)) path.append('/');
			try{
				List<URL> urls = new LinkedList<URL>();
				if(overlap) {
					Enumeration<URL> rs = classLoader.getResources(path.toString());
					if(rs != null) while(rs.hasMoreElements()) urls.add(rs.nextElement());
				}else {
					URL r = classLoader.getResource(path.toString());
					if(r != null) urls.add(r);
				}
				for(URL url : urls) try{
				    if(url.getProtocol().equals("file")){
					    File file = new File(url.toURI());
					    File[] filenames; 
					    if(filter == null) filenames = file.listFiles();
					    else filenames = file.listFiles(filter);
					    if (filenames != null && filenames.length > 0) for(File f : filenames)  try{
					    	if(dir == null || dir.equals(f.isDirectory())) store.add(path.toString() + f.getName());
					    }catch(Exception e){
				        	 log.error(url + ":list resource error", e);
				        }
				    }else if(url.getProtocol().equals("jar")){
				         JarURLConnection urlcon = (JarURLConnection) (url.openConnection());
			        	 JarFile jar = urlcon.getJarFile();
			             Enumeration<JarEntry> entries = jar.entries();
			             while (entries.hasMoreElements()) {
			                 String entry = entries.nextElement().getName();
			                 if (path.length() == 0 || entry.startsWith(path.toString())) {
			                	 String filename = entry.substring(path.length());
			                	 String[] fs = filename.split("/");
			                	 if(fs.length > 1 && fs[0].length() > 0) filename = fs[0] + '/';
			                	 if(fs[0].length() > 0 && (dir == null || dir.equals(fs.length > 1 || filename.endsWith("/"))) && (filter == null || filter.accept(new File(path.toString()), filename))) 
			                		 store.add(path + filename);
			                 }
			             }
				    }
				}catch(Exception e){
		        	 log.error(url + ":list resource error", e);
		        }
			}catch(Exception e){
				log.error(paras[0] + ":list resource error", e);
			}
			return new Long(store.size());
		}
	};
	
	public ClasspathStorageOResource() {
		this.log = LoggerFactory.getLogger(ClasspathStorageOResource.class);
	}

	public ClasspathStorageOResource(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ClasspathStorageOResource.class) : log;
	}
	
	public void setMockStorage(IProcessor<Resource, IExtendable<Object>> mockStorage) {
		this.mockStorage = mockStorage;
	}
	
	public void setDir(Boolean dir) {
		this.dir = dir;
	}
	
	public void setFilter(FilenameFilter filter) {
		this.filter = filter;
	}
	
	public void setOverlap(Boolean overlap) {
		if(overlap != null) this.overlap = overlap;
	}
	
	public void setClassLoader(ClassLoader classLoader) {
		if(classLoader != null) this.classLoader = classLoader;
	}

	public void setBase(String base) {
		this.base = base;
	}
	
	@Override
	public IExtendable<Object> find(K ... paras) {
		StringBuilder p = new StringBuilder();
		if(base != null) p.append(base);
		if(paras != null) for (K para : paras) if (para != null) p.append(para.toString());
		return mockStorage.process(new ClassPathResource(p.toString(), classLoader));
	}
	
	@Override
	public Collection<K[]> keys(K... paras) {
		StringBuilder path = new StringBuilder();
		ArrayList ret = new ArrayList<K[]>();
		HashSet<String> store = new HashSet<String>();
		if(base != null) path.append(base);
		if(paras != null && paras.length > 0 && paras[0] != null) path.append(paras[0].toString());
	    if(path.length() > 0 && '/' != path.charAt(path.length() - 1)) path.append('/');
		try{
			List<URL> urls = new LinkedList<URL>();
			if(overlap) {
				Enumeration<URL> rs = classLoader.getResources(path.toString());
				if(rs != null) while(rs.hasMoreElements()) urls.add(rs.nextElement());
			}else {
				URL r = classLoader.getResource(path.toString());
				if(r != null) urls.add(r);
			}
			for(URL url : urls) try{
			    if(url.getProtocol().equals("file")){
				    File file = new File(url.toURI());
				    File[] filenames; 
				    if(filter == null) filenames = file.listFiles();
				    else filenames = file.listFiles(filter);
				    if (filenames != null && filenames.length > 0) for(File f : filenames) try{
				    	if(dir == null || dir.equals(f.isDirectory())) store.add(path.toString() + f.getName());
				    }catch(Exception e){
			        	 log.error(url + ":list resource error", e);
			        }
			    }else if(url.getProtocol().equals("jar")){
			         JarURLConnection urlcon = (JarURLConnection) (url.openConnection());
		        	 JarFile jar = urlcon.getJarFile();
		             Enumeration<JarEntry> entries = jar.entries();
		             while (entries.hasMoreElements()) {
		                 String entry = entries.nextElement().getName();
		                 if (path.length() == 0 || entry.startsWith(path.toString())) {
		                	 String filename = entry.substring(path.length());
		                	 String[] fs = filename.split("/");
		                	 if(fs.length > 1 && fs[0].length() > 0) filename = fs[0] + '/';
		                	 if(fs[0].length() > 0 && (dir == null || dir.equals(fs.length > 1 || filename.endsWith("/"))) && (filter == null || filter.accept(new File(path.toString()), filename))) 
		                		 store.add(path + filename);
		                 }
		             }
			    }
			}catch(Exception e){
	        	 log.error(url + ":list resource error", e);
	        }
		}catch(Exception e){
			log.error(paras[0] + ":list resource error", e);
		}
		for(String p : store) ret.add(new String[]{base == null ? p : p.substring(base.length())});
		return ret;
	}

	@Override
	public Map<K[], IExtendable<Object>> all(K... paras) {
		StringBuilder path = new StringBuilder();
		Map ret = new HashMap<K[], IExtendable<Object>>();
		HashSet<String> store = new HashSet<String>();
		if(base != null) path.append(base);
		if(paras != null && paras.length > 0 && paras[0] != null) path.append(paras[0].toString());
	    if(path.length() > 0 && '/' != path.charAt(path.length() - 1)) path.append('/');
		try{
			List<URL> urls = new LinkedList<URL>();
			if(overlap) {
				Enumeration<URL> rs = classLoader.getResources(path.toString());
				if(rs != null) while(rs.hasMoreElements()) urls.add(rs.nextElement());
			}else {
				URL r = classLoader.getResource(path.toString());
				if(r != null) urls.add(r);
			}
			for(URL url : urls) try{
			    if(url.getProtocol().equals("file")){
				    File file = new File(url.toURI());
				    File[] filenames; 
				    if(filter == null) filenames = file.listFiles();
				    else filenames = file.listFiles(filter);
				    if (filenames != null && filenames.length > 0) for(File f : filenames) try{
				    	if(dir == null || dir.equals(f.isDirectory())) store.add(path.toString() + f.getName());
				    }catch(Exception e){
			        	 log.error(url + ":list resource error", e);
			        }
			    }else if(url.getProtocol().equals("jar")){
			         JarURLConnection urlcon = (JarURLConnection) (url.openConnection());
		        	 JarFile jar = urlcon.getJarFile();
		             Enumeration<JarEntry> entries = jar.entries();
		             while (entries.hasMoreElements()) {
		                 String entry = entries.nextElement().getName();
		                 if (path.length() == 0 || entry.startsWith(path.toString())) {
		                	 String filename = entry.substring(path.length());
		                	 String[] fs = filename.split("/");
		                	 if(fs.length > 1 && fs[0].length() > 0) filename = fs[0] + '/';
		                	 if(fs[0].length() > 0 && (dir == null || dir.equals(fs.length > 1 || filename.endsWith("/"))) && (filter == null || filter.accept(new File(path.toString()), filename))) 
		                		 store.add(path + filename);
		                 }
		             }
			    }
			}catch(Exception e){
	        	 log.error(url + ":list resource error", e);
	        }
		}catch(Exception e){
			log.error(paras[0] + ":list resource error", e);
		}
		for(String p : store) ret.put(new String[]{base == null ? p : p.substring(base.length())}, mockStorage.process(new ClassPathResource(p, classLoader)));
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
