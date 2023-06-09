package net.yeeyaa.eight.annotation.processor;

import java.io.BufferedReader;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.annotation.Annotation;
import java.lang.reflect.Modifier;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import javax.xml.bind.annotation.XmlType;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.core.storage.ByteWStorage;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ContextCallbackProcessor implements IProcessor<String, String> {
	protected final Logger log;
	protected Set<String> resources = Collections.newSetFromMap(new ConcurrentHashMap<String, Boolean>());
	protected volatile Boolean change = false;
	protected String indexName = "jaxb.index";
	protected Class<? extends Annotation> target = XmlType.class;
	protected IProcessor<String, Void> invoker; 
	protected IProcessor<IExtendable<Object>, Void> indexSetter; 
	protected ClassLoader classloader;
	
	public ContextCallbackProcessor() {
		this.log = LoggerFactory.getLogger(ContextCallbackProcessor.class);
	}

	public ContextCallbackProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ContextCallbackProcessor.class) : log;
	}
	
	public void setClassloader(ClassLoader classloader) {
		this.classloader = classloader;
	}

	public void setIndexName(String indexName) {
		if(indexName != null && indexName.trim().length() > 0) this.indexName = indexName.trim();
	}

	public void setTarget(Class<? extends Annotation> target) {
		if(target != null) this.target = target;
	}

	public void setIndexSetter(IProcessor<IExtendable<Object>, Void> indexSetter) {
		this.indexSetter = indexSetter;
	}

	public void setInvoker(IProcessor<String, Void> invoker) {
		this.invoker = invoker;
	}

	@Override
	public String process(String in) {
		if(in != null && in.endsWith(indexName) && in.length() > indexName.length()) {
			resources.add(in.substring(0, in.length() - indexName.length() - 1).replace('/', '.'));
			change = true;
		}
		return in;
	}
	
	public void invoke(){
		if(change) synchronized(this) {
			if (change) {
				StringBuilder sb = new StringBuilder();
				for(String s : resources) sb.append(s).append(':');
				invoker.process(sb.toString());
				change = false;
			}
		}
	}
	
	public void annotationContext(Collection<Class<?>> classes){
		HashMap<String, HashSet<String>> map = new HashMap<String, HashSet<String>>();
		if(classes != null && classes.size() > 0) for(Class<?> c : classes) if(c.isAnnotationPresent(target) && (c.getEnclosingClass() == null || Modifier.isStatic(c.getModifiers())) && c.getPackage() != null){
			String pname = c.getPackage().getName();
			HashSet<String> set = map.get(pname);
			if(set == null){
				set = new HashSet<String>();
				map.put(pname, set);
			}
			set.add(c.getName().substring(pname.length() + 1));
		}
		for(Entry<String, HashSet<String>> entry : map.entrySet()){
			String path = entry.getKey().replace('.', '/') + '/' + indexName;
			InputStream resource = classloader.getResourceAsStream(path);
			StringBuilder file = new StringBuilder();
			HashSet<String> set = entry.getValue();
			if(resource != null) try{		
				String line = null;
			    BufferedReader reader = new BufferedReader(new InputStreamReader(resource));
			    while ((line = reader.readLine()) != null) if(line.trim().length() > 0) {
			    	file.append(line);
			    	set.remove(line);
			    	file.append('\n');
			    }
			    reader.close();
			}catch(Exception e){
				log.error("ContextCallbackProcessor: processor failed.", e);
			}
			if(set.size() > 0){
				for(String line : set) {
					file.append(line);
					file.append('\n');
				}
				ByteWStorage storage = new ByteWStorage();
				storage.setName(path);
				storage.setCache(file.toString().getBytes());
				indexSetter.process(storage);
				resources.add(entry.getKey());
				change = true;
			}
		}
	}
	
	public class AnnotationContext implements IProcessor<Collection<Class<?>>, Collection<Class<?>>>{
		@Override
		public Collection<Class<?>> process(Collection<Class<?>> classes) {
			annotationContext(classes);
			return classes;
		}
	}
	
	public class Invoker implements IProcessor<Object, Object>{
		@Override
		public Object process(Object instance) {
			invoke();
			return instance;
		}
	}
}
