package net.yeeyaa.eight.core.processor;

import java.io.IOException;
import java.net.URL;
import java.util.Collections;
import java.util.Enumeration;
import java.util.LinkedList;

import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ClassLoaderProcessor extends ClassLoader implements IProcessor<String, Class<?>> {
	protected final Logger log;
	protected ClassLoader classLoader; 
    protected IProcessor<String, Class<?>> processor;
    protected IProcessor<Object, ClassLoader> loaderProcessor;
    protected Object key;
	protected IProcessor<URL, URL> resourceProcessor;
	
	public ClassLoaderProcessor() {
		this.log = LoggerFactory.getLogger(ClassLoaderProcessor.class);
	}
	
	public ClassLoaderProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ClassLoaderProcessor.class) : log;
	}
	
	public void setResourceProcessor(IProcessor<URL, URL> resourceProcessor) {
		this.resourceProcessor = resourceProcessor;
	}

	public void setLoaderProcessor(IProcessor<Object, ClassLoader> loaderProcessor) {
		this.loaderProcessor = loaderProcessor;
	}

	public void setKey(Object key) {
		this.key = key;
	}

	public void setClassLoader(ClassLoader classLoader) {
		this.classLoader = classLoader;
	}

	public void setProcessor(IProcessor<String, Class<?>> processor) {
		this.processor = processor;
	}

	@Override
	public Class<?> process(String instance) {
		if(instance != null) if(classLoader != null) try {
			return classLoader.loadClass(instance);
		} catch (ClassNotFoundException e) {
        	log.error("ClassLoaderProcessor : process error.", e);
		} else if(loaderProcessor != null) try {
			ClassLoader cl = loaderProcessor.process(key);
			if(cl != null) return cl.loadClass(instance);
		} catch (ClassNotFoundException e) {
        	log.error("ClassLoaderProcessor : process error.", e);
		} else if(processor != null) return processor.process(instance);
		else try {
			return super.loadClass(instance);
		} catch (ClassNotFoundException e) {
        	log.error("ClassLoaderProcessor : process error.", e);
		} 
		return null;
	}

	@Override
	public Class<?> loadClass(String name) throws ClassNotFoundException {
		if(name != null) if(processor != null) return processor.process(name);
		else if(loaderProcessor != null) {
			ClassLoader cl = loaderProcessor.process(key);
			if(cl != null) return cl.loadClass(name);
		} else if(classLoader != null) return classLoader.loadClass(name);
		else return super.loadClass(name);
		throw new ClassNotFoundException(name);
	}

	@Override
	public URL getResource(String name) {
		URL ret = null;
		if(loaderProcessor != null) {
			ClassLoader cl = loaderProcessor.process(key);
			if(cl != null) ret = cl.getResource(name);
		} else if(classLoader != null) ret = classLoader.getResource(name);
		else ret = super.getResource(name);
		return ret == null ? null : resourceProcessor == null ? ret : resourceProcessor.process(ret);
	}

	@Override
	public Enumeration<URL> getResources(String name) throws IOException {
		Enumeration<URL> ret = null;
		if(loaderProcessor != null) {
			ClassLoader cl = loaderProcessor.process(key);
			if(cl != null) ret = cl.getResources(name);
		} else if(classLoader != null) ret = classLoader.getResources(name);
		else ret = super.getResources(name);
		if (ret == null) ret = Collections.enumeration(Collections.<URL>emptySet());
		else if (resourceProcessor != null) {
			LinkedList<URL> us = new LinkedList<URL>();
			while (ret.hasMoreElements()) us.add(resourceProcessor.process(ret.nextElement()));
			ret = Collections.enumeration(us); 
		}
		return ret;
	}
}
