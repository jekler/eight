package net.yeeyaa.eight.core.util;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Enumeration;

import net.yeeyaa.eight.IProcessor;


public class DumbCloassLoader extends ClassLoader implements IProcessor<String, Class<?>>{
	protected IProcessor<String, Class<?>> loader;

	public DumbCloassLoader() {}

	public DumbCloassLoader(ClassLoader parent) {
		super(parent);
	}

	public DumbCloassLoader(IProcessor<String, Class<?>> loader) {
		this.loader = loader;
	}

	public void setLoader(IProcessor<String, Class<?>> loader) {
		this.loader = loader;
	}

	@Override
	public Class<?> loadClass(String name) throws ClassNotFoundException {
		if (loader == null) throw new ClassNotFoundException(name);
		else {
			Class<?> clz = loader.process(name);
			if (clz == null) throw new ClassNotFoundException(name);
			else return clz;
		}
	}

	@Override
	public URL getResource(String name) {
		return null;
	}

	@Override
	public Enumeration<URL> getResources(String name) throws IOException {
		return super.findResources(name);
	}

	@Override
	public InputStream getResourceAsStream(String name) {
		return null;
	}

	@Override
	protected synchronized Class<?> loadClass(String name, boolean resolve)	throws ClassNotFoundException {
		if (loader == null) throw new ClassNotFoundException(name);
		else {
			Class<?> clz = loader.process(name);
			if (clz == null) throw new ClassNotFoundException(name);
			else return clz;
		}
	}

	@Override
	protected Package getPackage(String name) {
		return null;
	}

	@Override
	protected Package[] getPackages() {
		return new Package[0];
	}

	@Override
	public Class<?> process(String name) {
		return loader == null ? null : loader.process(name);
	}
}
