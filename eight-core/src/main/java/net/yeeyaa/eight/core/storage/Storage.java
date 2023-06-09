package net.yeeyaa.eight.core.storage;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.Collection;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.core.util.MapperSet;


public abstract class Storage<T> implements IExtendable<Object> {
	public enum Method {
		input, output, exists, key, modified, list;
	}
	public static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{Method.input, Method.output, Method.exists, Method.key, Method.modified, Method.list});
	
	@Override
	public Collection<Object> methods() {
		return methods;
	}

	@Override
	public <N> N extend(Object object) {
		if (object != null) {
			Object method = object instanceof Method ? object : methods.process(object);
			if (method!= null) switch((Method) method) {
				case input : return (N) input();
				case output : return (N) output();
				case exists : return (N) exists();
				case key : return (N) key();
				case modified : return (N) modified();
				case list : return (N) list();
			}
		}
		return null;
	}
	
	public Boolean exists() {return null;}
	
	public T key() {return null;}
	
	public Long modified() {return null;}
	
	public InputStream input() {return null;}
	
	public OutputStream output() {return null;}
	
	public Collection<T> list() {return null;}
}
