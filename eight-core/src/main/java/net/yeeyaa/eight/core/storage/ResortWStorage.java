package net.yeeyaa.eight.core.storage;

import java.io.InputStream;
import java.io.OutputStream;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IProcessor;


public class ResortWStorage<T, R extends IExtendable<Object>> extends Storage<Object> {
	protected IProcessor<T, R> storages;
	protected T[] keys;
	protected Integer mode = 0;
	protected volatile R cache;
	protected volatile Boolean no = false;
	
	public void setMode(Integer mode) {
		if(mode != null) this.mode = mode;
	}

	public void setStorages(IProcessor<T, R> storages) {
		this.storages = storages;
	}

	public void setKeys(T[] keys) {
		this.keys = keys;
	}

	protected R resort(){
		if(no) return null;
		if(cache != null && (mode == 1 || mode == 3)) if(!Boolean.TRUE.equals(cache.extend(Method.exists))) cache = null;
		if(cache != null && (mode == 0 || mode == 1 || mode == 2 || mode == 3)) return cache;
		for(T key : keys) {
			R s = storages.process(key);
			if(s != null && Boolean.TRUE.equals(s.extend(Method.exists))) {
				if(mode == 0 || mode == 1) cache = s;
				return s;
			}
		}
		if(mode == 3 || mode == 2) no = true;
		return null;
	}
	
	public InputStream input() {
		return resort().extend(Method.input);
	}

	public OutputStream output() {
		return resort().extend(Method.output);
	}
	
	public Boolean exists() {
		return resort().extend(Method.exists);
	}

	public Object key() {
		return resort().extend(Method.key);
	}

	public Long modified() {
		return resort().extend(Method.modified);
	}
	
	public static class Resort<T, R extends IExtendable<Object>> implements IProcessor<T[], R>{
		protected IProcessor<T, R> storages;
		
		public void setStorages(IProcessor<T, R> storages) {
			this.storages = storages;
		}
		
		@Override
		public R process(T[] keys) {
			for(T key : keys) {
				R s = storages.process(key);
				if(s != null && Boolean.TRUE.equals(s.extend(Method.exists))) return s;
			}
			return null;
		}
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		for (T key : keys) sb.append(key).append(' ');
		return sb.toString();
	}
}
