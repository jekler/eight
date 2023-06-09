package net.yeeyaa.eight.core;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.slf4j.Marker;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;

public class PlatformLogger<K, V> implements IProcessor<Map<Object, Object>, IProcessor<Map<Object, Object>, Map<String, String>>>, IListableResource<K, V>, IExtendable<Object>, Logger {
	protected static final Logger log = LoggerFactory.getLogger(PlatformLogger.class);
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected IListable<Object, Object> resource;
	protected Boolean clear;
	protected final IProcessor<String[], Long> count = new IProcessor<String[], Long>(){
		@Override
		public Long process(String[] paras) {
			return new Long(MDC.getCopyOfContextMap().size());
		}
	};
	
	public void setClear(Boolean clear) {
		this.clear = clear;
	}

	public void setResource(IListable<Object, Object> resource) {
		this.resource = resource;
	}
	
	protected Collection<String> before() {
		try {
			if (Boolean.TRUE.equals(clear)) MDC.clear();
			Map<Object[], Object> all = resource.all(new String[0]);
			if (all != null && all.size() > 0) {
				Collection<String> ret = Boolean.FALSE.equals(clear) ? new ArrayList<String>(all.size()) : null;
				for (Entry<Object[], Object> entry : all.entrySet()) if (entry.getKey().length > 0 && entry.getKey()[0] != null) {
					MDC.put(entry.getKey()[0].toString(), entry.getValue() == null ? null : entry.getValue().toString());
					if (Boolean.FALSE.equals(clear)) ret.add(entry.getKey()[0].toString());
				}
				return ret;
			}
		} catch (Exception e) {
			log.error("log fail.", e);
		}
		return null;
	}
	
	protected void after(Collection<String> keys) {
		if (clear != null) if (clear) MDC.clear();
		else if (keys != null && keys.size() >0) for (String key : keys) MDC.remove(key);
	}
	
	@Override
	public IProcessor<Map<Object, Object>, Map<String, String>> process(Map<Object, Object> object) {
		final Map<String, String> map = new HashMap<String, String>();
		Map<String, String> tmp = MDC.getCopyOfContextMap();
		if (tmp != null && tmp.size() > 0) map.putAll(tmp);
		if (object != null && object.size() > 0) for (Entry<Object, Object> entry : object.entrySet()) if (entry.getKey() != null) map.put(entry.getKey().toString(), entry.getValue() == null ? null :  entry.getValue().toString());
		return new IProcessor<Map<Object, Object>, Map<String, String>>(){
			@Override
			public Map<String, String> process(Map<Object, Object> object) {
				MDC.setContextMap(map);
				if (object != null && object.size() > 0) for (Entry<Object, Object> entry : object.entrySet()) if (entry.getKey() != null)  MDC.put(entry.getKey().toString(), entry.getValue() == null ? null :  entry.getValue().toString());
				return map;
			}		
		};
	}
	
	@Override
	public V find(K... paras) {
		if (paras != null && paras.length >0 && paras[0] != null) return (V) MDC.get(paras[0].toString());
		else return null;
	}

	@Override
	public <P> P store(V value, K... paras) {
		if (paras != null && paras.length >0 && paras[0] != null) MDC.put(paras[0].toString(), value == null ? null : value.toString());
		return null;
	}

	@Override
	public <P> P discard(K... paras) {
		if (paras != null && paras.length >0 && paras[0] != null) MDC.remove(paras[0].toString());
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		MDC.clear();
		return null;
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		Collection<String> keys = MDC.getCopyOfContextMap().keySet();
		Collection ret = new ArrayList<V[]>(keys.size());
		for (String key : keys) ret .add(new String[]{key});
		return ret;
	}

	@Override
	public Map<K[], V> all(K... paras) {
		Map<String, String> kvs = MDC.getCopyOfContextMap();
		Map ret = new HashMap<String[], String>(kvs.size() * 2);
		for (Entry<String, String> entry : kvs.entrySet()) ret.put(new String[]{entry.getKey()}, entry.getValue());
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
	
	@Override
	public String getName() {
		return log.getName();
	}

	@Override
	public boolean isTraceEnabled() {
		return log.isTraceEnabled();
	}

	@Override
	public void trace(String msg) {
		Collection<String> keys = before();
		try {
			log.trace(msg);
		} finally {
			after(keys);
		}	
	}

	@Override
	public void trace(String format, Object arg) {
		Collection<String> keys = before();
		try {
			log.trace(format, arg);
		} finally {
			after(keys);
		}	
	}

	@Override
	public void trace(String format, Object arg1, Object arg2) {
		Collection<String> keys = before();
		try {
			log.trace(format, arg1, arg2);
		} finally {
			after(keys);
		}	
	}

	@Override
	public void trace(String format, Object... arguments) {
		Collection<String> keys = before();
		try {
			log.trace(format, arguments);
		} finally {
			after(keys);
		}	
	}

	@Override
	public void trace(String msg, Throwable t) {
		Collection<String> keys = before();
		try {
			log.trace(msg, t);
		} finally {
			after(keys);
		}	
	}

	@Override
	public boolean isTraceEnabled(Marker marker) {
		return log.isTraceEnabled(marker);
	}

	@Override
	public void trace(Marker marker, String msg) {
		Collection<String> keys = before();
		try {
			log.trace(marker, msg);
		} finally {
			after(keys);
		}
	}

	@Override
	public void trace(Marker marker, String format, Object arg) {
		Collection<String> keys = before();
		try {
			log.trace(marker, format, arg);
		} finally {
			after(keys);
		}
	}

	@Override
	public void trace(Marker marker, String format, Object arg1, Object arg2) {
		Collection<String> keys = before();
		try {
			log.trace(marker, format, arg1, arg2);
		} finally {
			after(keys);
		}
	}

	@Override
	public void trace(Marker marker, String format, Object... argArray) {
		Collection<String> keys = before();
		try {
			log.trace(marker, format, argArray);
		} finally {
			after(keys);
		}
	}

	@Override
	public void trace(Marker marker, String msg, Throwable t) {
		Collection<String> keys = before();
		try {
			log.trace(marker, msg, t);
		} finally {
			after(keys);
		}
	}

	@Override
	public boolean isDebugEnabled() {
		return log.isDebugEnabled();
	}

	@Override
	public void debug(String msg) {
		Collection<String> keys = before();
		try {
			log.debug(msg);
		} finally {
			after(keys);
		}
	}

	@Override
	public void debug(String format, Object arg) {
		Collection<String> keys = before();
		try {
			log.debug(format, arg);
		} finally {
			after(keys);
		}
	}

	@Override
	public void debug(String format, Object arg1, Object arg2) {
		Collection<String> keys = before();
		try {
			log.debug(format, arg1, arg2);
		} finally {
			after(keys);
		}
	}

	@Override
	public void debug(String format, Object... arguments) {
		Collection<String> keys = before();
		try {
			log.debug(format, arguments);
		} finally {
			after(keys);
		}
	}

	@Override
	public void debug(String msg, Throwable t) {
		Collection<String> keys = before();
		try {
			log.debug(msg, t);
		} finally {
			after(keys);
		}
	}

	@Override
	public boolean isDebugEnabled(Marker marker) {
		return log.isDebugEnabled(marker);
	}

	@Override
	public void debug(Marker marker, String msg) {
		Collection<String> keys = before();
		try {
			log.debug(marker, msg);
		} finally {
			after(keys);
		}
	}

	@Override
	public void debug(Marker marker, String format, Object arg) {
		Collection<String> keys = before();
		try {
			log.debug(marker, format, arg);
		} finally {
			after(keys);
		}
	}

	@Override
	public void debug(Marker marker, String format, Object arg1, Object arg2) {
		Collection<String> keys = before();
		try {
			log.debug(marker, format, arg1, arg2);
		} finally {
			after(keys);
		}
	}

	@Override
	public void debug(Marker marker, String format, Object... arguments) {
		Collection<String> keys = before();
		try {
			log.debug(marker, format, arguments);
		} finally {
			after(keys);
		}
	}

	@Override
	public void debug(Marker marker, String msg, Throwable t) {
		Collection<String> keys = before();
		try {
			log.debug(marker, msg, t);
		} finally {
			after(keys);
		}
	}

	@Override
	public boolean isInfoEnabled() {
		return log.isInfoEnabled();
	}

	@Override
	public void info(String msg) {
		Collection<String> keys = before();
		try {
			log.info(msg);
		} finally {
			after(keys);
		}
	}

	@Override
	public void info(String format, Object arg) {
		Collection<String> keys = before();
		try {
			log.info(format, arg);
		} finally {
			after(keys);
		}
	}

	@Override
	public void info(String format, Object arg1, Object arg2) {
		Collection<String> keys = before();
		try {
			log.info(format, arg1, arg2);
		} finally {
			after(keys);
		}
	}

	@Override
	public void info(String format, Object... arguments) {
		Collection<String> keys = before();
		try {
			log.info(format, arguments);
		} finally {
			after(keys);
		}
	}

	@Override
	public void info(String msg, Throwable t) {
		Collection<String> keys = before();
		try {
			log.info(msg, t);
		} finally {
			after(keys);
		}
	}

	@Override
	public boolean isInfoEnabled(Marker marker) {
		return log.isInfoEnabled(marker);
	}

	@Override
	public void info(Marker marker, String msg) {
		Collection<String> keys = before();
		try {
			log.info(marker, msg);
		} finally {
			after(keys);
		}
	}

	@Override
	public void info(Marker marker, String format, Object arg) {
		Collection<String> keys = before();
		try {
			log.info(marker, format, arg);
		} finally {
			after(keys);
		}
	}

	@Override
	public void info(Marker marker, String format, Object arg1, Object arg2) {
		Collection<String> keys = before();
		try {
			log.info(marker, format, arg1, arg2);
		} finally {
			after(keys);
		}
	}

	@Override
	public void info(Marker marker, String format, Object... arguments) {
		Collection<String> keys = before();
		try {
			log.info(marker, format, arguments);
		} finally {
			after(keys);
		}
	}

	@Override
	public void info(Marker marker, String msg, Throwable t) {
		Collection<String> keys = before();
		try {
			log.info(marker, msg, t);
		} finally {
			after(keys);
		}
	}

	@Override
	public boolean isWarnEnabled() {
		return log.isWarnEnabled();
	}

	@Override
	public void warn(String msg) {
		Collection<String> keys = before();
		try {
			log.warn(msg);
		} finally {
			after(keys);
		}
	}

	@Override
	public void warn(String format, Object arg) {
		Collection<String> keys = before();
		try {
			log.warn(format, arg);
		} finally {
			after(keys);
		}
	}

	@Override
	public void warn(String format, Object... arguments) {
		Collection<String> keys = before();
		try {
			log.warn(format, arguments);
		} finally {
			after(keys);
		}
	}

	@Override
	public void warn(String format, Object arg1, Object arg2) {
		Collection<String> keys = before();
		try {
			log.warn(format, arg1, arg2);
		} finally {
			after(keys);
		}
	}

	@Override
	public void warn(String msg, Throwable t) {
		Collection<String> keys = before();
		try {
			log.warn(msg, t);
		} finally {
			after(keys);
		}
	}

	@Override
	public boolean isWarnEnabled(Marker marker) {
		return log.isWarnEnabled(marker);
	}

	@Override
	public void warn(Marker marker, String msg) {
		Collection<String> keys = before();
		try {
			log.warn(marker, msg);
		} finally {
			after(keys);
		}
	}

	@Override
	public void warn(Marker marker, String format, Object arg) {
		Collection<String> keys = before();
		try {
			log.warn(marker, format, arg);
		} finally {
			after(keys);
		}
	}

	@Override
	public void warn(Marker marker, String format, Object arg1, Object arg2) {
		Collection<String> keys = before();
		try {
			log.warn(marker, format, arg1, arg2);
		} finally {
			after(keys);
		}
	}

	@Override
	public void warn(Marker marker, String format, Object... arguments) {
		Collection<String> keys = before();
		try {
			log.warn(marker, format, arguments);
		} finally {
			after(keys);
		}
	}

	@Override
	public void warn(Marker marker, String msg, Throwable t) {
		Collection<String> keys = before();
		try {
			log.warn(marker, msg, t);
		} finally {
			after(keys);
		}
	}

	@Override
	public boolean isErrorEnabled() {
		return log.isErrorEnabled();
	}

	@Override
	public void error(String msg) {
		Collection<String> keys = before();
		try {
			log.error(msg);
		} finally {
			after(keys);
		}
	}

	@Override
	public void error(String format, Object arg) {
		Collection<String> keys = before();
		try {
			log.error(format, arg);
		} finally {
			after(keys);
		}
	}

	@Override
	public void error(String format, Object arg1, Object arg2) {
		Collection<String> keys = before();
		try {
			log.error(format, arg1, arg2);
		} finally {
			after(keys);
		}
	}

	@Override
	public void error(String format, Object... arguments) {
		Collection<String> keys = before();
		try {
			log.error(format, arguments);
		} finally {
			after(keys);
		}
	}

	@Override
	public void error(String msg, Throwable t) {
		Collection<String> keys = before();
		try {
			log.error(msg, t);
		} finally {
			after(keys);
		}
	}

	@Override
	public boolean isErrorEnabled(Marker marker) {
		return log.isErrorEnabled(marker);
	}

	@Override
	public void error(Marker marker, String msg) {
		Collection<String> keys = before();
		try {
			log.error(marker, msg);
		} finally {
			after(keys);
		}
	}

	@Override
	public void error(Marker marker, String format, Object arg) {
		Collection<String> keys = before();
		try {
			log.error(marker, format, arg);
		} finally {
			after(keys);
		}
	}

	@Override
	public void error(Marker marker, String format, Object arg1, Object arg2) {
		Collection<String> keys = before();
		try {
			log.error(marker, format, arg1, arg2);
		} finally {
			after(keys);
		}
	}

	@Override
	public void error(Marker marker, String format, Object... arguments) {
		Collection<String> keys = before();
		try {
			log.error(marker, format, arguments);
		} finally {
			after(keys);
		}
	}

	@Override
	public void error(Marker marker, String msg, Throwable t) {
		Collection<String> keys = before();
		try {
			log.error(marker, msg, t);
		} finally {
			after(keys);
		}
	}
}
