package net.yeeyaa.eight.share.resource;

import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import javax.servlet.http.HttpSession;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;


public class HttpSessionResource implements IListableResource<Object, Object>, IExtendable<Object> {
	protected static final MapperSet<Object> resourceMethods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected IProcessor<Void, HttpSession> session;
	protected final IProcessor<Object, Object> count = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object object) {
			Long count = 0L;
			HttpSession s = session.process(null);
			if (s != null) {
				Enumeration e = s.getAttributeNames(); 
				while (e.hasMoreElements()) count ++;
			}
			return count;
		}
	};
	
	public void setSession(IProcessor<Void, HttpSession> session) {
		this.session = session;
	}

	@Override
	public Object find(Object ... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) {
			HttpSession s = session.process(null);
			if (s != null) return s.getAttribute(paras[0].toString());
		} 
		return null;
	}

	@Override
	public <P> P store(Object value, Object... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) {
			HttpSession s = session.process(null);
			if (s != null) s.setAttribute(paras[0].toString(), value);
		}
		return null;
	}

	@Override
	public <P> P discard(Object... paras) {
		if(paras != null && paras.length > 0  && paras[0] != null) {
			HttpSession s = session.process(null);
			if (s != null) s.removeAttribute(paras[0].toString());
		} 
		return null;
	}

	@Override
	public <P> P empty(Object... paras) {
		HttpSession s = session.process(null);
		if (s != null) {
			Enumeration e = s.getAttributeNames(); 
			while (e.hasMoreElements()) s.removeAttribute((String)e.nextElement());
		}
		return null;
	}

	@Override
	public Collection<Object[]> keys(Object... paras) {
		Collection<Object[]> ls = new LinkedList<Object[]>();
		HttpSession s = session.process(null);
		if (s != null) {
			Enumeration e = s.getAttributeNames(); 
			while (e.hasMoreElements()) ls.add(new String[]{(String)e.nextElement()});
		}
		return ls;
	}

	@Override
	public Map<Object[], Object> all(Object... paras) {
		Map<Object[], Object> map = new HashMap<Object[], Object>();
		HttpSession s = session.process(null);
		if (s != null) {
			Enumeration e = s.getAttributeNames(); 
			while (e.hasMoreElements()) {
				String key = (String)e.nextElement();
				map.put(new String[]{key}, s.getAttribute(key));
			}
		}
		return map;
	}

	@Override
	public <N> N extend(Object object) {
		if (object != null) {
			Object method = object instanceof ResourceMethod ? object : resourceMethods.process(object);
			if (method!= null) switch((ResourceMethod) method) {
				case count : return (N) count;
			}
		}
		return null;
	}
	
	@Override
	public Collection<Object> methods() {
		return resourceMethods;
	}
}