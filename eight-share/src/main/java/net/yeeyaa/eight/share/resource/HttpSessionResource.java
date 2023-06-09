package net.yeeyaa.eight.share.resource;

import java.lang.ref.WeakReference;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.ITriProcessor;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;


public class HttpSessionResource implements IListableResource<Object, Object>, IExtendable<Object>, IProcessor<HttpServletRequest, IListableResource<Object, Object>>, IBiProcessor<HttpServletRequest, String, Object>, ITriProcessor<HttpServletRequest, String, Object, Object> {
	protected static final MapperSet<Object> resourceMethods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected final IProcessor<Boolean, HttpSession> session;
	protected String region;
	protected Set<String> expose;
	protected Set<String> hide;
	protected final IProcessor<Object, Object> count = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object object) {
			Long count = 0L;
			HttpSession s = session.process(false);
			if (s != null) if (region == null) {
				Enumeration e = s.getAttributeNames(); 
				while (e.hasMoreElements()) {
					Object o = e.nextElement();
					if (hide != null && hide.contains(o) || expose != null && !expose.contains(o)) continue;
					count ++;
				}
			} else {
				Object r = s.getAttribute(region);
				if (r instanceof Map) for (Object o : ((Map<Object, Object>)r).keySet()) {
					if (hide != null && hide.contains(o) || expose != null && !expose.contains(o)) continue;
					count ++;
				}
			}
			return count;
		}
	};

	public HttpSessionResource() {
		this.session = null;
	}

	public HttpSessionResource(IProcessor<Boolean, HttpSession> session) {
		this.session = session;
	}

	public void setRegion(String region) {
		this.region = region == null ? UUID.randomUUID().toString() : region.length() == 0 ? null : region;
	}

	public void setExpose(String expose) {
		if (expose != null && expose.trim().length() > 0) {
			HashSet<String> tmp = new HashSet<String>();
			for (String e : expose.split("\\|")) {
				e = e.trim();
				if (e.length() > 0) tmp.add(e);
			}
			if (tmp.size() > 0) this.expose = tmp;
		}
	}

	public void setHide(String hide) {
		if (hide != null && hide.trim().length() > 0) {
			HashSet<String> tmp = new HashSet<String>();
			for (String h : hide.split("\\|")) {
				h = h.trim();
				if (h.length() > 0) tmp.add(h);
			}
			if (tmp.size() > 0) this.hide = tmp;
		}
	}

	@Override
	public Object find(Object ... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) {
			HttpSession s = session.process(false);
			if (s != null) if (region == null) return s.getAttribute(paras[0].toString());
			else {
				Object r = s.getAttribute(region);
				if (r instanceof Map) return ((Map<String, Object>)r).get(paras[0].toString());
			}
		} 
		return null;
	}

	@Override
	public <P> P store(Object value, Object... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null && value != null) {
			HttpSession s = session.process(true);
			if (s != null) if (region == null) s.setAttribute(paras[0].toString(), value);
			else {
				Object r = s.getAttribute(region);
				if (r instanceof Map) ((Map<String, Object>)r).put(paras[0].toString(), value);
				else synchronized(s) {
					r = new ConcurrentHashMap<String, Object>();
					((Map<String, Object>)r).put(paras[0].toString(), value);
					s.setAttribute(region, r);
				}
			}
		}
		return null;
	}

	@Override
	public <P> P discard(Object... paras) {
		if(paras != null && paras.length > 0  && paras[0] != null) {
			HttpSession s = session.process(false);
			if (s != null) if (region == null) s.removeAttribute(paras[0].toString());
			else {
				Object r = s.getAttribute(region);
				if (r instanceof Map) ((Map<String, Object>)r).remove(paras[0].toString());
			}
		} 
		return null;
	}

	@Override
	public <P> P empty(Object... paras) {
		HttpSession s = session.process(false);
		if (s != null) if (region == null) {
			Enumeration e = s.getAttributeNames(); 
			while (e.hasMoreElements()) s.removeAttribute((String)e.nextElement());
		} else {
			Object r = s.getAttribute(region);
			if (r instanceof Map) ((Map<String, Object>)r).clear();
		}
		return null;
	}

	@Override
	public Collection<Object[]> keys(Object... paras) {
		Collection<Object[]> ls = new LinkedList<Object[]>();
		HttpSession s = session.process(false);
		if (s != null) if (region == null) {
			Enumeration e = s.getAttributeNames(); 
			while (e.hasMoreElements()) {
				Object o = e.nextElement();
				if (hide != null && hide.contains(o) || expose != null && !expose.contains(o)) continue;
				ls.add(new String[]{o.toString()});
			}
		} else {
			Object r = s.getAttribute(region);
			if (r instanceof Map) for (String o : ((Map<String, Object>)r).keySet()) {
				if (hide != null && hide.contains(o) || expose != null && !expose.contains(o)) continue;
				ls.add(new String[]{o});
			}
		}
		return ls;
	}

	@Override
	public Map<Object[], Object> all(Object... paras) {
		Map<Object[], Object> map = new HashMap<Object[], Object>();
		HttpSession s = session.process(false);
		if (s != null) if (region == null) {
			Enumeration e = s.getAttributeNames(); 
			while (e.hasMoreElements()) {
				String o = (String)e.nextElement();
				if (hide != null && hide.contains(o) || expose != null && !expose.contains(o)) continue;
				map.put(new String[]{o}, s.getAttribute(o));
			}
		} else {
			Object r = s.getAttribute(region);
			if (r instanceof Map) for (Entry<String, Object> entry : ((Map<String, Object>)r).entrySet()) {
				String o = entry.getKey();
				if (hide != null && hide.contains(o) || expose != null && !expose.contains(o)) continue;
				map.put(new String[]{o}, entry.getValue());
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

	@Override
	public IListableResource<Object, Object> process(HttpServletRequest request) {
		final WeakReference<HttpServletRequest> req = new WeakReference<HttpServletRequest>(request);
		HttpSessionResource ret = new HttpSessionResource(new IProcessor<Boolean, HttpSession>() {
			protected HttpSession session;
			@Override
			public HttpSession process(Boolean create) {
				if (session == null) {
					HttpServletRequest r = req.get();
					if (r != null) session = r.getSession(create);
				}
				return session;
			}
		});
		ret.region = this.region;
		ret.expose = this.expose;
		ret.hide = this.hide;
		return ret;
	}

	@Override
	public Object operate(HttpServletRequest req, String name, Object value) {
		if (req != null && name != null && value != null) {
			HttpSession s = req.getSession();
			if (region == null) s.setAttribute(name, value);
			else {
				Object r = s.getAttribute(region);
				if (r instanceof Map) ((Map<String, Object>)r).put(name, value);
				else synchronized(s) {
					r = new ConcurrentHashMap<String, Object>();
					((Map<String, Object>)r).put(name, value);
					s.setAttribute(region, r);
				}
			}
			return true;
		} else return false;
	}

	@Override
	public Object perform(HttpServletRequest req, String name) {
		if (req != null && name != null) {
			HttpSession s = req.getSession(false);
			if (s != null) if (region == null) return s.getAttribute(name);
			else {
				Object r = s.getAttribute(region);
				if (r instanceof Map) return ((Map<String, Object>)r).get(name);
			}
		} 
		return null;
	}
}
