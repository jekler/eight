package net.yeeyaa.eight.osgi.runtime;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;
import net.yeeyaa.eight.ITriProcessor;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;
import net.yeeyaa.eight.osgi.IBundleProxy;


public class BundleCentertHolder implements IProcessor<Object, Object>, ITriProcessor<Boolean, Boolean, Boolean, Boolean>, IBiProcessor<Boolean, Boolean, Boolean>, IReadonlyListable<Object, List<IBundleProxy>>, IExtendable<Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count, ResourceMethod.status});
	protected final BundleCenter center;
	protected final IProcessor<Object, Object> count = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object paras) {
			return new Long(center.getServiceMap().size());
		}
	};
	protected final IProcessor<Object, Object> status = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object paras) {
			return center;
		}
	};
	
	public BundleCentertHolder(BundleCenter center) {
		this.center = center;
	}

	@Override
	public Object process(Object instance) {
		return center.getService(instance.toString());
	}

	@Override
	public Boolean perform(Boolean state, Boolean kill) {
		return center.state(state, kill);
	}

	@Override
	public Boolean operate(Boolean mode, Boolean status, Boolean flag) {
		return center.control(mode, status, flag);
	}
	
	public class ConfigResource implements IListableResource<Object, String>, IExtendable<Object> {
		@Override
		public String find(Object... paras) {
			return center.find(paras);
		}

		public <P> P store(String value, Object... paras) {
			return center.store(value, paras);	
		}

		@Override
		public <P> P  discard(Object... paras) {
			return center.discard(paras);
		}

		@Override
		public <P> P empty(Object... paras) {
			return center.empty(paras);
		}

		@Override
		public Collection<Object[]> keys(Object... paras) {
			return center.keys(paras);
		}

		@Override
		public Map<Object[], String> all(Object... paras) {
			return center.all(paras);
		}

		@Override
		public <N> N extend(Object method) {
			return center.extend(method);
		}

		@Override
		public Collection<Object> methods() {
			return center.methods();
		}
	}

	public class InfoResource implements IListableResource<Object, Object>, IExtendable<Object> {
		protected Boolean mode; // ture: permit, false: paras, null: properties
		protected Boolean readonly;
		protected final IProcessor<Object, Object> count = new IProcessor<Object, Object>(){
			@Override
			public Object process(Object paras) {
				Map<String, ?> infos = mode == null ? center.properties : mode ? center.permit : center.paras;
				return new Long(infos.size());
			}
		};
		
		public void setReadonly(Boolean readonly) {
			this.readonly = readonly;
		}

		public void setMode(Boolean mode) {
			this.mode = mode;
		}

		@Override
		public Object find(Object... paras) {
			if (paras != null && paras.length > 0) {
				Map<String, ?> infos = mode == null ? center.properties : mode ? center.permit : center.paras;
				return infos.get(paras[0]);
			} else return null;
		}

		@Override
		public <P> P store(Object value, Object... paras) {
			if (Boolean.FALSE.equals(readonly)) {
				Map infos = mode == null ? center.properties : mode ? center.permit : center.paras;
				if(paras != null && paras.length > 0 && paras[0] != null) infos.put(paras[0].toString(), value);
			}
			return null;
		}

		@Override
		public <P> P discard(Object... paras) {
			if (Boolean.FALSE.equals(readonly)) {
				Map<String, ?> infos = mode == null ? center.properties : mode ? center.permit : center.paras;
				if(paras != null && paras.length > 0 && paras[0] != null) infos.remove(paras[0]);
			}
			return null;
		}

		@Override
		public <P> P empty(Object... paras) {
			if(Boolean.FALSE.equals(readonly)) if (mode == null) center.properties = new ConcurrentHashMap<String, Object>();
			else if (mode) center.permit = new ConcurrentHashMap<String, Set<String>>();
			else center.paras = new ConcurrentHashMap<String, Object>();
			return null;
		}
		
		@Override
		public Collection<Object[]> keys(Object... paras) {
			Map<String, ?> infos = mode == null ? center.properties : mode ? center.permit : center.paras;
			Collection<Object[]> keys = new ArrayList<Object[]>(infos.size());
			for (String key : infos.keySet()) keys.add(new String[]{key});
			return keys;
		}

		@Override
		public Map<Object[], Object> all(Object... paras) {
			Map<String, ?> infos = mode == null ? center.properties : mode ? center.permit : center.paras;
			Map<Object[], Object> all = new HashMap<Object[], Object>(infos.size() * 2);
			for (Entry<String, ?> entry : infos.entrySet()) all.put(new String[]{entry.getKey()}, entry.getValue());
			return all;
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
	
	@Override
	public List<IBundleProxy> find(Object... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null){
			return center.getServiceMap().get(paras[0]);
		}else return null;
	}

	@Override
	public Collection<Object[]> keys(Object... paras) {
		ArrayList<Object[]> list = new ArrayList<Object[]>(center.getServiceMap().size());
		for(String key : center.getServiceMap().keySet()) list.add(new String[]{key});
		return list;
	}

	@Override
	public Map<Object[], List<IBundleProxy>> all(Object... paras) {
		Map<Object[], List<IBundleProxy>> map = new HashMap<Object[], List<IBundleProxy>>();
		for(Entry<String, List<IBundleProxy>>  entry : center.getServiceMap().entrySet()) map.put(new String[]{entry.getKey()}, new ArrayList<IBundleProxy>(entry.getValue()));
		return map;
	}

	@Override
	public <N> N extend(Object object) {
		if (object != null) {
			Object method = object instanceof ResourceMethod ? object : methods.process(object);
			if (method!= null) switch((ResourceMethod) method) {
				case count : return (N) count;
				case status : return (N) status;
			}
		}
		return null;
	}
	
	@Override
	public Collection<Object> methods() {
		return methods;
	}
}
