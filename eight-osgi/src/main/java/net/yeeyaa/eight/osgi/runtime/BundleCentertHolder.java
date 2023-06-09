package net.yeeyaa.eight.osgi.runtime;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;
import net.yeeyaa.eight.IThing;
import net.yeeyaa.eight.ITriProcessor;
import net.yeeyaa.eight.core.PlatformPool;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;
import net.yeeyaa.eight.osgi.IBundleLinker;


public class BundleCentertHolder implements IProcessor<Object, Object>, ITriProcessor<Boolean, Boolean, Boolean, Boolean>, IBiProcessor<Boolean, Boolean, Boolean>, IReadonlyListable<Object, List<IBundleLinker>>, IExtendable<Object>, IThing, ExecutorService {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count, ResourceMethod.status});
	protected final ExecutorService exec = PlatformPool.getCachedPool();
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
			if (status == null) return center.executor == null ? exec : center.executor;
			else if (Boolean.TRUE.equals(paras)) return center.executor;
			else if (Boolean.FALSE.equals(paras)) return exec;
			else return center;
		}
	};
	protected Integer terminate = 10;

	public BundleCentertHolder(BundleCenter center) {
		this.center = center;
	}

	public void setTerminate(Integer terminate) {
		if (terminate != null) this.terminate = terminate;
	}
	
	@PreDestroy
	public void destroy() {
		if (terminate == 0) exec.shutdownNow();
		else {
			exec.shutdown();
			if (terminate > 0) try {
				if (!exec.awaitTermination(terminate, TimeUnit.SECONDS)) exec.shutdownNow();
			} catch (Exception e) {
				center.logger.error("BundleCentertHolder: shutdown fail.", e);
			}
		}
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
		protected Boolean mode; 
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
	public List<IBundleLinker> find(Object... paras) {
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
	public Map<Object[], List<IBundleLinker>> all(Object... paras) {
		Map<Object[], List<IBundleLinker>> map = new HashMap<Object[], List<IBundleLinker>>();
		for(Entry<String, List<IBundleLinker>>  entry : center.getServiceMap().entrySet()) map.put(new String[]{entry.getKey()}, new ArrayList<IBundleLinker>(entry.getValue()));
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

	@Override
	public <L> L present(Class<L> clazz) {
		if (ExecutorService.class.equals(clazz)) return (L) this;
		return null;
	}

	@Override
	public void execute(Runnable command) {
		((center.executor == null ? exec : center.executor) == null ? exec : (center.executor == null ? exec : center.executor)).execute(command);
	}

	@Override
	public void shutdown() {
		if (center.executor == null) exec.shutdown();
	}

	@Override
	public List<Runnable> shutdownNow() {
		if (center.executor == null) return exec.shutdownNow();
		else return Collections.EMPTY_LIST;
	}

	@Override
	public boolean isShutdown() {
		if (center.executor == null) return exec.isShutdown();
		else return true;
	}

	@Override
	public boolean isTerminated() {
		if (center.executor == null) return exec.isTerminated();
		else return true;
	}

	@Override
	public boolean awaitTermination(long timeout, TimeUnit unit) throws InterruptedException {
		if (center.executor == null) return exec.awaitTermination(timeout, unit);
		else return true;
	}

	@Override
	public <T> Future<T> submit(Callable<T> task) {
		return (center.executor == null ? exec : center.executor).submit(task);
	}

	@Override
	public <T> Future<T> submit(Runnable task, T result) {
		return (center.executor == null ? exec : center.executor).submit(task, result);
	}

	@Override
	public Future<?> submit(Runnable task) {
		return (center.executor == null ? exec : center.executor).submit(task);
	}

	@Override
	public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks) throws InterruptedException {
		return (center.executor == null ? exec : center.executor).invokeAll(tasks);
	}

	@Override
	public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks, long timeout, TimeUnit unit) throws InterruptedException {
		return (center.executor == null ? exec : center.executor).invokeAll(tasks, timeout, unit);
	}

	@Override
	public <T> T invokeAny(Collection<? extends Callable<T>> tasks)	throws InterruptedException, ExecutionException {
		return (center.executor == null ? exec : center.executor).invokeAny(tasks);
	}

	@Override
	public <T> T invokeAny(Collection<? extends Callable<T>> tasks,	long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
		return (center.executor == null ? exec : center.executor).invokeAny(tasks, timeout, unit);
	}
}
