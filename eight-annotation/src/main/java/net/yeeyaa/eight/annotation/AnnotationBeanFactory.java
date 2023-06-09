package net.yeeyaa.eight.annotation;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;
import net.yeeyaa.eight.annotation.tag.ServiceClass;
import net.yeeyaa.eight.annotation.tag.ServiceScope;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.ListableBeanFactory;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.core.ResolvableType;


public class AnnotationBeanFactory implements ListableBeanFactory ,IProcessor<Collection<Class<?>>, IReadonlyListable<Object, Class<?>>>{
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected final Logger log;	
	protected volatile HashMap<String, Bean> beans = new HashMap<String, Bean>();
	protected volatile HashMap<Class<?>, Bean> types = new HashMap<Class<?>, Bean>();
	protected IProcessor<Object, Object> beanHolder;
	protected IProcessor<Class<?>, Object> newInstance;	
	protected String context = "context";
	protected Boolean overlap = false;
	
	public AnnotationBeanFactory() {
		this.log = LoggerFactory.getLogger(AnnotationBeanFactory.class);
	}

	public AnnotationBeanFactory(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(AnnotationBeanFactory.class) : log;
	}
	
	protected class Bean<T>{
		protected Class<T> clz;
		protected String name;
		protected Boolean singleton;
		protected Method init;
		protected Method destroy;
		protected Field context;
		protected volatile T instance;
		
		protected Bean(Class<T> clz, String name, Boolean singleton, Method init, Method destroy, Field context) {
			this.clz = clz;
			this.name = name;
			this.singleton = singleton;
			this.init = init;
			this.destroy = destroy;
			this.context = context;
		}

		protected T getInstance(){
			T ret = null;
			if(instance != null) ret = instance;
			else try{
				if(singleton) synchronized(this){
					if(instance == null) instance = newInstance();
					ret = instance;
				}else ret = newInstance();
			}catch(Exception e){
				log.error("AnnotationBeanFactory: performing failed.", e);
			}
			return ret;
		}
		
		protected T newInstance() throws Exception{
			T ret = (T) AnnotationBeanFactory.this.newInstance(clz);
			if(ret != null && context != null && beanHolder != null) context.set(ret, beanHolder);
			if(ret != null && init != null) init.invoke(ret);
			return ret;
		}
		
		protected void destroy(){
			if(instance != null && destroy != null)	try {
				destroy.invoke(instance);
			} catch (Exception e) {
				log.error("AnnotationBeanFactory: performing failed.", e);
			}
		}
	}
	
	protected static class PrivateListable<M> implements IReadonlyListable<Object, Class<?>>, IExtendable<M> {
		protected Map<String, Class<?>> map;
		protected final IProcessor<Object, Object> count = new IProcessor<Object, Object>(){
			@Override
			public Object process(Object paras) {
				return new Long(map.size());
			}
		};
		
		protected PrivateListable(Map<String, Class<?>> map) {
			this.map = map;
		}

		@Override
		public Class<?> find(Object... paras) {
			if(paras!= null && paras.length > 0 && paras[0] != null) return map.get(paras[0]);
			else return null;
		}

		@Override
		public Collection<Object[]> keys(Object... paras) {
			ArrayList<Object[]> list = new ArrayList<Object[]>(map.size());
			for(String name : map.keySet()) list.add(new String[]{name});
			return list;
		}

		@Override
		public Map<Object[], Class<?>> all(Object... paras) {
			Map<Object[], Class<?>> ret = new HashMap<Object[], Class<?>>();
			for(Entry<String, Class<?>> entry : map.entrySet()) ret.put(new String[]{entry.getKey()}, entry.getValue());
			return ret;
		}

		@Override
		public <N> N extend(M object) {
			if (object != null) {
				Object method = object instanceof ResourceMethod ? object : methods.process(object);
				if (method!= null) switch((ResourceMethod) method) {
					case count : return (N) count;
				}
			}
			return null;
		}
		
		@Override
		public Collection<M> methods() {
			return (Collection<M>) methods;
		}
	}
	
	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}

	public void setNewInstance(IProcessor<Class<?>, Object> newInstance) {
		this.newInstance = newInstance;
	}

	public void setOverlap(Boolean overlap) {
		if(overlap != null) this.overlap = overlap;
	}

	public void setContext(String context) {
		if(context != null && context.trim().length() > 0) this.context = context.trim();
	}

	protected Object newInstance(Class<?> clz) throws Exception {
		if (newInstance == null) return clz.newInstance();
		else return newInstance.process(clz);
	}
	
	@PreDestroy
	public synchronized void destroy(){
		HashMap<String, Bean> old = beans;
		beans = new HashMap<String, Bean>();
		types = new HashMap<Class<?>, Bean>();
		for(Bean bean : old.values()) bean.destroy();
	}
	
	@Override
	public Object getBean(String name) throws BeansException {
		Bean<Object> bean = beans.get(name);
		if(bean != null) return bean.getInstance();
		else return null;
	}
	
	@Override
	public <T> T getBean(String name, Class<T> requiredType) throws BeansException {
		Bean<T> bean = beans.get(name);
		if(bean == null) bean = types.get(requiredType);
		if(bean != null) return bean.getInstance();
		else return null;
	}

	@Override
	public <T> T getBean(Class<T> requiredType) throws BeansException {
		Bean<T> bean = types.get(requiredType);
		if(bean != null) return bean.getInstance();
		else return null;
	}

	@Override
	public Object getBean(String name, Object... args) throws BeansException {
		Bean<Object> bean = beans.get(name);
		if(bean != null) return bean.getInstance();
		else return null;
	}

	@Override
	public <T> T getBean(Class<T> requiredType, Object... args) throws BeansException {
		Bean<T> bean = types.get(requiredType);
		if(bean != null) return bean.getInstance();
		else return null;
	}

	@Override
	public boolean isTypeMatch(String name, ResolvableType type) throws NoSuchBeanDefinitionException {
		Bean<Object> bean = beans.get(name);
		if(bean != null) return type.isInstance(bean.getInstance());
		return false;
	}

	@Override
	public String[] getBeanNamesForType(ResolvableType type) {
		return getBeanNamesForType(type.resolve(), true, true);
	}
	
	@Override
	public boolean containsBean(String name) {
		return beans.containsKey(name);
	}

	@Override
	public boolean isSingleton(String name) throws NoSuchBeanDefinitionException {
		Bean<Object> bean = beans.get(name);
		if(bean != null) return bean.singleton;
		else throw new NoSuchBeanDefinitionException(name);
	}

	@Override
	public boolean isPrototype(String name)throws NoSuchBeanDefinitionException {
		Bean<Object> bean = beans.get(name);
		if(bean != null) return !bean.singleton;
		else throw new NoSuchBeanDefinitionException(name);
	}

	@Override
	public boolean isTypeMatch(String name, Class<?> targetType) throws NoSuchBeanDefinitionException {
		Bean<Object> bean = beans.get(name);
		if(bean != null) return bean.clz.equals(targetType);
		else throw new NoSuchBeanDefinitionException(name);
	}

	@Override
	public Class<?> getType(String name) throws NoSuchBeanDefinitionException {
		Bean bean = beans.get(name);
		if(bean != null) return bean.clz;
		else throw new NoSuchBeanDefinitionException(name);
	}

	@Override
	public String[] getAliases(String name) {
		return new String[0];
	}

	@Override
	public boolean containsBeanDefinition(String beanName) {
		return beans.containsKey(beanName);
	}

	@Override
	public int getBeanDefinitionCount() {
		return beans.size();
	}

	@Override
	public String[] getBeanDefinitionNames() {
		Collection<String> names = beans.keySet();
		return names.toArray(new String[names.size()]);
	}

	@Override
	public String[] getBeanNamesForType(Class<?> type) {
		return getBeanNamesForType(type, true, true);
	}

	@Override
	public String[] getBeanNamesForType(Class<?> type, boolean includeNonSingletons, boolean allowEagerInit) {
		Bean<?> bean = types.get(type);
		if(bean != null && (includeNonSingletons || bean.singleton)) return new String[]{bean.name};
		else return new String[0];
	}

	@Override
	public <T> Map<String, T> getBeansOfType(Class<T> type) throws BeansException {
		return getBeansOfType(type, true, true);
	}

	@Override
	public <T> Map<String, T> getBeansOfType(Class<T> type, boolean includeNonSingletons, boolean allowEagerInit) throws BeansException {
		HashMap<String, T> ret = new HashMap<String, T>();
		Bean<T> bean = types.get(type);
		if(bean != null && (includeNonSingletons || bean.singleton)) ret.put(bean.name, bean.getInstance());
		return ret;
	}

	@Override
	public String[] getBeanNamesForAnnotation(Class<? extends Annotation> annotationType) {
		HashSet<String> names = new HashSet<String>();
		for(Bean<?> bean : beans.values()) if(bean.clz.isAnnotationPresent(annotationType)) names.add(bean.name);
		return names.toArray(new String[names.size()]);
	}

	@Override
	public Map<String, Object> getBeansWithAnnotation(Class<? extends Annotation> annotationType) throws BeansException {
		HashMap<String, Object> ret = new HashMap<String, Object>();
		for(Bean<?> bean : beans.values()) if(bean.clz.isAnnotationPresent(annotationType)) ret.put(bean.name, bean.getInstance());
		return ret;
	}

	@Override
	public <A extends Annotation> A findAnnotationOnBean(String beanName, Class<A> annotationType) throws NoSuchBeanDefinitionException {
		Bean<?> bean = beans.get(beanName);
		if(bean != null && bean.clz.isAnnotationPresent(annotationType)) return bean.clz.getAnnotation(annotationType);
		else throw new NoSuchBeanDefinitionException(beanName);
	}
	
	public synchronized IReadonlyListable<Object, Class<?>> process(Collection<Class<?>> classes){
		HashMap<String, Class<?>> ret = new HashMap<String, Class<?>>();
		if(classes != null && classes.size() > 0){
			HashMap<String, Bean> beans = new HashMap<String, Bean>();
			HashMap<Class<?>, Bean> types = new HashMap<Class<?>, Bean>();
			types.putAll(this.types);
			beans.putAll(this.beans);
			if(!overlap) classes.removeAll(types.keySet());
			for(Class<?> c : classes) if(c.isAnnotationPresent(ServiceClass.class) && (c.getEnclosingClass() == null || Modifier.isStatic(c.getModifiers()))){
				ServiceClass sc = c.getAnnotation(ServiceClass.class);
				String name = sc.name();
				if(name == null || name.trim().length() == 0) name = c.getName();
				Boolean singleton = ServiceScope.SINGLETON.equals(sc.scope());
				Method init = null;
				Method destroy = null;
				Field context = null;
				for (Method me : c.getMethods()) try{
					if (me.isAnnotationPresent(PostConstruct.class)) init = me;
					else if(singleton && me.isAnnotationPresent(PreDestroy.class)) destroy = me;
				}catch(Exception e){
					log.error("AnnotationBeanFactory: performing failed.", e);
				}
				try{
					Field f = c.getDeclaredField(this.context);
					if(!Modifier.isStatic(f.getModifiers()) && IProcessor.class.equals(f.getType())){
						if(!f.isAccessible()) f.setAccessible(true);
						context = f;
					}
				}catch(Exception e){}
				Bean<?> bean = new Bean(c, name, singleton, init, destroy, context);
				beans.put(name, bean);
				types.put(c, bean);
				ret.put(name, c);
			}
			this.beans = beans;
			this.types = types;
		}
		return new PrivateListable(ret);
	}
	
	public class ReadonlyListable<T, R, M> implements IReadonlyListable<T, R>, IExtendable<M> {
		protected Integer mode = 0; 
		protected final IProcessor<T[], Object> count = new IProcessor<T[], Object>(){
			@Override
			public Object process(T[] paras) {
				return new Long(getBeanDefinitionCount());
			}
		};
		
		public void setMode(Integer mode) {
			if(mode != null) this.mode = mode;
		}

		@Override
		public R find(T... paras) {
			if(paras!= null && paras.length > 0 && paras[0] != null) switch(mode) {
				case 0: return (R)getBean(paras[0].toString());
				default: return (R)getType(paras[0].toString());
			}
			else return null;
		}

		@Override
		public Collection<T[]> keys(T... paras) {
			String[] names = getBeanDefinitionNames();
			Collection ret = new ArrayList<String[]>(names.length);
			for(String name : names) ret.add(new String[]{name});
			return (Collection<T[]>)ret;
		}
	
		@Override
		public Map<T[], R> all(T... paras) {
			String[] names = getBeanDefinitionNames();
			Map ret = new HashMap<String[], Object>(names.length);
			for(String name : names) ret.put(new String[]{name}, find((T)name));
			return ret;
		}
	

		@Override
		public <N> N extend(M object) {
			if (object != null) {
				Object method = object instanceof ResourceMethod ? object : methods.process(object);
				if (method!= null) switch((ResourceMethod) method) {
					case count : return (N) count;
				}
			}
			return null;
		}
		
		@Override
		public Collection<M> methods() {
			return (Collection<M>) methods;
		}
	}
	
	public class GetBean<R> implements IProcessor<String, R>{
		protected Integer mode = 0; 
		
		public void setMode(Integer mode) {
			if(mode != null) this.mode = mode;
		}

		@Override
		public R process(String beanname) {
			if(beanname != null) switch(mode) {
				case 0: return (R)getBean(beanname);
				default: return (R)getType(beanname);
			}else return null;
		}
	}
	
	public class Destroy implements IProcessor<Object, Object>{
		@Override
		public Object process(Object instance) {
			destroy();
			return instance;
		}
	}
}
