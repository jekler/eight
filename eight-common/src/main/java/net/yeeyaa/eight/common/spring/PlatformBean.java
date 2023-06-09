package net.yeeyaa.eight.common.spring;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.ListableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.ApplicationListener;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.event.ApplicationContextEvent;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.ContextStartedEvent;

public class PlatformBean implements ApplicationContextAware, ApplicationListener<ApplicationContextEvent>{
	protected final Logger log;
	protected ApplicationContext context;
	protected volatile ConcurrentHashMap<String, TreeSet<BeanHolder>> beans = new ConcurrentHashMap<String, TreeSet<BeanHolder>>();
	protected volatile HashMap<ListableBeanFactory, BeanHolder> contains = new HashMap<ListableBeanFactory, BeanHolder>();
	protected volatile CountDownLatch startSignal = new CountDownLatch(1);
	protected Integer priority = 100;
	protected Boolean autostart;
	protected Object init;
	
	protected class BeanHolder implements Comparable<BeanHolder>{
		protected ListableBeanFactory holder;
		protected Integer priority;
		
		protected BeanHolder(ListableBeanFactory holder, Integer priority) {
			this.holder = holder;
			this.priority = priority;
		}

		protected Object getBean(String name){
			return holder.getBean(name);
		}
		
		protected Class<?> getType(String name){
			return holder.getType(name);
		}
		
		@Override
		public int compareTo(BeanHolder o) {
			return this.priority.compareTo(o.priority);
		}	
	}

	public PlatformBean() {
		this.log = LoggerFactory.getLogger(PlatformBean.class);
	}

	public PlatformBean(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(PlatformBean.class) : log;
	}
	
	public void setInit(Object init) {
		this.init = init;
	}

	public void setPriority(Integer priority) {
		if(priority != null && priority > 0) this.priority = priority;
	}

	public void setAutostart(Boolean autostart) {
		this.autostart = autostart;
	}

	public synchronized void release(Object reload){
		CountDownLatch signal = startSignal;
		if(signal.getCount() == 0) startSignal = new CountDownLatch(1);
		try{
			beans = new ConcurrentHashMap<String, TreeSet<BeanHolder>>();
			contains = new HashMap<ListableBeanFactory, BeanHolder>();
			if(!Boolean.FALSE.equals(reload)) register(context, priority);
		}finally{
			if(signal.getCount() == 0) startSignal.countDown();
		}
	}
	
	public synchronized void close(Object suicide){
		for(ListableBeanFactory factory : contains.keySet()) if(factory instanceof ConfigurableApplicationContext && (factory != context || !Boolean.FALSE.equals(suicide))) try {
			((ConfigurableApplicationContext)factory).stop();
		} finally {
			((ConfigurableApplicationContext)factory).close();
		}
	}
	
	public synchronized void reinit(Object reload){
		for(ListableBeanFactory factory : contains.keySet()) if(factory instanceof ConfigurableApplicationContext && (factory != context || Boolean.FALSE.equals(reload))) try { 
			((ConfigurableApplicationContext)factory).stop();
		} finally {
			((ConfigurableApplicationContext)factory).close();
		}
		CountDownLatch signal = startSignal;
		if(signal.getCount() == 0) startSignal = new CountDownLatch(1);
		try{
			beans = new ConcurrentHashMap<String, TreeSet<BeanHolder>>();
			contains = new HashMap<ListableBeanFactory, BeanHolder>();
			if(!Boolean.FALSE.equals(reload)) register(context, priority);
		}finally{
			if(signal.getCount() == 0) startSignal.countDown();
		}
	}
	
	public synchronized void register(ListableBeanFactory factory, Integer priority){
		if(factory != null && priority != null && priority > 0 && !contains.containsKey(factory)) {
			BeanHolder holder = new BeanHolder(factory, priority);
			contains.put(factory, holder);
			for(String name : factory.getBeanDefinitionNames()){
				TreeSet<BeanHolder> set = beans.get(name);
				if(set == null) {
					set = new TreeSet<BeanHolder>();
					beans.put(name, set);
				}
				set.add(holder);
			}
		}
	}
	
	public synchronized void register(ListableBeanFactory factory, Integer priority, String... names){
		if(!contains.containsKey(factory)) register(factory, priority);
		register(factory, names);
	}

	public synchronized void register(ListableBeanFactory factory, String... names){
		if(names != null && names.length > 0 && contains.containsKey(factory)){
			BeanHolder holder = contains.get(factory);
			for(String name : names){
				TreeSet<BeanHolder> set = beans.get(name);
				if(set == null) {
					set = new TreeSet<BeanHolder>();
					beans.put(name, set);
				}
				set.add(holder);
			}
		}
	}
	
	public Object getBean(Object beanname){
		if(beanname != null) try{
			startSignal.await();
			TreeSet<BeanHolder> set = beans.get(beanname.toString());
			if(set != null && set.size() > 0){
				return set.first().getBean(beanname.toString());
			}
		}catch(Exception e){
            log.error("PlatformBean: get bean failed: "+beanname.toString(), e);		
		}
		return null;
	}
	
	public Object[] getVerBean(Object beanname){
		if(beanname != null) try{
			startSignal.await();
			TreeSet<BeanHolder> set = beans.get(beanname.toString());
			if(set != null && set.size() > 0){
				BeanHolder holder = set.first();
				Object[] ret = new Object[2];
				ret[0] = holder.priority;
				ret[1] = holder.getBean(beanname.toString());
				if(ret[1] != null) return ret;
				else return null;
			}
		}catch(Exception e){
            log.error("PlatformBean: get version and bean failed: "+beanname.toString(), e);		
		}
		return null;
	}
	
	public Class<?> getType(Object beanname){
		if(beanname != null) try{
			startSignal.await();
			TreeSet<BeanHolder> set = beans.get(beanname.toString());
			if(set != null && set.size() > 0){
				return set.first().getType(beanname.toString());
			}
		}catch(Exception e){
            log.error("PlatformBean: get type failed: "+beanname.toString(), e);		
		}
		return null;
	}
	
	public Object[] getVerType(Object beanname){
		if(beanname != null) try{
			startSignal.await();
			TreeSet<BeanHolder> set = beans.get(beanname.toString());
			if(set != null && set.size() > 0){
				BeanHolder holder = set.first();
				Object[] ret = new Object[2];
				ret[0] = holder.priority;
				ret[1] = holder.getType(beanname.toString());
				if(ret[1] != null) return ret;
				else return null;
			}
		}catch(Exception e){
            log.error("PlatformBean: get version and type failed: "+beanname.toString(), e);		
		}
		return null;
	}
	
	public Integer getVer(Object beanname){
		if(beanname != null) try{
			startSignal.await();
			TreeSet<BeanHolder> set = beans.get(beanname.toString());
			if(set != null && set.size() > 0){
				return set.first().priority;
			}
		}catch(Exception e){
            log.error("PlatformBean: get version failed: "+beanname.toString(), e);		
		}
		return null;
	}

	public void begin(Object init){
		startSignal.countDown();
		if(init != null) {
			Object o = getBean(init);
			if(o instanceof IProcessor) ((IProcessor)o).process(null);
		}
	}

	public synchronized void suspend(){
		if(startSignal.getCount() == 0)	startSignal = new CountDownLatch(1);
	}
	
	public String[] getBeanDefinitionNames(){
		return beans.keySet().toArray(new String[beans.keySet().size()]);
	}
	
	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		context = applicationContext;
		register(applicationContext, priority);		
	}

	@Override
	public void onApplicationEvent(ApplicationContextEvent event) {
		if ((autostart== null && event instanceof ContextStartedEvent) || (Boolean.TRUE.equals(autostart) && event instanceof ContextRefreshedEvent)) begin(init);
	}

	public int getBeanDefinitionCount() {
		return beans.size();
	}
	
	public class Begin implements IProcessor<Object, Object>{
		protected volatile Object init;
		protected Boolean repeat = false;
		
		public void setRepeat(Boolean repeat) {
			if(repeat != null) this.repeat = repeat;
		}

		public void setInit(Object init) {
			this.init = init;
		}

		@Override
		public synchronized Object process(Object instance) {
			if(init != null || repeat) begin(init);
			if(!repeat) init = null;
			return instance;
		}		
	}

	public class Close implements IProcessor<Object, Object>{
		protected Object suicide = true;
		protected Boolean log;
		
		public void setSuicide(Boolean suicide) {
			if(suicide != null) this.suicide = suicide;
		}

		public void setLog(Boolean log) {
			this.log = log;
		}

		@Override
		public Object process(Object suicide) {
			if(suicide == null) suicide = this.suicide;
			try {
				close(suicide);
			} catch (Exception e) {
				if (Boolean.TRUE.equals(log)) PlatformBean.this.log.error("PlatformBean: Close exception.", e);
			}
			return suicide;
		}	
	}
	
	public class ReadonlyListable<T, R> implements IReadonlyListable<T, R>{
		protected Integer mode = 0; 
		
		public void setMode(Integer mode) {
			if(mode != null) this.mode = mode;
		}

		@Override
		public R find(T... paras) {
			if(paras!= null && paras.length > 0) switch(mode) {
				case 0: return (R)getBean(paras[0]);
				case 1: return (R)getType(paras[0]);
				case 2: return (R)getVer(paras[0]);
				case 3: return (R)getVerType(paras[0]);
				default: return (R)getVerBean(paras[0]);
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
	}
	
	public class GetBean<T, R> implements IProcessor<T, R>{
		protected Integer mode = 0; 
		
		public void setMode(Integer mode) {
			if(mode != null) this.mode = mode;
		}
		
		@Override
		public R process(T beanname) {
			switch(mode) {
				case 0: return (R)getBean(beanname);
				case 1: return (R)getType(beanname);
				case 2: return (R)getVer(beanname);
				case 3: return (R)getVerType(beanname);
				default: return (R)getVerBean(beanname);
			}
		}
	}
}
