package net.yeeyaa.eight.annotation.processor;

import java.util.Collection;
import java.util.HashSet;

import javax.annotation.PostConstruct;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;
import net.yeeyaa.eight.common.spring.PlatformBean;

import org.springframework.beans.factory.ListableBeanFactory;


public class RegBeanProcessor implements IProcessor<Object, Void> {
	protected PlatformBean platformBean;
	protected ListableBeanFactory factory;
	protected Integer priority = 50;
	
	public void setFactory(ListableBeanFactory factory) {
		this.factory = factory;
	}

	public void setPriority(Integer priority) {
		if(priority != null && priority > 0)this.priority = priority;
	}

	public void setPlatformBean(PlatformBean platformBean) {
		this.platformBean = platformBean;
	}

	@PostConstruct
	public void init(){
		platformBean.register(factory, priority);
	}
	
	@Override
	public Void process(Object instance) {
		HashSet<String> beans = new HashSet<String>();
		if(instance instanceof IReadonlyListable) {
			for(Object[] bean : ((IReadonlyListable<Object, Object>)instance).keys()) if(bean != null && bean.length > 0 && bean[0] != null) beans.add(bean[0].toString());
		} else if(instance instanceof Collection) for(Object bean : (Collection<Object>)instance) if(bean != null) beans.add(bean.toString());
		if(beans.size() > 0) platformBean.register(factory, beans.toArray(new String[beans.size()]));
		return null;
	}
}
