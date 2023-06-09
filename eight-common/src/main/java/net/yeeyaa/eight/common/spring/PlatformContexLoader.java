package net.yeeyaa.eight.common.spring;

import java.util.Collection;

import net.yeeyaa.eight.IProcessor;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.support.BeanDefinitionReader;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.context.support.GenericGroovyApplicationContext;
import org.springframework.context.support.GenericXmlApplicationContext;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;


public class PlatformContexLoader implements ApplicationContextAware, IProcessor<Collection<Resource>, Void> {
	protected ApplicationContext parent;
	protected ClassLoader classLoader;
	protected ResourceLoader resourceLoader;
	protected Integer priority = 50;	
	protected IProcessor<Collection<Resource>, Integer> proprityCreator;
	protected IProcessor<ApplicationContext, BeanDefinitionReader> readerCreator;
	protected PlatformBean platformBean;
	protected Boolean needParent = true;
	protected IProcessor<Void, Void> postProcessor;
	protected Boolean type; 
	
	public void setType(Boolean type) {
		this.type = type;
	}

	public void setPostProcessor(IProcessor<Void, Void> postProcessor) {
		this.postProcessor = postProcessor;
	}

	public void setNeedParent(Boolean needParent) {
		if(needParent != null) this.needParent = needParent;
	}

	public void setPlatformBean(PlatformBean platformBean) {
		this.platformBean = platformBean;
	}

	public void setProprityCreator(IProcessor<Collection<Resource>, Integer> proprityCreator) {
		this.proprityCreator = proprityCreator;
	}

	public void setReaderCreator(IProcessor<ApplicationContext, BeanDefinitionReader> readerCreator) {
		this.readerCreator = readerCreator;
	}

	public void setResourceLoader(ResourceLoader resourceLoader) {
		this.resourceLoader = resourceLoader;
	}

	public void setPriority(Integer priority) {
		if(priority != null && priority > 0) this.priority = priority;
	}

	public void setParent(ApplicationContext parent) {
		this.parent = parent;
	}

	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		if(parent == null && needParent) parent = applicationContext;
	}
	
	public void setClassLoader(ClassLoader classLoader) {
		this.classLoader = classLoader;
	}
	
	@Override
	public Void process(Collection<Resource> resources) {
		if(resources != null && resources.size() > 0) {
			GenericApplicationContext context;
			if (type == null) context = parent == null ? new GenericApplicationContext() : new GenericApplicationContext(parent);
			else if (type = true) context = new GenericXmlApplicationContext();
			else context = new GenericGroovyApplicationContext();
			if(classLoader != null) context.setClassLoader(classLoader);
			if(resourceLoader != null) context.setResourceLoader(resourceLoader);
			Integer priority = this.priority;
			if(proprityCreator != null) priority = proprityCreator.process(resources);
			BeanDefinitionReader reader = readerCreator.process(context);
			reader.loadBeanDefinitions(resources.toArray(new Resource[resources.size()]));
			context.refresh();
			context.start();
			platformBean.register(context, priority);
			if(postProcessor != null) postProcessor.process(null);
		}
		return null;
	}
}
