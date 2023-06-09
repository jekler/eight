package net.yeeyaa.eight.access.util;

import java.util.Collection;

import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.common.spring.PlatformBean;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.support.BeanDefinitionReader;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.web.context.ConfigurableWebApplicationContext;
import org.springframework.web.context.support.GenericWebApplicationContext;
import org.springframework.web.context.support.StaticWebApplicationContext;
import org.springframework.web.context.support.XmlWebApplicationContext;


public class WebContexLoader implements ApplicationContextAware, IProcessor<Collection<Resource>, Void> {
	protected ApplicationContext parent;
	protected ClassLoader classLoader;
	protected ResourceLoader resourceLoader;
	protected Integer priority = 50;	
	protected IProcessor<Collection<Resource>, Integer> proprityCreator;
	protected IProcessor<ConfigurableWebApplicationContext, BeanDefinitionReader> readerCreator;
	protected PlatformBean platformBean;
	protected Boolean needParent = true;
	protected IProcessor<Void, Void> postProcessor;
	protected Boolean type; 
	protected ServletConfig servletConfig;
	protected String namespace;
	protected String[] configLocations;
	protected ServletContext servletContext;
	protected ConfigurableEnvironment environment;
	protected Boolean allowBeanDefinitionOverriding;
	protected Boolean allowCircularReferences;
	
	public void setServletConfig(ServletConfig servletConfig) {
		this.servletConfig = servletConfig;
	}

	public void setNamespace(String namespace) {
		this.namespace = namespace;
	}

	public void setConfigLocations(String[] configLocations) {
		this.configLocations = configLocations;
	}

	public void setServletContext(ServletContext servletContext) {
		this.servletContext = servletContext;
	}

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

	public void setReaderCreator(IProcessor<ConfigurableWebApplicationContext, BeanDefinitionReader> readerCreator) {
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
			ConfigurableWebApplicationContext context;
			if (type == null) {
				GenericWebApplicationContext c = new GenericWebApplicationContext();
				if(classLoader != null) c.setClassLoader(classLoader);
				if(resourceLoader != null) c.setResourceLoader(resourceLoader);
				if(environment != null) c.setEnvironment(environment);
				if(allowBeanDefinitionOverriding != null) c.setAllowBeanDefinitionOverriding(allowBeanDefinitionOverriding);
				if(allowCircularReferences != null) c.setAllowCircularReferences(allowCircularReferences);
				context = c;
			} else if (type = true) {
				XmlWebApplicationContext c = new XmlWebApplicationContext();
				if(classLoader != null) c.setClassLoader(classLoader);
				if(environment != null) c.setEnvironment(environment);
				if(allowBeanDefinitionOverriding != null) c.setAllowBeanDefinitionOverriding(allowBeanDefinitionOverriding);
				if(allowCircularReferences != null) c.setAllowCircularReferences(allowCircularReferences);
				context = c;
			} else {
				StaticWebApplicationContext c = new StaticWebApplicationContext();
				if(classLoader != null) c.setClassLoader(classLoader);
				if(resourceLoader != null) c.setResourceLoader(resourceLoader);
				if(environment != null) c.setEnvironment(environment);
				if(allowBeanDefinitionOverriding != null) c.setAllowBeanDefinitionOverriding(allowBeanDefinitionOverriding);
				if(allowCircularReferences != null) c.setAllowCircularReferences(allowCircularReferences);
				context = c;
			}
			if(servletConfig != null) context.setServletConfig(servletConfig);
			if(namespace != null) context.setNamespace(namespace);
			if(servletContext != null) context.setServletContext(servletContext);
			if(configLocations != null && configLocations.length > 0) if (configLocations.length == 1) context.setConfigLocation(configLocations[0]);
			else context.setConfigLocations(configLocations);
			if(parent != null) context.setParent(parent);
			if(parent != null) context.setParent(parent);
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
