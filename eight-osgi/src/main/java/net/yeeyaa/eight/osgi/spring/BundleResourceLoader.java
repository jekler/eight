package net.yeeyaa.eight.osgi.spring;

import net.yeeyaa.eight.IProcessor;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.springframework.core.io.DefaultResourceLoader;
import org.springframework.core.io.Resource;


public class BundleResourceLoader extends DefaultResourceLoader {
	protected volatile Bundle bundle;
	protected IProcessor<Resource, Resource> resourceProcessor;
	
	public BundleResourceLoader() {}

	public BundleResourceLoader(Bundle bundle) {
		this.bundle = bundle;
	}

	public BundleResourceLoader(ClassLoader classLoader) {
		super(classLoader);
	}
	
	public BundleResourceLoader(Bundle bundle, ClassLoader classLoader) {
		super(classLoader);
		this.bundle = bundle;
	}
	
	public void setBundle(Bundle bundle) {
		this.bundle = bundle;
	}

	public void setContext(BundleContext context) {
		if (context != null) this.bundle = context.getBundle();
	}

	public void setResourceProcessor(IProcessor<Resource, Resource> resourceProcessor) {
		this.resourceProcessor = resourceProcessor;
	}

	public Resource getResource(String location) {
		return resourceProcessor == null ? new BundleResource(bundle, location) : resourceProcessor.process(new BundleResource(bundle, location));
	}
}
