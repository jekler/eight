package net.yeeyaa.eight.osgi.runtime;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Hashtable;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceRegistration;
import org.osgi.service.url.AbstractURLStreamHandlerService;
import org.osgi.service.url.URLConstants;
import org.osgi.service.url.URLStreamHandlerService;


public class BundleProtocol extends AbstractURLStreamHandlerService implements IProcessor<URL, URLConnection>{
	public static final String PROTOCOL = "bundle";
	protected String region;
	protected Boolean special;
	protected IProcessor<Object, Object> beanHolder;
	protected BundleContext context;
	protected ServiceRegistration<URLStreamHandlerService>  registration;

	public void setRegion(String region) {
		this.region = region;
	}

	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}

	public void setContext(BundleContext context) {
		this.context = context;
	}

	public void setSpecial(Boolean special) {
		this.special = special;
	}

	@PostConstruct
	public void initialize(){
		if (context != null) try{
			Hashtable<String, Object> properties = new Hashtable<String, Object>(1);
			StringBuilder protocol = new StringBuilder(PROTOCOL);
			if (!Boolean.TRUE.equals(special)) protocol.append(context.getBundle().getBundleId());
			if (region != null) protocol.append(region);
			properties.put(URLConstants.URL_HANDLER_PROTOCOL, new String[]{protocol.toString()});
			registration = context.registerService(URLStreamHandlerService.class, this, properties);
		} catch(Exception e) {
			throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
		}
	}

	@PreDestroy
	public void destroy(){
		if (registration != null) {
			registration.unregister();
			registration = null;
		}
	}

	@Override
	public URLConnection openConnection(URL url) throws IOException {
		URLConnection conn = process(url);
		if (conn == null) throw new IOException("BundleProtocol: no such resource." + url);
		else return conn;
	}

	@Override
	public URLConnection process(URL url) {
		if (url != null) try { 
			if (beanHolder == null) {
				Bundle bundle = url.getPort() == -1 ? context.getBundle() : context.getBundle(new Long(url.getPort()));
				if (bundle == null) bundle = context.getBundle(); 
				if (url.getUserInfo() == null) {
					URL resource = bundle.getEntry(url.getPath());
					if (resource != null) return resource.openConnection();
				} else return new URL(bundle.getLocation()).openConnection();
			} else {
				Object o = beanHolder.process(url.getHost());
				if (o instanceof URLStreamHandlerService) return ((URLStreamHandlerService) o).openConnection(url);
				else if (o instanceof IProcessor) {
					Object ret = ((IProcessor<URL, Object>) o).process(url);
					if (ret instanceof URLConnection) return (URLConnection) ret;
				}
			}
		} catch(Exception e) {
			throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
		} 
		return null;
	}
}
