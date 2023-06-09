package net.yeeyaa.eight.osgi.runtime;

import java.util.Collection;
import java.util.Dictionary;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.servlet.Servlet;

import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.framework.ServiceReference;
import org.osgi.service.http.HttpContext;
import org.osgi.service.http.HttpService;
import org.osgi.util.tracker.ServiceTracker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class BundleHttpService {
	protected final Logger log;
	protected ServiceTracker<HttpService, HttpService> tracker;
	protected BundleContext context;
	protected Collection<ServiceInfo> services;
	protected String filter = "(" + Constants.OBJECTCLASS + "=" + HttpService.class.getName() + ")";
	
	public static class ServiceInfo {
		protected String alias;
		protected String name;
		protected Servlet servlet;
		protected Dictionary<String, String> initparams;
		protected HttpContext context;
		
		public void setAlias(String alias) {
			this.alias = alias;
		}
		
		public void setServlet(Servlet servlet) {
			this.servlet = servlet;
		}
		
		public void setInitparams(Dictionary<String, String> initparams) {
			this.initparams = initparams;
		}
		
		public void setContext(HttpContext context) {
			this.context = context;
		}

		public void setName(String name) {
			this.name = name;
		}
	}
	
	public BundleHttpService() {
		log  = LoggerFactory.getLogger(BundleHttpService.class);
	}

	public BundleHttpService(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(BundleHttpService.class) : log;
	}
	
	public void setFilter(String filter) {
		if (filter != null) if (filter.indexOf('(') == 0) this.filter = filter;
		else {
			StringBuilder sb = new StringBuilder("(").append(Constants.OBJECTCLASS).append("=").append(HttpService.class.getName()).append(")");
			String[] fs = filter.split("\\|");
			if (fs.length > 0) {
				sb.insert(0, "(&").append("(|");
				for (String f : fs) if ("null".equals(f)) sb.append("(!(").append(Constants.SERVICE_PID).append("=*))");
				else if (f.indexOf('!') == 0) sb.append("(!(").append(Constants.SERVICE_PID).append("=").append(f.substring(1)).append("))");
				else sb.append("(").append(Constants.SERVICE_PID).append("=").append(f).append(")");
				sb.append("))");
			}
			this.filter = sb.toString();
		}
	}

	public void setServices(Collection<ServiceInfo> services) {
		this.services = services;
	}

	public void setContext(BundleContext context) {
		this.context = context;
	}
	
	@PostConstruct
	public void initialize(){
		try{
			tracker = new ServiceTracker<HttpService, HttpService>(context, context.createFilter(filter), null) {
				public void removedService(ServiceReference<HttpService> reference, HttpService httpService) {
					if (services != null && services.size() > 0) for (ServiceInfo service : services) try {
						httpService.unregister(service.alias);
					} catch (IllegalArgumentException e) {}
					catch (Exception e) {
						log.error("BundleHttpService: initialize fail.", e);
					}
				}

				public HttpService addingService(ServiceReference<HttpService> reference) {
					try {
						HttpService httpService = context.getService(reference);
						if (services != null && services.size() > 0) for (ServiceInfo service : services) try {
							if (service.servlet != null && service.alias != null) httpService.registerServlet(service.alias, service.servlet, service.initparams, service.context);
							else if (service.name != null && service.alias != null) httpService.registerResources(service.alias, service.name, service.context);
						} catch (Exception e) {
							log.error("BundleHttpService: initialize fail.", e);
						}
						return httpService;
					} catch (Throwable ex) {
						log.error("BundleHttpService: initialize fail.", ex);
						return null;
					}
				}
			};
			tracker.open();
		}catch(Exception e){
			log.error("BundleHttpService: initialize fail.", e);
		}
	}

	@PreDestroy
	public void destroy(){
		if(tracker != null) try{
			tracker.close();
			tracker = null;
		}catch(Exception e){
			log.error("BundleHttpService: destory fail.", e);
		}
	}
}
