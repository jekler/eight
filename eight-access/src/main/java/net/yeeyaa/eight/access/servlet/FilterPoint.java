package net.yeeyaa.eight.access.servlet;

import java.io.IOException;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.Servlet;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class FilterPoint extends HttpServlet implements FilterChain, IProcessor<Object[], Object> {
	protected static final long serialVersionUID = 1177241586943828483L;
	protected final Logger log;
	protected IProcessor<Object[], Object> preProcessor;
	protected IProcessor<Object[], Object> postProcessor;	
	protected Object filter;
	protected Object next;
	
	public FilterPoint() {
		this.log = LoggerFactory.getLogger(FilterPoint.class);
	}

	public FilterPoint(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(FilterPoint.class) : log;
	}
	
	public void setPreProcessor(IProcessor<Object[], Object> preProcessor) {
		this.preProcessor = preProcessor;
	}

	public void setPostProcessor(IProcessor<Object[], Object> postProcessor) {
		this.postProcessor = postProcessor;
	}

	public void setFilter(Object filter) {
		this.filter = filter;
	}

	public void setNext(Object next) {
		this.next = next;
	}
	
	@Override
	public void doFilter(ServletRequest request, ServletResponse resp) throws IOException, ServletException {
		service(request, resp);
	}

	@Override
	public Object process(Object[] instance) {
		if (instance != null && instance.length > 1) try {
			HttpServletRequest request = (HttpServletRequest) instance[0];
			HttpServletResponse resp = (HttpServletResponse) instance[1];
			Object next = instance.length > 2 ? instance[2] : this.next;
			if(preProcessor != null) preProcessor.process(new Object[]{request, resp});
			if (filter instanceof Filter) if (next instanceof FilterChain) ((Filter) filter).doFilter(request, resp, (FilterChain) next);
			else {
				FilterPoint point = new FilterPoint();
				point.next = next;
				((Filter) filter).doFilter(request, resp, point);
			} else if (filter instanceof IProcessor) ((IProcessor<Object, Object>) filter).process(new Object[]{request, resp, next});
			else if (next instanceof FilterChain) ((FilterChain) next).doFilter(request, resp);
			else if (next instanceof Servlet) ((Servlet) next).service(request, resp);
			else if (next instanceof IProcessor) ((IProcessor<Object, Object>) filter).process(new Object[]{request, resp});
			if(postProcessor != null) postProcessor.process(new Object[]{request, resp});
		} catch (Exception e) {
			 log.error ("Failed to create HTTPS port", e);
			 throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
		}
		return instance;
	}
	
	@Override
	public void service(ServletRequest request, ServletResponse resp) throws ServletException, IOException {
		if(preProcessor != null) preProcessor.process(new Object[]{request, resp});
		if (filter instanceof Filter) if (next instanceof FilterChain) ((Filter) filter).doFilter(request, resp, (FilterChain) next);
		else {
			FilterPoint point = new FilterPoint();
			point.next = next;
			next = point;
			((Filter) filter).doFilter(request, resp, point);
		} else if (filter instanceof IProcessor) ((IProcessor<Object, Object>) filter).process(new Object[]{request, resp, next});
		else if (next instanceof FilterChain) ((FilterChain) next).doFilter(request, resp);
		else if (next instanceof Servlet) ((Servlet) next).service(request, resp);
		else if (next instanceof IProcessor) ((IProcessor<Object, Object>) filter).process(new Object[]{request, resp});
		if(postProcessor != null) postProcessor.process(new Object[]{request, resp});
	}

	@Override
	public void init(ServletConfig config) throws ServletException {
		super.init(config);
		if (next == null) {
			String key = config.getInitParameter("next");
			if(key != null) {
				Object o = config.getServletContext().getAttribute(key);
				if (o instanceof HttpServlet) next = (HttpServlet)o;
			}
		}
		if (filter == null) {
			String key = config.getInitParameter("filter");
			if(key != null) {
				Object o = config.getServletContext().getAttribute(key);
				if (o instanceof FilterChain) filter = (FilterChain)o;
			}
		}		
		if (preProcessor == null) {
			String key = config.getInitParameter("preProcessor");
			if(key != null) {
				Object o = config.getServletContext().getAttribute(key);
				if (o instanceof IProcessor) preProcessor = (IProcessor<Object[], Object>)o;
			}
		}
		if (postProcessor == null) {
			String key = config.getInitParameter("postProcessor");
			if(key != null) {
				Object o = config.getServletContext().getAttribute(key);
				if (o instanceof IProcessor) postProcessor = (IProcessor<Object[], Object>)o;
			}
		}
	}
}
