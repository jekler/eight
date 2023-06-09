package net.yeeyaa.eight.access.servlet;

import java.io.Serializable;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.common.spring.RestartProcessor;

import org.springframework.context.ApplicationContext;
import org.springframework.context.Lifecycle;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.context.support.WebApplicationContextUtils;


public class SysInitContextListener implements ServletContextListener, Serializable {
	protected static final long serialVersionUID = -324587565066771377L;
	protected WebApplicationContext context;
	
	@Override
	public void contextDestroyed(ServletContextEvent sce) {
		String close = sce.getServletContext().getInitParameter("closeafter");
		if(close != null) {
			Object c = context.getBean(close);
			if(c instanceof IProcessor) ((IProcessor<Object, Object>)c).process(null);
		}
	}
	
	@Override
	public void contextInitialized(final ServletContextEvent sce) {
		context = WebApplicationContextUtils.getRequiredWebApplicationContext(sce.getServletContext());
		if (context instanceof Lifecycle) ((Lifecycle)context).start();
		String begin = sce.getServletContext().getInitParameter("begin");
		String c = sce.getServletContext().getInitParameter("context");
		if(c != null) {
			Object b = context.getBean(c);
			if(b instanceof RestartProcessor) {
				RestartProcessor restart = (RestartProcessor) b;
				restart.setCreator(new IProcessor<Void, ApplicationContext>(){
					@Override
					public ApplicationContext process(Void instance) {
						return WebApplicationContextUtils.getRequiredWebApplicationContext(sce.getServletContext());
					}
				});
				restart.setBegin(begin);
				restart.setClose(sce.getServletContext().getInitParameter("close"));
				restart.setContext(context);
				restart.setSelf(c);
			}
		}
		if(begin != null) {
			Object b = context.getBean(begin);
			if(b instanceof IProcessor) ((IProcessor<Object, Object>)b).process(null);
		}
	}
}
