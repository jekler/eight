package net.yeeyaa.eight.access.util;

import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collections;
import java.util.Enumeration;
import java.util.EventListener;
import java.util.Map;
import java.util.Set;

import javax.servlet.Filter;
import javax.servlet.FilterRegistration;
import javax.servlet.FilterRegistration.Dynamic;
import javax.servlet.RequestDispatcher;
import javax.servlet.Servlet;
import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRegistration;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.SessionCookieConfig;
import javax.servlet.SessionTrackingMode;
import javax.servlet.descriptor.JspConfigDescriptor;


public class ProxyConfig implements ServletConfig, ServletContext {
	protected ServletConfig config;
	protected ClassLoader classloader;
	
	public class ProxyServlet implements Servlet {
		protected final Servlet servlet;
		
		public ProxyServlet(Servlet servlet) {
			this.servlet = servlet;
		}
	
		@Override
		public void destroy() {
			servlet.destroy();
		}
	
		@Override
		public ServletConfig getServletConfig() {
			return servlet.getServletConfig();
		}
	
		@Override
		public String getServletInfo() {
			return servlet.getServletInfo();
		}
	
		@Override
		public void init(ServletConfig config) throws ServletException {
			ProxyConfig.this.config = config;
			servlet.init(config);
		}
		
		@Override
		public void service(ServletRequest request, ServletResponse response) throws ServletException, IOException {
			servlet.service(request, response);
		}
	}

	public void setConfig(ServletConfig config) {
		this.config = config;
	}

	public void setClassloader(ClassLoader classloader) {
		this.classloader = classloader;
	}

	@Override
	public String getInitParameter(String key) {
		return config == null ? null : config.getInitParameter(key);
	}

	@Override
	public Enumeration<String> getInitParameterNames() {
		return config == null ? Collections.enumeration(Collections.EMPTY_SET) : config.getInitParameterNames();
	}

	@Override
	public ServletContext getServletContext() {
		return this;
	}

	@Override
	public String getServletName() {
		return config == null ? null : config.getServletName();
	}

	@Override
	public Dynamic addFilter(String name, String value) {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().addFilter(name, value);
	}

	@Override
	public Dynamic addFilter(String name, Filter filter) {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().addFilter(name, filter);
	}

	@Override
	public Dynamic addFilter(String name, Class<? extends Filter> filter) {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().addFilter(name, filter);
	}

	@Override
	public void addListener(String listener) {
		if (config != null && config.getServletContext() != null) config.getServletContext().addListener(listener);
	}

	@Override
	public <T extends EventListener> void addListener(T listener) {
		if (config != null && config.getServletContext() != null) config.getServletContext().addListener(listener);
		
	}

	@Override
	public void addListener(Class<? extends EventListener> listener) {
		if (config != null && config.getServletContext() != null) config.getServletContext().addListener(listener);
		
	}

	@Override
	public javax.servlet.ServletRegistration.Dynamic addServlet(String name, String servlet) {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().addServlet(name, servlet);
	}

	@Override
	public javax.servlet.ServletRegistration.Dynamic addServlet(String name, Servlet servlet) {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().addServlet(name, servlet);
	}

	@Override
	public javax.servlet.ServletRegistration.Dynamic addServlet(String name, Class<? extends Servlet> servlet) {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().addServlet(name, servlet);
	}

	@Override
	public <T extends Filter> T createFilter(Class<T> filter) throws ServletException {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().createFilter(filter);
	}

	@Override
	public <T extends EventListener> T createListener(Class<T> listener) throws ServletException {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().createListener(listener);
	}

	@Override
	public <T extends Servlet> T createServlet(Class<T> servlet) throws ServletException {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().createServlet(servlet);
	}

	@Override
	public void declareRoles(String... roles) {
		if (config != null && config.getServletContext() != null) config.getServletContext().declareRoles(roles);
	}

	@Override
	public Object getAttribute(String name) {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().getAttribute(name);
	}

	@Override
	public Enumeration<String> getAttributeNames() {
		return config == null || config.getServletContext() == null ? Collections.enumeration(Collections.EMPTY_SET) : config.getServletContext().getAttributeNames();
	}

	@Override
	public ClassLoader getClassLoader() {
		return classloader == null ? config == null || config.getServletContext() == null ? getClass().getClassLoader() : config.getServletContext().getClassLoader() : classloader;
	}

	@Override
	public ServletContext getContext(String context) {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().getContext(context);
	}

	@Override
	public String getContextPath() {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().getContextPath();
	}

	@Override
	public Set<SessionTrackingMode> getDefaultSessionTrackingModes() {
		return config == null || config.getServletContext() == null ? Collections.EMPTY_SET : config.getServletContext().getDefaultSessionTrackingModes();
	}

	@Override
	public int getEffectiveMajorVersion() {
		return config == null || config.getServletContext() == null ? 0 : config.getServletContext().getEffectiveMajorVersion();
	}

	@Override
	public int getEffectiveMinorVersion() {
		return config == null || config.getServletContext() == null ? 0 : config.getServletContext().getEffectiveMinorVersion();
	}

	@Override
	public Set<SessionTrackingMode> getEffectiveSessionTrackingModes() {
		return config == null || config.getServletContext() == null ? Collections.EMPTY_SET : config.getServletContext().getEffectiveSessionTrackingModes();
	}

	@Override
	public FilterRegistration getFilterRegistration(String name) {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().getFilterRegistration(name);
	}

	@Override
	public Map<String, ? extends FilterRegistration> getFilterRegistrations() {
		return config == null || config.getServletContext() == null ? Collections.EMPTY_MAP : config.getServletContext().getFilterRegistrations();
	}

	@Override
	public JspConfigDescriptor getJspConfigDescriptor() {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().getJspConfigDescriptor();
	}

	@Override
	public int getMajorVersion() {
		return config == null || config.getServletContext() == null ? 0 : config.getServletContext().getMajorVersion();
	}

	@Override
	public String getMimeType(String type) {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().getMimeType(type);
	}

	@Override
	public int getMinorVersion() {
		return config == null || config.getServletContext() == null ? 0 : config.getServletContext().getMinorVersion();
	}

	@Override
	public RequestDispatcher getNamedDispatcher(String dispatcher) {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().getNamedDispatcher(dispatcher);
	}

	@Override
	public String getRealPath(String path) {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().getRealPath(path);
	}

	@Override
	public RequestDispatcher getRequestDispatcher(String dispatcher) {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().getRequestDispatcher(dispatcher);
	}

	@Override
	public URL getResource(String resource) throws MalformedURLException {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().getResource(resource);
	}

	@Override
	public InputStream getResourceAsStream(String resource) {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().getResourceAsStream(resource);
	}

	@Override
	public Set<String> getResourcePaths(String path) {
		return config == null || config.getServletContext() == null ? Collections.EMPTY_SET : config.getServletContext().getResourcePaths(path);
	}

	@Override
	public String getServerInfo() {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().getServerInfo();
	}

	@Override
	public Servlet getServlet(String servlet) throws ServletException {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().getServlet(servlet);
	}

	@Override
	public String getServletContextName() {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().getServletContextName();
	}

	@Override
	public Enumeration<String> getServletNames() {
		return config == null || config.getServletContext() == null ? Collections.enumeration(Collections.EMPTY_SET) : config.getServletContext().getServletNames();
	}

	@Override
	public ServletRegistration getServletRegistration(String servlet) {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().getServletRegistration(servlet);
	}

	@Override
	public Map<String, ? extends ServletRegistration> getServletRegistrations() {
		return config == null || config.getServletContext() == null ? Collections.EMPTY_MAP : config.getServletContext().getServletRegistrations();
	}

	@Override
	public Enumeration<Servlet> getServlets() {
		return config == null || config.getServletContext() == null ? Collections.enumeration(Collections.EMPTY_SET) : config.getServletContext().getServlets();
	}

	@Override
	public SessionCookieConfig getSessionCookieConfig() {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().getSessionCookieConfig();
	}

	@Override
	public String getVirtualServerName() {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().getVirtualServerName();
	}

	@Override
	public void log(String log) {
		if (config != null && config.getServletContext() != null) config.getServletContext().log(log);
	}

	@Override
	public void log(Exception error, String log) {
		if (config != null && config.getServletContext() != null) config.getServletContext().log(error, log);
	}

	@Override
	public void log(String log, Throwable error) {
		if (config != null && config.getServletContext() != null) config.getServletContext().log(log, error);
	}

	@Override
	public void removeAttribute(String key) {
		if (config != null && config.getServletContext() != null) config.getServletContext().removeAttribute(key);
	}

	@Override
	public void setAttribute(String key, Object value) {
		if (config != null && config.getServletContext() != null) config.getServletContext().setAttribute(key, value);
	}

	@Override
	public boolean setInitParameter(String key, String value) {
		return config == null || config.getServletContext() == null ? null : config.getServletContext().setInitParameter(key, value);
	}

	@Override
	public void setSessionTrackingModes(Set<SessionTrackingMode> track) {
		if (config != null && config.getServletContext() != null) config.getServletContext().setSessionTrackingModes(track);
	}
}
