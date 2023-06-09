package net.yeeyaa.eight.access.servlet;

import java.io.IOException;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.ITriProcessor;


public class HttpServletPoint extends HttpServlet implements IProcessor<String, Object> {
	protected String servletInfo = "";
	protected IProcessor<Object, Object> destroy;
	protected ITriProcessor<String, HttpServletRequest, HttpServletResponse, Object> deafultService;
	protected IBiProcessor<HttpServletRequest, HttpServletResponse, Object> service;
	protected IBiProcessor<HttpServletRequest, HttpServletResponse, Object> delete;
	protected IBiProcessor<HttpServletRequest, HttpServletResponse, Object> get;
	protected IBiProcessor<HttpServletRequest, HttpServletResponse, Object> head;
	protected IBiProcessor<HttpServletRequest, HttpServletResponse, Object> options;
	protected IBiProcessor<HttpServletRequest, HttpServletResponse, Object> post;
	protected IBiProcessor<HttpServletRequest, HttpServletResponse, Object> put;
	protected IBiProcessor<HttpServletRequest, HttpServletResponse, Object> trace;
	protected IProcessor<HttpServletRequest, Long> lastModified;
	
	public void setDeafultService(ITriProcessor<String, HttpServletRequest, HttpServletResponse, Object> deafultService) {
		this.deafultService = deafultService;
	}

	public void setService(IBiProcessor<HttpServletRequest, HttpServletResponse, Object> service) {
		this.service = service;
	}

	public void setServletInfo(String servletInfo) {
		this.servletInfo = servletInfo;
	}

	public void setDestroy(IProcessor<Object, Object> destroy) {
		this.destroy = destroy;
	}

	@Override
	protected void doDelete(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		if (delete != null) delete.perform(req, resp);
		else if (deafultService != null) deafultService.operate("delete", req, resp);
		else super.doDelete(req, resp);
	}

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		if (get != null) get.perform(req, resp);
		else if (deafultService != null) deafultService.operate("get", req, resp);
		else super.doGet(req, resp);
	}

	@Override
	protected void doHead(HttpServletRequest req, HttpServletResponse resp)	throws ServletException, IOException {
		if (head != null) head.perform(req, resp);
		else if (deafultService != null) deafultService.operate("head", req, resp);
		else super.doHead(req, resp);
	}

	@Override
	protected void doOptions(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		if (options != null) options.perform(req, resp);
		else if (deafultService != null) deafultService.operate("options", req, resp);
		else super.doOptions(req, resp);
	}

	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp)	throws ServletException, IOException {
		if (post != null) post.perform(req, resp);
		else if (deafultService != null) deafultService.operate("post", req, resp);
		else super.doPost(req, resp);
	}

	@Override
	protected void doPut(HttpServletRequest req, HttpServletResponse resp)	throws ServletException, IOException {
		if (put != null) put.perform(req, resp);
		else if (deafultService != null) deafultService.operate("put", req, resp);
		else super.doPut(req, resp);
	}

	@Override
	protected void doTrace(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		if (trace != null) trace.perform(req, resp);
		else if (deafultService != null) deafultService.operate("trace", req, resp);
		else super.doTrace(req, resp);
	}

	@Override
	protected long getLastModified(HttpServletRequest req) {
		if (lastModified == null) return super.getLastModified(req);
		else return lastModified.process(req);
	}

	@Override
	protected void service(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		if (service == null) super.service(req, resp);
		else service.perform(req, resp);
	}

	@Override
	public void init(ServletConfig config) throws ServletException {
		super.init(config);
		String servletInfo = config.getInitParameter("servletInfo");
		if(servletInfo != null && servletInfo.trim().length() > 0) this.servletInfo = servletInfo.trim();
		String destroy = config.getInitParameter("destroy");
		if(destroy != null) {
			Object o = config.getServletContext().getAttribute(destroy);
			if (o instanceof IProcessor) this.destroy = (IProcessor<Object, Object>)o;
		}
		String deafultService = config.getInitParameter("deafultService");
		if(deafultService != null) {
			Object o = config.getServletContext().getAttribute(deafultService);
			if (o instanceof ITriProcessor) this.deafultService = (ITriProcessor<String, HttpServletRequest, HttpServletResponse, Object>)o;
		}
		String lastModified = config.getInitParameter("lastModified");
		if(lastModified != null) {
			Object o = config.getServletContext().getAttribute(lastModified);
			if (o instanceof IProcessor) this.lastModified = (IProcessor<HttpServletRequest, Long>)o;
		}
		String service = config.getInitParameter("service");
		if(service != null) {
			Object o = config.getServletContext().getAttribute(service);
			if (o instanceof IBiProcessor) this.service = (IBiProcessor<HttpServletRequest, HttpServletResponse, Object>)o;
		}
		String delete = config.getInitParameter("delete");
		if(delete != null) {
			Object o = config.getServletContext().getAttribute(delete);
			if (o instanceof IBiProcessor) this.delete = (IBiProcessor<HttpServletRequest, HttpServletResponse, Object>)o;
		}
		String get = config.getInitParameter("get");
		if(get != null) {
			Object o = config.getServletContext().getAttribute(get);
			if (o instanceof IBiProcessor) this.get = (IBiProcessor<HttpServletRequest, HttpServletResponse, Object>)o;
		}
		String head = config.getInitParameter("head");
		if(head != null) {
			Object o = config.getServletContext().getAttribute(head);
			if (o instanceof IBiProcessor) this.head = (IBiProcessor<HttpServletRequest, HttpServletResponse, Object>)o;
		}
		String put = config.getInitParameter("put");
		if(put != null) {
			Object o = config.getServletContext().getAttribute(put);
			if (o instanceof IBiProcessor) this.put = (IBiProcessor<HttpServletRequest, HttpServletResponse, Object>)o;
		}
		String post = config.getInitParameter("post");
		if(post != null) {
			Object o = config.getServletContext().getAttribute(post);
			if (o instanceof IBiProcessor) this.post = (IBiProcessor<HttpServletRequest, HttpServletResponse, Object>)o;
		}
		String options = config.getInitParameter("options");
		if(options != null) {
			Object o = config.getServletContext().getAttribute(options);
			if (o instanceof IBiProcessor) this.options = (IBiProcessor<HttpServletRequest, HttpServletResponse, Object>)o;
		}
		String trace = config.getInitParameter("trace");
		if(trace != null) {
			Object o = config.getServletContext().getAttribute(trace);
			if (o instanceof IBiProcessor) this.trace = (IBiProcessor<HttpServletRequest, HttpServletResponse, Object>)o;
		}
	}
	
	@Override
	public void destroy() {
		if (destroy != null) destroy.process(this);
	}

	@Override
	public String getServletInfo() {
		return servletInfo;
	}

	@Override
	public Object process(String object) {
		if ("context".equals(object)) return getServletContext();
		else if ("config".equals(object)) return getServletConfig();
		else if ("parameter".equals(object)) return getInitParameterNames();
		else if ("servletInfo".equals(object)) return servletInfo;
		else if ("name".equals(object)) return getServletName();
		else if ("deafultService".equals(object)) return deafultService;
		else if ("service".equals(object)) return service;
		else if ("delete".equals(object)) return delete;
		else if ("get".equals(object)) return get;
		else if ("head".equals(object)) return head;
		else if ("options".equals(object)) return options;
		else if ("post".equals(object)) return post;
		else if ("put".equals(object)) return put;
		else if ("trace".equals(object)) return trace;
		else if ("lastModified".equals(object)) return lastModified;
		else if ("destroy".equals(object)) return destroy;
		else return getInitParameter(object);
	}
}
