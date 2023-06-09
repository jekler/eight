package net.yeeyaa.eight.access.websocket;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.HttpRequestHandler;


public class WebSocketServlet extends HttpServlet {
	protected final Logger log;
	protected HttpRequestHandler handler;	
	protected HttpRequestHandler defaultHandler;	
	protected Map<Pattern, HttpRequestHandler> matchHandlers;
	protected IProcessor<HttpServletRequest, HttpServletRequest> request;
	protected IProcessor<HttpServletResponse, HttpServletResponse> response;

	public WebSocketServlet() {
		this.log = LoggerFactory.getLogger(WebSocketServlet.class);
	}

	public WebSocketServlet(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(WebSocketServlet.class) : log;
	}
	
	public void setRequest(IProcessor<HttpServletRequest, HttpServletRequest> request) {
		this.request = request;
	}

	public void setResponse(IProcessor<HttpServletResponse, HttpServletResponse> response) {
		this.response = response;
	}

	public void setHandler(HttpRequestHandler handler) {
		this.handler = handler;
	}

	public void setDefaultHandler(HttpRequestHandler deafultHandler) {
		this.defaultHandler = deafultHandler;
	}

	public void setMatchHandlers(Map<String, HttpRequestHandler> matchHandlers) {
		if (matchHandlers != null && matchHandlers.size() > 0) {
			this.matchHandlers = new LinkedHashMap<Pattern, HttpRequestHandler> (matchHandlers.size() * 2);
			for (Entry<String, HttpRequestHandler> entry : matchHandlers.entrySet()) if (entry.getKey() != null && entry.getValue() != null) try {
				this.matchHandlers.put(Pattern.compile(entry.getKey()), entry.getValue());
			} catch (Exception e) {
				log.error("WebSocketDispatcher : create match pattern error。", e);
			}
		}
	}

	@Override
	public void service(HttpServletRequest request, HttpServletResponse resp) throws ServletException, IOException {
		if (this.request != null) request = this.request.process(request);
		if (response != null) resp = response.process(resp);
		String path = request.getPathInfo();
		if (path == null && handler != null) handler.handleRequest(request, resp);
		else {
			if (path != null && matchHandlers != null && matchHandlers.size() > 0) for (Entry<Pattern, HttpRequestHandler> entry : matchHandlers.entrySet()) if (entry.getKey().matcher(path).matches()) {
				entry.getValue().handleRequest(request, resp);
				return;
			}
			if (defaultHandler != null) defaultHandler.handleRequest(request, resp);
		}
	}

	@Override
	public void init(ServletConfig config) throws ServletException {
		super.init(config);
		if (handler == null) {
			String key = config.getInitParameter("handler");
			if(key != null) handler = (HttpRequestHandler)config.getServletContext().getAttribute(key);
		}
		if (defaultHandler == null) {
			String key = config.getInitParameter("deafultHandler");
			if(key != null) defaultHandler = (HttpRequestHandler)config.getServletContext().getAttribute(key);
		}
		if (request == null) {
			String key = config.getInitParameter("request");
			if(key != null) request = (IProcessor<HttpServletRequest, HttpServletRequest>)config.getServletContext().getAttribute(key);
		}
		if (response == null) {
			String key = config.getInitParameter("response");
			if(key != null) response = (IProcessor<HttpServletResponse, HttpServletResponse>)config.getServletContext().getAttribute(key);
		}
		if (defaultHandler == null) {
			String key = config.getInitParameter("deafultHandler");
			if(key != null) defaultHandler = (HttpRequestHandler)config.getServletContext().getAttribute(key);
		}		
		if (matchHandlers == null) {
			String key = config.getInitParameter("matchHandlers");
			if (key != null) {
				String[] o = key.split("::");
				if (o.length > 0) {
					matchHandlers = new LinkedHashMap<Pattern, HttpRequestHandler>(o.length * 2);
					for (int i = 0; i < o.length; i++) {
						String[] pair = o[i].split(":");
						if(pair.length > 1 && pair[0] != null && pair[1] != null) try {
							HttpRequestHandler handler = (HttpRequestHandler)config.getServletContext().getAttribute(pair[1]);
							if (handler != null) matchHandlers.put(Pattern.compile(pair[0].replace("`d", ":").replace("`e", "`")), handler);
						} catch (Exception e) {
							log.error("WebSocketDispatcher : create match pattern error。", e);
						}
					}
				}
			}
		}
	}
}
