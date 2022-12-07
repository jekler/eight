package net.yeeyaa.eight.access.servlet;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.PlatformException.Type;
import net.yeeyaa.eight.core.storage.Storage.Method;
import net.yeeyaa.eight.core.util.TypeConvertor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ServletPoint extends HttpServlet {
	protected final Logger log;
	protected IProcessor<Object, Object> next;
	protected String contentType;
	protected String charset;
	protected Integer buffer = 8192;
	protected Long max = -1L;
	protected IProcessor<IBiProcessor<Object, Object, Object>, Object> stream;

	public ServletPoint() {
		this.log = LoggerFactory.getLogger(ServletPoint.class);
	}

	public ServletPoint(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ServletPoint.class) : log;
	}
	
	public void setStream(IProcessor<IBiProcessor<Object, Object, Object>, Object> stream) {
		this.stream = stream;
	}

	public void setMax(Long max) {
		if(max != null && max >= 0) this.max = max;
	}

	public void setBuffer(Integer buffer) {
		if(buffer != null && buffer > 0) this.buffer = buffer;
	}
	
	public void setCharset(String charset) {
		if(charset != null && charset.trim().length() > 0) this.charset = charset.trim();
	}
	
	public void setNext(IProcessor<Object, Object> next) {
		this.next = next;
	}

	public void setContentType(String contentType) {
		if(contentType != null && contentType.trim().length() > 0) this.contentType = contentType.trim();
	}
	
	protected class Wrapper {
		protected IProcessor<OutputStream, Object> processor;

		public Wrapper(IProcessor<OutputStream, Object> processor) {
			this.processor = processor;
		}
	}
	
	@Override
	public void service(ServletRequest request, ServletResponse resp) throws ServletException, IOException {
		try {		
			Object ret = null;
			final InputStream input = request.getInputStream();
			if (next == null || (stream != null && request instanceof HttpServletRequest && "PUT".equals(((HttpServletRequest)request).getMethod()))) try {
				final HashMap<String, LinkedList<String>> map = new HashMap<String, LinkedList<String>>();
				Enumeration<String> names = ((HttpServletRequest)request).getHeaderNames();
				while (names.hasMoreElements()) {
					String key = names.nextElement();
					LinkedList<String> ls = new LinkedList<String>();
					Enumeration<String> values = ((HttpServletRequest)request).getHeaders(key);
					while (values.hasMoreElements()) ls.add(values.nextElement());
					map.put(key, ls);
				}
				for (Entry<String, String[]> entry : request.getParameterMap().entrySet()) {
					LinkedList<String> ls = map.get(entry.getKey());
					if (ls == null) {
						ls = new LinkedList<String>();
						map.put(entry.getKey(), ls);
					}
					for (String value : entry.getValue()) ls.add(value);
				}
		    	ret = stream.process(new IBiProcessor<Object, Object, Object>(){
					@Override
					public Object perform(Object instance, Object para) {
						if (instance == null) return input;
						else if (instance instanceof IProcessor) return new Wrapper((IProcessor<OutputStream, Object>) instance);
						else if (para instanceof Integer) {
							Integer index = ((Integer) para) < 0 ? 0 : ((Integer) para);
							List<String> list = map.get(instance.toString());
							if (list == null || index >= list.size()) return null;
							else return list.get(index);
						} else return map.get(instance.toString());
					}
		    	});
			} catch (PlatformException e) {
				ret = e.getType();
			} else ret = next.process(TypeConvertor.inputToBytes(input, buffer, max));	
			if (ret instanceof Type) {
				((HttpServletResponse)resp).setHeader("Warning", ((Type)ret).getCode() + " " + ((Type)ret).getCate() + " " + ((Type)ret).getMessage());
				((HttpServletResponse)resp).setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
				resp.flushBuffer();
			} else {
				if(contentType != null) resp.setContentType(contentType);
				if(charset != null) resp.setCharacterEncoding(charset);
				resp.setBufferSize(buffer);
				OutputStream out = resp.getOutputStream();
				try{
					if (ret instanceof byte[]) out.write((byte[]) ret);
					else if (ret instanceof Wrapper) ((Wrapper)ret).processor.process(out);
					else {
						InputStream in = ret instanceof IExtendable ? ((IExtendable<Object>) ret).<InputStream>extend(Method.input) : ret instanceof InputStream ? (InputStream) ret : null;
						if (in == null) if (ret == null) out.write(new byte[0]);
						else out.write(charset == null ? ret.toString().getBytes() : ret.toString().getBytes(charset));
						else try {
							int nRead;
							byte[] data = new byte[buffer];
							while ((nRead = in.read(data, 0, data.length)) != -1) out.write(data, 0, nRead);
						} finally {
							in.close();
						}
					}
				} finally {
					out.close();
				}	
			}
		} catch (RuntimeException e) {
			log.error ("ServletPoint: exception occurs!", e);
			throw e;
		} catch (IOException e) {
			log.error ("ServletPoint: exception occurs!", e);
			throw e;
		}
	}

	@Override
	public void init(ServletConfig config) throws ServletException {
		super.init(config);
		if (next == null) {
			String contentType = config.getInitParameter("contentType");
			if(contentType != null && contentType.trim().length() > 0) this.contentType = contentType.trim();
			String charset = config.getInitParameter("charset");
			if(charset != null && charset.trim().length() > 0) this.charset = charset.trim();
			String key = config.getInitParameter("portal");
			if(key != null) {
				Object o = config.getServletContext().getAttribute(key);
				if (o instanceof IProcessor) next = (IProcessor<Object, Object>)o;
			}
			String stream = config.getInitParameter("stream");
			if(stream != null) {
				Object o = config.getServletContext().getAttribute(stream);
				if (o instanceof IProcessor) this.stream = (IProcessor<IBiProcessor<Object, Object, Object>, Object>)o;
			}
			String max = config.getInitParameter("max");
			if (max != null) this.max = new Long(max);
			String buf = config.getInitParameter("buffer");
			if (buf != null) buffer = new Integer(buf);
		}
	}
	
	public static class ServletRequestProcessor implements IProcessor<ServletRequest, Object> {
		protected final Logger log;
		public enum Method{attr, attrName, auth, encode, length, type, context, cookie, date, header, headerName, headers, input, secure,
			intHead, localAddr, locale, locales, localName, localPort, method, parameter, parameterMap, parameterName, value, path,
			pathTranslate, protocol, query, reader, remoteAddr, remoteHost, remotePort, remoteUser, requestDispatcher, requestedSessionId,
			requestURI, requestURL, schema, serverName, serverPort, servletPath, session, principal, fromCookie, fromURL, valid, userInRole};

		protected Method type = Method.attr;
		protected Object para;
		
		public ServletRequestProcessor() {
			this.log = LoggerFactory.getLogger(ServletRequestProcessor.class);
		}

		public ServletRequestProcessor(Logger log) {
			this.log = log == null ? LoggerFactory.getLogger(ServletRequestProcessor.class) : log;
		}
		
		public void setType(Method type) {
			if(type != null) this.type = type;
		}

		public void setPara(Object para) {
			this.para = para;
		}

		@Override
		public Object process(ServletRequest request) {
			if (request != null) try { switch(type) {
				case attr : return request.getAttribute(para == null ? "" : para.toString());
				case attrName : return request.getAttributeNames();
				case auth : return ((HttpServletRequest)request).getAuthType();
				case encode : return request.getCharacterEncoding();
				case length : return request.getContentLength();
				case type : return request.getContentType();
				case context : return ((HttpServletRequest)request).getContextPath();
				case cookie : return ((HttpServletRequest)request).getCookies();
				case date : return ((HttpServletRequest)request).getDateHeader(para == null ? "" : para.toString());
				case header : return ((HttpServletRequest)request).getHeader(para == null ? "" : para.toString());
				case headerName : return ((HttpServletRequest)request).getHeaderNames();
				case headers : return ((HttpServletRequest)request).getHeaders(para == null ? "" : para.toString());
				case input : return request.getInputStream();	
				case intHead : return ((HttpServletRequest)request).getIntHeader(para == null ? "" : para.toString());
				case localAddr : return request.getLocalAddr();
				case locale : return request.getLocale();
				case locales : return request.getLocales();
				case localName : return request.getLocalName();
				case localPort : return request.getLocalPort();
				case method : return ((HttpServletRequest)request).getMethod();
				case parameter : return request.getParameter(para == null ? "" : para.toString());
				case parameterMap : return request.getParameterMap();
				case parameterName : return request.getParameterNames();
				case value : return request.getParameterValues(para == null ? "" : para.toString());
				case path : return ((HttpServletRequest)request).getPathInfo();
				case pathTranslate : return ((HttpServletRequest)request).getPathTranslated();
				case protocol : return request.getProtocol();
				case query : return ((HttpServletRequest)request).getQueryString();
				case reader : return request.getReader();		
				case remoteAddr : return request.getRemoteAddr();
				case remoteHost : return request.getRemoteHost();
				case remotePort : return request.getRemotePort();
				case remoteUser : return ((HttpServletRequest)request).getRemoteUser();
				case requestDispatcher : return request.getRequestDispatcher(para == null ? "" : para.toString());
				case requestedSessionId : return ((HttpServletRequest)request).getRequestedSessionId();
				case requestURI : return ((HttpServletRequest)request).getRequestURI();
				case requestURL : return ((HttpServletRequest)request).getRequestURL();
				case schema : return request.getScheme();
				case serverName : return request.getServerName();
				case serverPort : return request.getServerPort();
				case servletPath : return ((HttpServletRequest)request).getServletPath();
				case session : return ((HttpServletRequest)request).getSession(Boolean.TRUE.equals(para));
				case principal : return ((HttpServletRequest)request).getUserPrincipal();	
				case fromCookie : return ((HttpServletRequest)request).isRequestedSessionIdFromCookie();
				case fromURL : return ((HttpServletRequest)request).isRequestedSessionIdFromURL();
				case valid : return ((HttpServletRequest)request).isRequestedSessionIdValid();
				case userInRole : return ((HttpServletRequest)request).isUserInRole(para == null ? "" : para.toString());
				case secure : return request.isSecure();
			}} catch (Exception e) {
				log.error("ServletRequestProcessor : perform error。", e);
			}
			return null;
		}
	}
}