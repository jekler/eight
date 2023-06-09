package net.yeeyaa.eight.access.http;

import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpContext;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URLDecoder;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.PlatformException.Type;
import net.yeeyaa.eight.core.storage.Storage.Method;
import net.yeeyaa.eight.core.util.TypeConvertor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class HttpPoint implements HttpHandler{
	protected static final Pattern url = Pattern.compile("(([^\\?&=]+)=([^&=]+))");
	protected final Logger log;
	protected String context = "/";
	protected IProcessor<String, HttpContext> contextHolder;
	protected String contentType = "text/plain; charset=UTF-8";
	protected IProcessor<Object, Object> next;
	protected IProcessor<HttpExchange, HttpExchange> preProcessor;
	protected IProcessor<InputStream, InputStream> gzipInput;
	protected IProcessor<OutputStream, OutputStream> gzipOutput;
	protected Boolean compress;
	protected String charset = Charset.defaultCharset().name();
	protected IProcessor<IBiProcessor<Object, Object, Object>, Object> stream;
	protected Integer readMode = 0;
	protected Integer buffer = 8192;
	protected Long max = -1L;

	public HttpPoint() {
		this.log = LoggerFactory.getLogger(HttpPoint.class);
	}

	public HttpPoint(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(HttpPoint.class) : log;
	}

	public void setCharset(String charset) {
		if(charset != null && charset.trim().length() > 0) this.charset = charset.trim();
	}
	
	public void setGzipInput(IProcessor<InputStream, InputStream> gzipInput) {
		this.gzipInput = gzipInput;
	}

	public void setGzipOutput(IProcessor<OutputStream, OutputStream> gzipOutput) {
		this.gzipOutput = gzipOutput;
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

	public void setContextHolder(IProcessor<String, HttpContext> contextHolder) {
		this.contextHolder = contextHolder;
	}

	public void setPreProcessor(IProcessor<HttpExchange, HttpExchange> preProcessor) {
		this.preProcessor = preProcessor;
	}
	
	public void setReadMode(Integer readMode) {
		if(readMode != null) this.readMode = readMode;
	}

	public void setNext(IProcessor<Object, Object> next) {
		this.next = next;
	}

	public void setCompress(Boolean compress) {
		this.compress = compress;
	}

	public void setContentType(String contentType) {
		if(contentType != null) this.contentType = contentType;
	}

	public void setContext(String context) {
		if(context != null) this.context = context;
	}

	@PostConstruct
	public void initialize(){ 
		contextHolder.process(context).setHandler(this);
	}
	
	protected class Wrapper {
		protected IProcessor<OutputStream, Object> processor;

		public Wrapper(IProcessor<OutputStream, Object> processor) {
			this.processor = processor;
		}
	}
	
	protected InputStream decompressStream(InputStream input) throws IOException{
		if (!input.markSupported()) input = new BufferedInputStream(input);
		input.mark(2);
	    int magic = input.read() & 0xff | ((input.read() << 8) & 0xff00);
	    input.reset();
	    if(magic == GZIPInputStream.GZIP_MAGIC) return gzipInput == null ? new GZIPInputStream(input) : gzipInput.process(input);
	    else return input;
	}
	
	protected Boolean isGzip(HttpExchange exchange){
		List<String> encodings = exchange.getRequestHeaders().get("Accept-Encoding");
		if(encodings != null && encodings.size() > 0) for(String encoding : encodings) if(encoding != null && encoding.indexOf("gzip") > -1) return true;
		return false;
	}
	
	@Override
	public void handle(HttpExchange exchange) throws IOException {
		try {
			if(preProcessor != null) exchange = preProcessor.process(exchange);
			final InputStream input;
		    switch(readMode){
		    	case 0: input = decompressStream(exchange.getRequestBody());
		    	break;
		    	case 1: input = gzipInput == null ? new GZIPInputStream(exchange.getRequestBody()) : gzipInput.process(exchange.getRequestBody());
		    	break;
		    	default: input = exchange.getRequestBody();
		    }
			Object ret = null;
		    if (next == null || (stream != null && "PUT".equals(exchange.getRequestMethod()))) try {
		    	final HashMap<String, List<String>> map = new HashMap<String, List<String>>(exchange.getRequestHeaders());
		    	Matcher m = url.matcher(URLDecoder.decode(exchange.getRequestURI().toString(), charset));
				while (m.find()) {
					String key = m.group(2);
					List<String> value = map.get(key);
					if (value == null) {
						value = new LinkedList<String>();
						map.put(key, value);
					}
					value.add(m.group(3));
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
		    Headers responseHeaders = exchange.getResponseHeaders();  
			if (ret instanceof Type) try {
				responseHeaders.set("Warning", ((Type)ret).getCode() + " " + ((Type)ret).getCate() + " " + ((Type)ret).getMessage());
				exchange.sendResponseHeaders(HttpURLConnection.HTTP_INTERNAL_ERROR, -1); 
			} finally {
				exchange.close();
			} else {
				OutputStream responseBody;
				if (ret instanceof byte[]) responseHeaders.set("Content-Type", contentType);
			    if(Boolean.TRUE.equals(compress) || (compress == null && isGzip(exchange))){
			    	responseHeaders.set("Content-Encoding", "gzip");
			    	exchange.sendResponseHeaders(HttpURLConnection.HTTP_OK, 0);  
			        responseBody = gzipOutput == null ? new GZIPOutputStream(exchange.getResponseBody()) : gzipOutput.process(exchange.getResponseBody());  
			    } else {
			    	if (ret instanceof byte[]) exchange.sendResponseHeaders(HttpURLConnection.HTTP_OK, ((byte[])ret).length); 
			    	else exchange.sendResponseHeaders(HttpURLConnection.HTTP_OK, 0); 
			        responseBody = exchange.getResponseBody();  
			    } 
				try{
					if (ret instanceof byte[]) responseBody.write((byte[]) ret);
					else if (ret instanceof Wrapper) ((Wrapper)ret).processor.process(responseBody);
					else {
						InputStream in = ret instanceof IExtendable ? ((IExtendable<Object>) ret).<InputStream>extend(Method.input) : ret instanceof InputStream ? (InputStream) ret : null;
						if (in == null) if (ret == null) responseBody.write(new byte[0]);
						else responseBody.write(charset == null ? ret.toString().getBytes() : ret.toString().getBytes(charset));
						else try {
							int nRead;
							byte[] data = new byte[buffer];
							while ((nRead = in.read(data, 0, data.length)) != -1) responseBody.write(data, 0, nRead);
						} finally {
							in.close();
						}
					}
				}finally{
					responseBody.close();
				}
			}
		} catch (RuntimeException e) {
			log.error ("HttpPoint: exception occurs!", e);
			throw e;
		} catch (IOException e) {
			log.error ("HttpPoint: exception occurs!", e);
			throw e;
		}
	}

	@PreDestroy
	public void destroy() {
		HttpContext c = contextHolder.process(context);
		c.getServer().removeContext(c);
	}

	public class Destroy implements IProcessor<Object, Object>{
		public Object process(Object in) {
			destroy();
			return in;
		}
	}
	
	public static class HttpExchangeProcessor implements IProcessor<HttpExchange, Object> {
		public enum Method{attr, context, local, remote, principal, protocol, input, output, request, response, method, uri, code};

		protected Method type = Method.attr;
		protected Object para;
		
		public void setType(Method type) {
			if(type != null) this.type = type;
		}

		public void setPara(Object para) {
			this.para = para;
		}

		@Override
		public Object process(HttpExchange exchange) {
			if (exchange != null) switch(type) {
				case attr : return exchange.getAttribute(para == null ? "" : para.toString());
				case context : if (para == null) return exchange.getHttpContext();
				else return exchange.getHttpContext().getPath();
				case local : if (para == null) return exchange.getLocalAddress();
				else return exchange.getLocalAddress().toString();
				case remote : if (para == null) return exchange.getRemoteAddress();
				else return exchange.getRemoteAddress().toString();
				case principal : if (para == null) return exchange.getPrincipal();
				else return exchange.getPrincipal().toString();
				case protocol : return exchange.getProtocol();
				case input : return exchange.getRequestBody();
				case output : return exchange.getResponseBody();
				case request : if (para == null) return exchange.getRequestHeaders();
				else return exchange.getRequestHeaders().get(para);
				case response : if (para == null) return exchange.getResponseHeaders();
				else return exchange.getResponseHeaders().get(para);
				case method : return exchange.getRequestMethod();
				case uri : if (para == null) return exchange.getRequestURI();
				else return exchange.getRequestURI().toString();
				case code : return exchange.getResponseCode();
			}
			return null;
		}
	}
}
