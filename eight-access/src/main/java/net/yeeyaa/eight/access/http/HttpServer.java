package net.yeeyaa.eight.access.http;

import java.io.InputStream;
import java.net.InetSocketAddress;
import java.security.KeyStore;
import java.security.cert.X509Certificate;
import java.util.concurrent.ExecutorService;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLParameters;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.core.storage.Storage.Method;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sun.net.httpserver.HttpContext;
import com.sun.net.httpserver.HttpsServer;
import com.sun.net.httpserver.HttpsConfigurator;
import com.sun.net.httpserver.HttpsParameters;

public class HttpServer implements IProcessor<String, HttpContext>{
	protected final Logger log;
	protected com.sun.net.httpserver.HttpServer server;
	protected String host;
	protected Integer port = 7241;
	protected ExecutorService executor;
	protected IProcessor<Object, Object> destroy;
	protected Integer backlog = 0;
	protected Boolean https = false;
	protected String protocol = "TLS";
	protected String keystore = "JKS";
	protected String algorithm = "SunX509";
	protected String trustAlgorithm = "SunX509";
	protected IExtendable<Object> keyfile;
	protected String password;
	protected IProcessor<Object[], Void> configurator;
	protected SSLContext sslContext;
	protected Integer wait = 0;
	protected Boolean trust;
	
	public HttpServer() {
		this.log = LoggerFactory.getLogger(HttpServer.class);
	}

	public HttpServer(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(HttpServer.class) : log;
	}
	
	public void setWait(Integer wait) {
		if(wait > 0) this.wait = wait;
	}

	public void setDestroy(IProcessor<Object, Object> destroy) {
		this.destroy = destroy;
	}

	public void setSslContext(SSLContext sslContext) {
		this.sslContext = sslContext;
	}

	public void setHost(String host) {
		this.host = host;
	}

	public void setBacklog(Integer backlog) {
		if(backlog != null) this.backlog = backlog;
	}
	
	public void setPort(Integer port) {
		if(port != null && port > 0 && port < 65535) this.port = port;
	}

	public void setExecutor(ExecutorService executor) {
		this.executor = executor;
	}

	public void setHttps(Boolean https) {
		if(https != null) this.https = https;
	}

	public void setProtocol(String protocol) {
		if(protocol != null) this.protocol = protocol;
	}

	public void setKeystore(String keystore) {
		if(keystore != null) this.keystore = keystore;
	}

	public void setAlgorithm(String algorithm) {
		if(algorithm != null) this.algorithm = algorithm;
	}

	public void setTrustAlgorithm(String trustAlgorithm) {
		if(trustAlgorithm != null) this.trustAlgorithm = trustAlgorithm;
	}

	public void setKeyfile(IExtendable<Object> keyfile) {
		this.keyfile = keyfile;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public void setConfigurator(IProcessor<Object[], Void> configurator) {
		this.configurator = configurator;
	}
	
	public void setTrust(Boolean trust) {
		this.trust = trust;
	}

	@PostConstruct
	public void initialize(){ 
		try{
			InetSocketAddress address;
			if(host == null) address = new InetSocketAddress(port);
			else address = new InetSocketAddress(host, port);
			if(https) {
				HttpsServer httpsServer = HttpsServer.create (address, backlog);
			    if(this.sslContext == null) {
			    	sslContext = SSLContext.getInstance (protocol);
				    KeyStore ks = KeyStore.getInstance (keystore);
				    char[] password = null;
				    if(this.password != null) password = this.password.toCharArray();
				    InputStream is = null;
		            try {
		            	is = keyfile.<InputStream>extend(Method.input);
		                ks.load(is, password);
		            } finally {
		                if (is != null) is.close();
		            }
				    KeyManagerFactory kmf = KeyManagerFactory.getInstance(algorithm);
				    kmf.init(ks, password);
				    TrustManager[] trm;
				    if (Boolean.FALSE.equals(trust)) trm = new TrustManager[]{new X509TrustManager() {
					    public X509Certificate[] getAcceptedIssuers() {
					        return null;
					    }

					    public void checkClientTrusted(X509Certificate[] certs, String authType) {}

					    public void checkServerTrusted(X509Certificate[] certs, String authType) {}
					}}; else {
					    TrustManagerFactory tmf = TrustManagerFactory.getInstance(trustAlgorithm);
					    tmf.init(ks);
					    trm = tmf.getTrustManagers();
					}
				    sslContext.init(kmf.getKeyManagers (), trm, null);
			    }
			    if(configurator == null) httpsServer.setHttpsConfigurator(new HttpsConfigurator(sslContext){
			        public void configure ( HttpsParameters params ){
			            try{
			            	SSLParameters paras = sslContext.getDefaultSSLParameters();
			                params.setNeedClientAuth (false);
			            	params.setWantClientAuth(paras.getWantClientAuth());
			                params.setCipherSuites(paras.getCipherSuites());
			                params.setProtocols(paras.getProtocols());
			                params.setSSLParameters(paras);
			            } catch (Exception e){
			                log.error ("Failed to create HTTPS port", e);
			            }
			        }
			    } ); else httpsServer.setHttpsConfigurator(new HttpsConfigurator(sslContext){
			        public void configure (HttpsParameters params){
			        	configurator.process(new Object[]{params, sslContext});
			        }
			    } );
			    server = httpsServer;
			} else server = com.sun.net.httpserver.HttpServer.create(address, backlog);  
	        if(executor != null) server.setExecutor(executor);  
	        server.start();
		}catch(Exception e){
			log.error("http server start error.", e);
		}
	}
	
	@PreDestroy
	public void destroy() {
		server.stop(wait);
		if (destroy != null) destroy.process(executor);
	}
	
	public class Destroy implements IProcessor<Object, Object>{
		public Object process(Object in) {
			destroy();
			return in;
		}
	}

	@Override
	public HttpContext process(String context) {
		return server.createContext(context);
	}
}
