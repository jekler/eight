package net.yeeyaa.eight.client.protocol;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.security.KeyStore;
import java.security.cert.X509Certificate;
import java.util.Map;
import java.util.Map.Entry;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import javax.annotation.PostConstruct;
import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.PlatformException.DefaultError;
import net.yeeyaa.eight.client.data.ClientMsg;
import net.yeeyaa.eight.core.storage.Storage.Method;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class HttpStreamProtocol implements IProcessor<ClientMsg, ClientMsg> {
	protected final Logger log;
	protected String url;
	protected IProcessor<InputStream, InputStream> gzipInput;
	protected IProcessor<OutputStream, OutputStream> gzipOutput;
	protected Boolean compress = true;
	protected Integer readMode = 0;
	protected Map<String, String> properties;
	protected HostnameVerifier verifier;
	protected String protocol = "TLS";
	protected String keystore = "JKS";
	protected String algorithm = "SunX509";
	protected String trustAlgorithm = "SunX509";
	protected IExtendable<Object> keyfile;
	protected String password;
	protected SSLSocketFactory sslContextFactory;
	protected Integer buffer = 8192;
	protected Boolean trust;
	
	public HttpStreamProtocol() {
		this.log = LoggerFactory.getLogger(HttpStreamProtocol.class);
	}

	public HttpStreamProtocol(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(HttpStreamProtocol.class) : log;
	}
	
	public void setBuffer(Integer buffer) {
		if(buffer != null && buffer > 0) this.buffer = buffer;
	}

	public void setSslContext(SSLContext sslContext) {
		if(sslContext != null) this.sslContextFactory = sslContext.getSocketFactory();
	}
	
	public void setGzipInput(IProcessor<InputStream, InputStream> gzipInput) {
		this.gzipInput = gzipInput;
	}

	public void setGzipOutput(IProcessor<OutputStream, OutputStream> gzipOutput) {
		this.gzipOutput = gzipOutput;
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

	public void setTrust(Boolean trust) {
		this.trust = trust;
	}
	
	public void setVerifier(final IProcessor<Object[], Boolean> verifier) {
		if(verifier != null) this.verifier = new HostnameVerifier(){
			@Override
			public boolean verify(String hostname, SSLSession session) {
				return verifier.process(new Object[]{hostname, session});
			}
		};
	}
	
	public void setReadMode(Integer readMode) {
		if(readMode != null) this.readMode = readMode;
	}
	
	public void setCompress(Boolean compress) {
		if(compress != null) this.compress = compress;
	}

	public void setProperties(Map<String, String> properties) {
		this.properties = properties;
	}

	public void setUrl(String url) {
		this.url = url;
	}
	
	@PostConstruct
	public void initialize(){
		if(sslContextFactory == null && keyfile != null) try{
			SSLContext sslContext = SSLContext.getInstance (protocol);
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
		    sslContextFactory = sslContext.getSocketFactory();
		}catch(Exception e) {
			log.error("HttpProtocol: init ssl context fail.", e);
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

	protected Object request(String name, String token, String id, IProcessor<OutputStream, IProcessor<InputStream, Object>> processor) throws IOException{		
		byte[] data = new byte[buffer];
		InputStream input = null;
		StringBuilder sb = new StringBuilder(url).append("?name=").append(name);
		if (token != null) sb.append("&token=").append(token);
		if (id != null) sb.append("&id=").append(id);
		HttpURLConnection conn = (HttpURLConnection)new URL(sb.toString()).openConnection();
		try{
			if(conn instanceof HttpsURLConnection){
				if(verifier != null) ((HttpsURLConnection)conn).setHostnameVerifier(verifier);
				if(sslContextFactory != null) ((HttpsURLConnection)conn).setSSLSocketFactory(sslContextFactory);
			}
			conn.setDoOutput(true);
			conn.setDoInput(true);
			conn.setRequestMethod("PUT");
			if(readMode == 0 || readMode == 1) conn.setRequestProperty("Accept-Encoding", "gzip");
			if(compress) conn.setRequestProperty("Content-Encoding", "gzip");
			if(properties != null) for(Entry<String, String> entry : properties.entrySet()) conn.setRequestProperty(entry.getKey(), entry.getValue());
			OutputStream out;
			IProcessor<InputStream, Object> second = null; 
			if(compress) out = gzipOutput == null ? new GZIPOutputStream(conn.getOutputStream()) : gzipOutput.process(conn.getOutputStream());
			else out = conn.getOutputStream();
			try{
				second = processor.process(out);
			}finally{
				out.close();
			}
		    switch(readMode){
		    	case 0: input = decompressStream(conn.getInputStream());
		    	break;
		    	case 1: input = gzipInput == null ? new GZIPInputStream(conn.getInputStream()) : gzipInput.process(conn.getInputStream());
		    	break;
		    	default: input = conn.getInputStream();
		    }
			try{
			    if (second == null) return null;
			    else return second.process(input);
			} finally {
				try {
					while ((input.read(data, 0, data.length)) != -1);
				} catch (Throwable t) {} finally {
					input.close();
				}
			}
		} catch(IOException e) {
			input = conn.getErrorStream();
			if (input != null) try {
				while ((input.read(data, 0, data.length)) != -1);
			} catch (Throwable t) {} finally {
				input.close();
			}
			if (conn.getResponseCode() == HttpURLConnection.HTTP_INTERNAL_ERROR) {
				String warning = conn.getHeaderField("Warning");;
				if (warning != null) {
					String[] param = warning.split(" ");
					if (param.length == 3) throw new PlatformException(new DefaultError(param[1], Integer.parseInt(param[0]), param[2]));
				}
			}
			throw e;
		}
	}

	@Override
	public ClientMsg process(ClientMsg instance) {
		try {
			Object ret = request(instance.name, instance.token, instance.id, (IProcessor<OutputStream, IProcessor<InputStream, Object>>)instance.content);
			instance.content = ret;
			return instance;
		} catch (IOException e) {
			DefaultError se = new DefaultError();
			se.setCate("ConnectServerError");
			se.setCode(999);
			se.setMessage(e.getMessage());
			throw new PlatformException(se, e);
		}
	}
}
