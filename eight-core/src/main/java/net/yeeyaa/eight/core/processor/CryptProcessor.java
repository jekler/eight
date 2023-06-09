package net.yeeyaa.eight.core.processor;

import java.io.FilterInputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.AlgorithmParameters;
import java.security.Key;
import java.security.Provider;
import java.security.SecureRandom;
import java.security.cert.Certificate;
import java.security.spec.AlgorithmParameterSpec;
import java.util.UUID;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.TypeConvertor;


public class CryptProcessor implements IProcessor<Object, Object>{
	protected final Logger log;
	protected Object algorithm = "AES/CBC/PKCS5Padding";
	protected Boolean mode;
	protected byte[] prefix;
	protected Key key;
	protected SecureRandom random;
	protected Object parameter;
	protected Certificate cert;

	public CryptProcessor() {
		this.log = LoggerFactory.getLogger(CryptProcessor.class);
		key = new SecretKeySpec(TypeConvertor.md5Encode(TypeConvertor.uuidToBytes(UUID.randomUUID())), "AES");
	}

	public CryptProcessor(Object salt, String encrypt, Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(CryptProcessor.class) : log;
		key = new SecretKeySpec(salt == null ? TypeConvertor.md5Encode(TypeConvertor.uuidToBytes(UUID.randomUUID())) : salt instanceof byte[] ? (byte[]) salt : TypeConvertor.md5Encode(salt.toString().getBytes()), encrypt == null ? "AES" : encrypt);
	}	
	
	public CryptProcessor(Certificate cert, Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(CryptProcessor.class) : log;
		this.cert = cert;
	}
	
	public CryptProcessor(Key key, Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(CryptProcessor.class) : log;
		this.key = key;
	}
	
	public void setParameter(Object parameter) {
		try {
			if (parameter instanceof AlgorithmParameterSpec || parameter instanceof AlgorithmParameters) this.parameter = parameter;
			else if (parameter instanceof byte[]) this.parameter = new IvParameterSpec((byte[])parameter);
			else if (parameter instanceof String) this.parameter = AlgorithmParameters.getInstance(parameter.toString());
			else if (parameter instanceof Object[]){
				Object[] para = (Object[])parameter;
				if(para.length > 1 && para[0] instanceof String) if (para[1] instanceof Provider) this.parameter = AlgorithmParameters.getInstance((String)para[0],(Provider)para[1]);
				else if (para[1] instanceof String) this.parameter = AlgorithmParameters.getInstance((String)para[0],(String)para[1]);
			}
			if (parameter == null) parameter = new IvParameterSpec(new byte[16]);
		} catch (Exception e) {
			log.error("init failed.", e);
        }
	}

	public void setRandom(Object random) {
		try {
			if (random instanceof SecureRandom) this.random = (SecureRandom) random;
			else if (random instanceof String) this.random = SecureRandom.getInstance((String)random);
			else if (random instanceof byte[]) this.random = new SecureRandom((byte[])random);
			else if (random instanceof Object[]) {
				Object[] para = (Object[])random;
				if (para.length > 1 && para[0] instanceof String) if (para[1] instanceof String) this.random = SecureRandom.getInstance((String)para[0], (String)para[1]);
				else if (para[1] instanceof Provider) this.random = SecureRandom.getInstance((String)para[0], (Provider)para[1]);
			} 
			if (random == null) random = new SecureRandom();
		} catch (Exception e) {
			log.error("init failed.", e);
        }
	}

	public void setAlgorithm(Object algorithm) {
		if (algorithm != null) this.algorithm = algorithm;
	}

	public void setMode(Boolean mode) {
		this.mode = mode;
	}

	public void setPrefix(byte[] prefix) {
		this.prefix = prefix;
	}

	@Override
	public Object process(Object in) {
		try {		
			if (in instanceof InputStream) {
                Cipher cipher = null; 
            	if (algorithm instanceof Object[]) {
    				Object[] para = (Object[])algorithm;
    				if (para.length > 1 && para[0] instanceof String) if (para[1] instanceof String) cipher = Cipher.getInstance((String)para[0], (String)para[1]);
    				else if (para[1] instanceof Provider) cipher = Cipher.getInstance((String)para[0], (Provider)para[1]);
    			}
            	if (cipher == null) cipher = Cipher.getInstance(algorithm.toString());
	            if (cert != null) if (random == null) cipher.init(Cipher.DECRYPT_MODE, cert);
	            else cipher.init(Cipher.DECRYPT_MODE, cert, random);
	            else if (random == null) if (parameter == null) cipher.init(Cipher.DECRYPT_MODE, key);
	            else if (parameter instanceof AlgorithmParameterSpec) cipher.init(Cipher.DECRYPT_MODE, key, (AlgorithmParameterSpec)parameter);
	            else cipher.init(Cipher.DECRYPT_MODE, key, (AlgorithmParameters)parameter);
	            else if (parameter == null) cipher.init(Cipher.DECRYPT_MODE, key, random);
	            else if (parameter instanceof AlgorithmParameterSpec) cipher.init(Cipher.DECRYPT_MODE, key, (AlgorithmParameterSpec)parameter, random);
	            else cipher.init(Cipher.DECRYPT_MODE, key, (AlgorithmParameters)parameter, random);
	            return new CipherInputStream((InputStream) in, cipher);
			} else if (in instanceof OutputStream) {
                Cipher cipher = null; 
            	if (algorithm instanceof Object[]) {
    				Object[] para = (Object[])algorithm;
    				if (para.length > 1 && para[0] instanceof String) if (para[1] instanceof String) cipher = Cipher.getInstance((String)para[0], (String)para[1]);
    				else if (para[1] instanceof Provider) cipher = Cipher.getInstance((String)para[0], (Provider)para[1]);
    			}
            	if (cipher == null) cipher = Cipher.getInstance(algorithm.toString());
	            if (cert != null) if (random == null) cipher.init(Cipher.ENCRYPT_MODE, cert);
	            else cipher.init(Cipher.ENCRYPT_MODE, cert, random);
	            else if (random == null) if (parameter == null) cipher.init(Cipher.ENCRYPT_MODE, key);
	            else if (parameter instanceof AlgorithmParameterSpec) cipher.init(Cipher.ENCRYPT_MODE, key, (AlgorithmParameterSpec)parameter);
	            else cipher.init(Cipher.ENCRYPT_MODE, key, (AlgorithmParameters)parameter);
	            else if (parameter == null) cipher.init(Cipher.ENCRYPT_MODE, key, random);
	            else if (parameter instanceof AlgorithmParameterSpec) cipher.init(Cipher.ENCRYPT_MODE, key, (AlgorithmParameterSpec)parameter, random);
	            else cipher.init(Cipher.ENCRYPT_MODE, key, (AlgorithmParameters)parameter, random);
	            return new CipherOutputStream((OutputStream) in, cipher);
			} else if (in instanceof byte[] && ((byte[])in).length > 0) {
				byte[] src = (byte[]) in;
	            if (Boolean.TRUE.equals(mode)) {
	                Cipher cipher = null; 
	            	if (algorithm instanceof Object[]) {
	    				Object[] para = (Object[])algorithm;
	    				if (para.length > 1 && para[0] instanceof String) if (para[1] instanceof String) cipher = Cipher.getInstance((String)para[0], (String)para[1]);
	    				else if (para[1] instanceof Provider) cipher = Cipher.getInstance((String)para[0], (Provider)para[1]);
	    			}
	            	if (cipher == null) cipher = Cipher.getInstance(algorithm.toString());
		            if (cert != null) if (random == null) cipher.init(Cipher.ENCRYPT_MODE, cert);
		            else cipher.init(Cipher.ENCRYPT_MODE, cert, random);
		            else if (random == null) if (parameter == null) cipher.init(Cipher.ENCRYPT_MODE, key);
		            else if (parameter instanceof AlgorithmParameterSpec) cipher.init(Cipher.ENCRYPT_MODE, key, (AlgorithmParameterSpec)parameter);
		            else cipher.init(Cipher.ENCRYPT_MODE, key, (AlgorithmParameters)parameter);
		            else if (parameter == null) cipher.init(Cipher.ENCRYPT_MODE, key, random);
		            else if (parameter instanceof AlgorithmParameterSpec) cipher.init(Cipher.ENCRYPT_MODE, key, (AlgorithmParameterSpec)parameter, random);
		            else cipher.init(Cipher.ENCRYPT_MODE, key, (AlgorithmParameters)parameter, random);
		            byte[] cipherText = cipher.doFinal(src);
		            int prefixLength = prefix == null ? 0 : prefix.length;
		            if (prefixLength > 0) {
		            	byte[] dst = new byte[prefixLength + cipherText.length];
		            	System.arraycopy(prefix, 0, dst, 0, prefixLength);
		            	return dst;
		            } else return cipherText;
	            } else if (prefix == null || prefix.length <= src.length) {
	            	if (prefix != null && prefix.length > 0) {
	            		for (int i = 0; i < prefix.length; i++) if (prefix[i] != src[i]) return mode == null ? src : null;
	            		byte[] data = new byte[src.length - prefix.length];
	                    System.arraycopy(src, prefix.length, data, 0, data.length);
	                    src = data;
	            	}
	                Cipher cipher = null; 
	            	if (algorithm instanceof Object[]) {
	    				Object[] para = (Object[])algorithm;
	    				if (para.length > 1 && para[0] instanceof String) if (para[1] instanceof String) cipher = Cipher.getInstance((String)para[0], (String)para[1]);
	    				else if (para[1] instanceof Provider) cipher = Cipher.getInstance((String)para[0], (Provider)para[1]);
	    			}
	            	if (cipher == null) cipher = Cipher.getInstance(algorithm.toString());
		            if (cert != null) if (random == null) cipher.init(Cipher.DECRYPT_MODE, cert);
		            else cipher.init(Cipher.DECRYPT_MODE, cert, random);
		            else if (random == null) if (parameter == null) cipher.init(Cipher.DECRYPT_MODE, key);
		            else if (parameter instanceof AlgorithmParameterSpec) cipher.init(Cipher.DECRYPT_MODE, key, (AlgorithmParameterSpec)parameter);
		            else cipher.init(Cipher.DECRYPT_MODE, key, (AlgorithmParameters)parameter);
		            else if (parameter == null) cipher.init(Cipher.DECRYPT_MODE, key, random);
		            else if (parameter instanceof AlgorithmParameterSpec) cipher.init(Cipher.DECRYPT_MODE, key, (AlgorithmParameterSpec)parameter, random);
		            else cipher.init(Cipher.DECRYPT_MODE, key, (AlgorithmParameters)parameter, random);
	                byte[] decryptedBytes = cipher.doFinal(src);
	                return decryptedBytes;
	            } else return mode == null ? src : null;
			}
        } catch (Exception e) {
			log.error("processor failed.", e);
        }
        return null;
	}
	
	protected class CipherInputStream extends FilterInputStream {
		protected Cipher cipher;
		protected InputStream input;
		protected byte[] ibuffer = new byte[512];
		protected boolean done = false;
		protected byte[] obuffer;
		protected int ostart = 0;
		protected int ofinish = 0;
		protected boolean closed = false;
		protected boolean encrypt = true;
		
		protected int getMoreData() throws IOException {
	        if (done) return -1;
	        else {
	            int c = input.read(ibuffer);
	            if (c == -1) { 
	                done = true;
	                try {
	                    obuffer = cipher.doFinal();
	                } catch (Exception e) {
	                    obuffer = null;
	                    throw new IOException(e);
	                }
	                if (obuffer == null) return -1;
	                else {
	                    ostart = 0;
	                    ofinish = obuffer.length;
	                    return ofinish;
	                }
	            } else {
	                try {
	                    obuffer = cipher.update(ibuffer, 0, c);
	                } catch (IllegalStateException e) {
	                    obuffer = null;
	                    throw e;
	                }
	                ostart = 0;
	                if (obuffer == null) ofinish = 0;
	                else ofinish = obuffer.length;
	                return ofinish;
	            }
	        }
	    }

	    public CipherInputStream(InputStream in, Cipher c) throws IOException {
	        super(in);
	        input = in;
	        cipher = c;
	        if (prefix != null && prefix.length > 0) {
	        	byte[] buffer = new byte[prefix.length];
	        	if (in.read(buffer) != prefix.length) encrypt = false;
	        	else for (int i = 0; i < prefix.length; i++ ) if (buffer[i] != prefix[i]) {
	        		encrypt =false;
	        		break;
	        	}
	        	if (!encrypt)  if (mode == null) obuffer = buffer;
	        	else throw new IOException("Invalid format");	
	        }
	    }

	    public int read() throws IOException {
	    	if (encrypt) {
		        if (ostart >= ofinish) {
		            int c;
		            for(c = 0; c == 0; c = getMoreData());
		            if (c == -1) return -1;
		        }
		        return obuffer[ostart++] & 255;
	    	} else if (obuffer == null) return input.read();
	    	else {
	    		int ret = (int) obuffer[ostart++];
	    		if (ostart >= obuffer.length) obuffer = null;
	    		return ret;
	    	}
	    }

	    public int read(byte[] b) throws IOException {
	        return read(b, 0, b.length);
	    }

	    public int read(byte[] b, int off, int len) throws IOException {
	    	if (encrypt) {
		        int c;
		        if (ostart >= ofinish) {
		            for(c = 0; c == 0; c = getMoreData());
		            if (c == -1) return -1;
		        }
		        if (len <= 0) return 0;
		        else {
		            c = ofinish - ostart;
		            if (len < c) c = len;
		            if (b != null) System.arraycopy(obuffer, ostart, b, off, c);
		            ostart += c;
		            return c;
		        }
	    	} else if (obuffer == null) return input.read(b, off, len);
	    	else {
	    		int c = obuffer.length - ostart;
	    		if (c > len) {
	    			System.arraycopy(obuffer, ostart, b, off, len);
	    			ostart += len;
	    			return len;
	    		} else {
	    			len -= c;
	    			System.arraycopy(obuffer, ostart, b, off, c);
	    			obuffer = null;
	    			if (len > 0) {
	    				len = input.read(b, off + c, len);
	    				if (len < 0) return c;
	    				else return c + len;
	    			} else return c;
	    		}
	    	}
	    }

	    public long skip(long n) throws IOException {
	    	if (n > 0) if (encrypt) {
		        int offset = ofinish - ostart;
		        if (n > (long) offset) n = (long) offset;
		        if (n < 0L)  return 0L;
		        else {
		            ostart = (int)((long)ostart + n);
		            return n;
		        }
	    	} else if (obuffer == null) return input.skip(n);
	    	else {
	    		int c = obuffer.length - ostart;
	    		if (c > n) {
	    			ostart += n;
	    			return n;
	    		} else {
	    			obuffer = null;
	    			return input.skip(n -c) + c;
	    		}
	    	} else return 0;
	    }

	    public int available() throws IOException {
	    	if (encrypt) return ofinish - ostart;
	    	else if (obuffer == null) return input.available();
	    	else return obuffer.length - ostart + input.available();
	    }

	    public void close() throws IOException {
	        if (!closed) {
	            closed = true;
	            input.close();
	            if (!done) {
		            ostart = 0;
		            ofinish = 0;
	                try {
	                    cipher.doFinal();
	                } catch (Exception e) {
	                	throw new IOException(e);
	                }
	            }
	        }
	    }

	    public boolean markSupported() {
	        return false;
	    }
	}
	
	protected class CipherOutputStream extends FilterOutputStream {
		protected Cipher cipher;
		protected OutputStream output;
		protected byte[] ibuffer = new byte[1];
		protected byte[] obuffer;
		protected boolean closed = false;

	    public CipherOutputStream(OutputStream out, Cipher c) throws IOException {
	        super(out);
	        output = out;
	        cipher = c;
	        if (prefix != null && prefix.length > 0) out.write(prefix);
	    }
	    
	    public void write(int b) throws IOException {
	        ibuffer[0] = (byte)b;
	        obuffer = cipher.update(ibuffer, 0, 1);
	        if (obuffer != null) {
	            output.write(obuffer);
	            obuffer = null;
	        }
	    }

	    public void write(byte[] b) throws IOException {
	        write(b, 0, b.length);
	    }

	    public void write(byte[] b, int off, int len) throws IOException {
	        obuffer = cipher.update(b, off, len);
	        if (obuffer != null) {
	            output.write(obuffer);
	            obuffer = null;
	        }
	    }

	    public void flush() throws IOException {
	        if (obuffer != null) {
	            output.write(obuffer);
	            obuffer = null;
	        }
	        output.flush();
	    }

	    public void close() throws IOException {
	        if (!closed) {
	            closed = true;
	            try {
	                obuffer = cipher.doFinal();
	            } catch (Exception e) {
	                obuffer = null;
	                throw new IOException(e);
	            } finally {
		            try {
		                flush();
		            } catch (IOException e) {
	                	throw new IOException(e);
		            } finally {
		            	out.close();
		            }
	            }
	        }
	    }
	}
}
