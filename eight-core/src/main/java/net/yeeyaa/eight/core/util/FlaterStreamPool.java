package net.yeeyaa.eight.core.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PushbackInputStream;
import java.io.SequenceInputStream;
import java.io.UnsupportedEncodingException;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Vector;
import java.util.Map.Entry;
import java.util.jar.Attributes;
import java.util.jar.JarFile;
import java.util.jar.Manifest;
import java.util.zip.CRC32;
import java.util.zip.CheckedInputStream;
import java.util.zip.Deflater;
import java.util.zip.DeflaterInputStream;
import java.util.zip.DeflaterOutputStream;
import java.util.zip.InflaterOutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.Inflater;
import java.util.zip.InflaterInputStream;
import static java.util.zip.ZipEntry.DEFLATED;
import static java.util.zip.ZipEntry.STORED;
import static java.util.zip.GZIPInputStream.GZIP_MAGIC;

import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.ITriProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;
import net.yeeyaa.eight.core.util.Content.Couple;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class FlaterStreamPool implements IProcessor<Object, Object>, IBiProcessor<Object, String, Object>, ITriProcessor<Object, String, Integer, Object>, Runnable, Comparator<Object[]> {	
	protected final Logger log;
    protected static final int JAR_MAGIC = 0xCAFE;
    protected static final int FHCRC	= 2;
    protected static final int FEXTRA	= 4;
    protected static final int FNAME	= 8;
    protected static final int FCOMMENT	= 16;
	protected static final int TRAILER_SIZE = 8;
	protected static final long LOCSIG = 0x04034b50L;
	protected static final long EXTSIG = 0x08074b50L;
	protected static final long CENSIG = 0x02014b50L;
	protected static final long ENDSIG = 0x06054b50L;
	protected static final int LOCHDR = 30;
	protected static final int EXTHDR = 16;
	protected static final int CENHDR = 46;
	protected static final int ENDHDR = 22;
	protected static final int LOCVER = 4;
	protected static final int LOCFLG = 6;
	protected static final int LOCHOW = 8;
	protected static final int LOCTIM = 10;
	protected static final int LOCCRC = 14;
	protected static final int LOCSIZ = 18;
	protected static final int LOCLEN = 22;
	protected static final int LOCNAM = 26;
	protected static final int LOCEXT = 28;
	protected static final int EXTCRC = 4;
	protected static final int EXTSIZ = 8;
	protected static final int EXTLEN = 12;
	protected static final int CENVEM = 4;
	protected static final int CENVER = 6;
	protected static final int CENFLG = 8;
	protected static final int CENHOW = 10;
	protected static final int CENTIM = 12;
	protected static final int CENCRC = 16;
	protected static final int CENSIZ = 20;
	protected static final int CENLEN = 24;
	protected static final int CENNAM = 28;
	protected static final int CENEXT = 30;
	protected static final int CENCOM = 32;
	protected static final int CENDSK = 34;
	protected static final int CENATT = 36;
	protected static final int CENATX = 38;
	protected static final int CENOFF = 42;
	protected static final int ENDSUB = 8;
	protected static final int ENDTOT = 10;
	protected static final int ENDSIZ = 12;
	protected static final int ENDOFF = 16;
	protected static final int ENDCOM = 20;
	protected static final byte[] buf = new byte[0];
	protected static final byte[] header = {(byte) GZIP_MAGIC, (byte)(GZIP_MAGIC >> 8), Deflater.DEFLATED, 0, 0, 0, 0, 0, 0, 0};
	protected volatile LinkedList<Entry<Deflater, Boolean>> deflaters = new LinkedList<Entry<Deflater, Boolean>>();
	protected volatile LinkedList<Entry<Inflater, Boolean>> inflaters = new LinkedList<Entry<Inflater, Boolean>>();
	protected int level = Deflater.DEFAULT_COMPRESSION;
	protected int strategy = Deflater.DEFAULT_STRATEGY;
	protected int buffer = 512;
	protected boolean nowrap = true;
	protected Integer size = -1; 
	protected Mode mode = Mode.gzip; 
	protected volatile long lastRecycle = System.currentTimeMillis();
	protected long interval;
	
	public enum Mode{zip, gzip, jar, flater, rflater}
	
	public FlaterStreamPool() {
		this.log = LoggerFactory.getLogger(FlaterStreamPool.class);
	}
	
	public FlaterStreamPool(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(FlaterStreamPool.class) : log;
	}
	
	public void setLevel(Integer level) {
		if (level != null && level < 10 && level > -1) this.level = level;
	}

	public void setInterval(Long interval) {
		if (interval != null && interval > 0) this.interval = interval;
	}

	public void setStrategy(Integer strategy) {
		if (strategy != null && strategy < 3 && strategy > 0) this.strategy = strategy;
	}

	public void setNowrap(Boolean nowrap) {
		if (nowrap != null) this.nowrap = true;
	}

	public void setBuffer(Integer buffer) {
		if (buffer != null && buffer > 0) this.buffer = buffer;
	}

	public void setMode(Mode mode) {
		if (mode != null) this.mode = mode;
	}

	public void setSize(Integer size) {
		if (size != null && size >= 0) this.size = size;
	}
    
	@Override
	public Object process(Object instance) {
		return operate(instance, null, buffer);
	}

	@Override
	public Object perform(Object instance, String type) {
		return operate(instance, type, buffer);
	}
	
	@Override
	public Object operate(Object instance, String type, Integer buffer) {
		if (Boolean.FALSE.equals(instance)) reset();
		else try {
			buffer = buffer != null && buffer > 0 ? buffer : this.buffer;
			Mode mode = this.mode;
			if (type != null) try {
				mode = Mode.valueOf(type); 
			} catch (Exception e) {
				log.error("FlaterStreamPool: mode string illegal.", e);	
			}
			switch (mode) {
				case flater: if (instance instanceof InputStream) return new InfInputStream((InputStream) instance, getInflater(), buffer);
					else if (instance instanceof OutputStream) return new DefOutputStream((OutputStream) instance, getDeflater(), buffer);
					break;
				case rflater: if (instance instanceof InputStream) return new DefInputStream((InputStream) instance, getDeflater(), buffer);
					else if (instance instanceof OutputStream) return new InfOutputStream((OutputStream) instance, getInflater(), buffer);
					break;
				case zip: if (instance instanceof InputStream) return new ZipInputStream((InputStream) instance, this, buffer);
					else if (instance instanceof OutputStream) return new ZipOutputStream((OutputStream) instance, this, buffer);
					break;
				case jar: if (instance instanceof InputStream) return new JarInputStream((InputStream) instance, this, buffer);
					else if (instance instanceof OutputStream) return new JarOutputStream((OutputStream) instance, this, buffer);
					break;
				default: if (instance instanceof InputStream) return new GzipInputStream((InputStream) instance, getInflater(), buffer);
					else if (instance instanceof OutputStream) return new GzipOutputStream((OutputStream) instance, getDeflater(), buffer);
					break;
			}
		} catch (Exception e) {
            log.error("FlaterStreamPool: convert stream fail.", e);	
		}
		return null;
	}
	
	@Override
	public int compare(Object[] ret, Object[] para) {
		Object r = null;
		if (para == null || para.length < 1) return -1;
		else if (para.length > 2) r = operate(para[0], para[1] == null ? null : para[1].toString(), (Integer)para[2]);
		else if (para.length > 1) r = perform(para[0], para[1] == null ? null : para[1].toString());
		else if (para[0] instanceof InputStream || para[0] instanceof OutputStream) r = process(para[0]);
		else if (para[0] instanceof Inflater) setInflater((Inflater)para[0]);
		else if (para[0] instanceof Deflater) setDeflater((Deflater)para[0]);	
		else if (Boolean.TRUE.equals(para[0])) r = getInflater();
		else if (Boolean.FALSE.equals(para[0])) reset();
		else r = getDeflater();
		if (ret != null && ret.length > 0) ret[0] = r;
		return 0;
	}
	
	@Override
	public void run() {
		LinkedList<Deflater> defs = new LinkedList<Deflater>();
		synchronized(deflaters) {
			Iterator<Entry<Deflater, Boolean>> itr = deflaters.iterator();
			while (itr.hasNext()) {
				Entry<Deflater, Boolean> entry = itr.next();
				if (entry.getValue()) {
					defs.add(entry.getKey());
					itr.remove();
				} else entry.setValue(true);
			}
		}
		LinkedList<Inflater> infs = new LinkedList<Inflater>();
		synchronized(inflaters) {
			Iterator<Entry<Inflater, Boolean>> itr = inflaters.iterator();
			while (itr.hasNext()) {
				Entry<Inflater, Boolean> entry = itr.next();
				if (entry.getValue()) {
					infs.add(entry.getKey());
					itr.remove();
				} else entry.setValue(true);
			}
		}
		for (Inflater inf : infs) inf.end();
		for (Deflater def : defs) def.end();
	}
	
	protected void recycle() {
		if (interval > 0 && System.currentTimeMillis() > lastRecycle + interval) synchronized(this) {
			if (System.currentTimeMillis() > lastRecycle + interval) {
				run();
				lastRecycle = System.currentTimeMillis();
			}
		}
	}
	
	protected void recycleDeflater() {
		LinkedList<Deflater> defs = new LinkedList<Deflater>();
		synchronized(deflaters) {
			int count = deflaters.size() / 2;
			if (count > 0) for (int i = 0; i < count; i++) defs.add(deflaters.removeLast().getKey());
		}
		for (Deflater def : defs) def.end();
	}
	
	protected void recycleInflater() {
		LinkedList<Inflater> infs = new LinkedList<Inflater>();
		synchronized(inflaters) {
			int count = inflaters.size() / 2;
			if (count > 0) for (int i = 0; i < count; i++) infs.add(inflaters.removeLast().getKey());
		}
		for (Inflater inf : infs) inf.end();
	}
	
	protected Deflater getDeflater(){
		Deflater deflater = null;
		if (deflaters.size() > 0) synchronized(deflaters) {
			if (deflaters.size() > 0) deflater = deflaters.removeFirst().getKey();
		}
		if (deflater == null) {
			deflater = new Deflater(level, nowrap);
			if (strategy != Deflater.DEFAULT_STRATEGY) deflater.setStrategy(strategy);
		}
		return deflater;
	}
	
	protected Inflater getInflater(){
		Inflater inflater = null;
		if (inflaters.size() > 0) synchronized(inflaters) {
			if (inflaters.size() > 0) inflater = inflaters.removeFirst().getKey();
		}
		if (inflater == null) inflater = new Inflater(nowrap);
		return inflater;
	}
	
	protected void setDeflater(Deflater deflater){
		recycle();
		if (size >= 0 && deflaters.size() >= size) {
			deflater.end();
			synchronized(deflaters) {
				if (deflaters.size() >= size) recycleDeflater();
			}
		} else {
			deflater.setInput(buf, 0, 0);
			deflater.reset();
			synchronized(deflaters) {
				deflaters.addFirst(new Couple<Deflater, Boolean>(deflater, false));
			}
		}
	}
	
	protected void setInflater(Inflater inflater){
		recycle();
		if (size >= 0 && inflaters.size() >= size) {
			inflater.end();
			synchronized(inflaters) {
				if (inflaters.size() >= size) recycleInflater();
			}
		} else {
			inflater.reset();
			synchronized(inflaters) {
				inflaters.addFirst(new Couple<Inflater, Boolean>(inflater, false));
			}
		}
	}
	
	@PreDestroy
	public void reset() {
		LinkedList<Entry<Deflater, Boolean>> defs = deflaters;
		deflaters = new LinkedList<Entry<Deflater, Boolean>>();
		LinkedList<Entry<Inflater, Boolean>> infs = inflaters;
		inflaters = new LinkedList<Entry<Inflater, Boolean>>();
		synchronized(defs) {
			Iterator<Entry<Deflater, Boolean>> itr = defs.iterator();
			while (itr.hasNext()) {
				Entry<Deflater, Boolean> entry = itr.next();
				entry.getKey().end();
			}
		}
		synchronized(infs) {
			Iterator<Entry<Inflater, Boolean>> itr = infs.iterator();
			while (itr.hasNext()) {
				Entry<Inflater, Boolean> entry = itr.next();
				entry.getKey().end();
			}
		}
	}
	
	public class InfOutputStream extends InflaterOutputStream implements Comparator<Object[]> {
		protected boolean closed = false;
	    
	    public InfOutputStream(OutputStream out, Inflater infl, Integer size) {
			super(out, infl, size != null && size > 0 ? size : 512);
		}

		@Override
		public synchronized void close() throws IOException {
			if (!closed) try {
				super.close();
	            closed = true;
			} finally {
	        	setInflater(inf);
			}
		}

		@Override
		public int compare(Object[] ret, Object[] para) {
			if (para == null || para.length < 1) return -1;
			else try {
				if ("finish".equals(para[0])) finish();
				else if ("close".equals(para[0])) close();
				else if ("write".equals(para[0]) && para.length > 1) {
					if (para[1] instanceof Object[]) write((byte[])((Object[])para[1])[0], (Integer)((Object[])para[1])[1], (Integer)((Object[])para[1])[2]);
					else if (para[1] instanceof Integer) write((Integer)para[1]);
					else if (para[1] instanceof byte[]) write((byte[])para[1]);
				} 
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
			}
			if (ret != null && ret.length > 0) ret[0] = null;
			return 0;
		}
	}
	
	public class DefInputStream extends DeflaterInputStream implements Comparator<Object[]> {
		protected boolean closed = false;
		
		public DefInputStream(InputStream in, Deflater defl, Integer size) {
			super(in, defl, size != null && size > 0 ? size : 512);
		}
	    
		@Override
		public synchronized void close() throws IOException {
			if (!closed) try {
				super.close();
	            closed = true;
			} finally {
	        	setDeflater(def);
			}
		}
		
		@Override
		public int compare(Object[] ret, Object[] para) {
			Object r = null;
			if (para == null || para.length < 1) return -1;
			else try {
				if ("read".equals(para[0])) {
					if (para.length > 1) {
						if (para[1] instanceof byte[]) r = read((byte[])para[1]);
						else if (para[1] instanceof Object[]) r = read((byte[])((Object[])para[1])[0], (Integer)((Object[])para[1])[1], (Integer)((Object[])para[1])[2]);
					}
				} else if ("available".equals(para[0])) r = available();
				else if ("close".equals(para[0])) close();
				else if ("markSupported".equals(para[0])) r= markSupported();
				else if ("reset".equals(para[0])) reset();
				else if ("mark".equals(para[0])) {
					if (para.length > 1 && para[1] instanceof Integer) mark((Integer)para[1]);
				} else if ("skip".equals(para[0]) && para.length > 1 && para[1] instanceof Long) r = skip((Long)para[1]);
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);	
			}
			if (ret != null && ret.length > 0) ret[0] = r;
			return 0;
		}
	}
	
	public class InfInputStream extends InflaterInputStream implements Comparator<Object[]> {
	    protected boolean closed = false;
	    
		public InfInputStream(InputStream in, Inflater inf, Integer size) {
			super(in, inf, size != null && size > 0 ? size : 512);
		}

		@Override
		public synchronized void close() throws IOException {
			if (!closed) try {
				super.close();
	            closed = true;
			} finally {
	        	setInflater(inf);
			}
		}

		@Override
		public int compare(Object[] ret, Object[] para) {
			Object r = null;
			if (para == null || para.length < 1) return -1;
			else try {
				if ("read".equals(para[0])) {
					if (para.length > 1) {
						if (para[1] instanceof byte[]) r = read((byte[])para[1]);
						else if (para[1] instanceof Object[]) r = read((byte[])((Object[])para[1])[0], (Integer)((Object[])para[1])[1], (Integer)((Object[])para[1])[2]);
					}
				} else if ("available".equals(para[0])) r = available();
				else if ("close".equals(para[0])) close();
				else if ("markSupported".equals(para[0])) r= markSupported();
				else if ("reset".equals(para[0])) reset();
				else if ("mark".equals(para[0])) {
					if (para.length > 1 && para[1] instanceof Integer) mark((Integer)para[1]);
				} else if ("skip".equals(para[0]) && para.length > 1 && para[1] instanceof Long) r = skip((Long)para[1]);
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
			}
			if (ret != null && ret.length > 0) ret[0] = r;
			return 0;
		}
	}
	
	public class DefOutputStream extends DeflaterOutputStream implements Comparator<Object[]> {
		protected boolean closed = false;
		
	    public DefOutputStream(OutputStream out, Deflater def, Integer size) {
			super(out, def, size != null && size > 0 ? size : 512);
	    }
	    
		@Override
		public synchronized void close() throws IOException {
			if (!closed) try {
				super.close();
	            closed = true;
			} finally {
	        	setDeflater(def);
			}
		}
		
		@Override
		public int compare(Object[] ret, Object[] para) {
			if (para == null || para.length < 1) return -1;
			else try {
				if ("finish".equals(para[0])) finish();
				else if ("close".equals(para[0])) close();
				else if ("write".equals(para[0]) && para.length > 1) {
					if (para[1] instanceof Object[]) write((byte[])((Object[])para[1])[0], (Integer)((Object[])para[1])[1], (Integer)((Object[])para[1])[2]);
					else if (para[1] instanceof Integer) write((Integer)para[1]);
					else if (para[1] instanceof byte[]) write((byte[])para[1]);
				} 
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
			}
			if (ret != null && ret.length > 0) ret[0] = null;
			return 0;
		}
	}
		
	public class GzipInputStream extends InfInputStream {
	    protected final CRC32 crc = new CRC32();
	    protected final byte[] tmpbuf = new byte[128];
	    protected boolean eos;

	    protected void ensureOpen() throws IOException {
	    	if (closed) throw new IOException("Stream closed");
	    }

	    public GzipInputStream(InputStream in, Inflater inf, Integer size) throws IOException {
	    	super(in, inf, size != null && size > 0 ? size : 512);
	        readHeader(in);
	    }

	    public int read(byte[] buf, int off, int len) throws IOException {
	        ensureOpen();
	        if (eos) return -1;
	        int n = super.read(buf, off, len);
	        if (n == -1) if (readTrailer()) eos = true;
	        else return this.read(buf, off, len);
	        else crc.update(buf, off, n);
	        return n;
	    }

	    public void close() throws IOException {
            eos = true;
            super.close();	
	    }

	    protected int readHeader(InputStream this_in) throws IOException {
	        CheckedInputStream in = new CheckedInputStream(this_in, crc);
	        crc.reset();
	        if (readUShort(in) != GZIP_MAGIC) throw new IOException("Not in GZIP format");
	        if (readUByte(in) != 8) throw new IOException("Unsupported compression method");
	        int flg = readUByte(in);
	        skipBytes(in, 6);
	        int n = 10;
	        if ((flg & FEXTRA) == FEXTRA) {
	            int m = readUShort(in);
	            skipBytes(in, m);
	            n += m + 2;
	        }
	        if ((flg & FNAME) == FNAME) do {
                n++;
            } while (readUByte(in) != 0);
	        if ((flg & FCOMMENT) == FCOMMENT) do {
                n++;
            } while (readUByte(in) != 0);
	        if ((flg & FHCRC) == FHCRC) {
	            int v = (int)crc.getValue() & 0xffff;
	            if (readUShort(in) != v) throw new IOException("Corrupt GZIP header");
	            n += 2;
	        }
	        crc.reset();
	        return n;
	    }

	    protected boolean readTrailer() throws IOException {
	        InputStream in = this.in;
	        int n = inf.getRemaining();
	        if (n > 0) in = new SequenceInputStream(new ByteArrayInputStream(buf, len - n, n), in);
	        if ((readUInt(in) != crc.getValue()) || (readUInt(in) != (inf.getBytesWritten() & 0xffffffffL))) throw new IOException("Corrupt GZIP trailer");
	        if (this.in.available() > 0 || n > 26) {
	            int m = 8;                  
	            try {
	                m += readHeader(in);    
	            } catch (IOException ze) {
	                return true;  
	            }
	            inf.reset();
	            if (n > m) inf.setInput(buf, len - n + m, n - m);
	            return false;
	        }
	        return true;
	    }

	    protected long readUInt(InputStream in) throws IOException {
			long s = readUShort(in);
			return ((long)readUShort(in) << 16) | s;
	    }

	    protected int readUShort(InputStream in) throws IOException {
			int b = readUByte(in);
			return ((int)readUByte(in) << 8) | b;
	    }

	    protected int readUByte(InputStream in) throws IOException {
			int b = in.read();
			if (b == -1) throw new EOFException();
	        if (b < -1 || b > 255) throw new IOException(this.in.getClass().getName() + ".read() returned value out of range -1..255: " + b);
			return b;
	    }

	    protected void skipBytes(InputStream in, int n) throws IOException {
			while (n > 0) {
			    int len = in.read(tmpbuf, 0, n < tmpbuf.length ? n : tmpbuf.length);
			    if (len == -1) throw new EOFException();
			    n -= len;
			}
	    }
	}
	
	public class GzipOutputStream extends DefOutputStream {
	    protected final CRC32 crc = new CRC32();

	    public GzipOutputStream(OutputStream out, Deflater def, Integer size) throws IOException {
	    	super(out, def, size != null && size > 0 ? size : 512);
	    	writeHeader();
	    }

	    public synchronized void write(byte[] buf, int off, int len) throws IOException {
			super.write(buf, off, len);
			crc.update(buf, off, len);
	    }

	    public void finish() throws IOException {
			if (!def.finished()) {
			    def.finish();
			    while (!def.finished()) {
	                int len = def.deflate(buf, 0, buf.length);
	                if (def.finished() && len <= buf.length - TRAILER_SIZE) {
	                    writeTrailer(buf, len);
	                    len = len + TRAILER_SIZE;
	                    out.write(buf, 0, len);
	                    return;
	                }
	                if (len > 0) out.write(buf, 0, len);
			    }
		        byte[] trailer = new byte[TRAILER_SIZE];
			    writeTrailer(trailer, 0);
		        out.write(trailer);
			}
	    }

	    protected void writeHeader() throws IOException {
	        out.write(header);
	    }

	    protected void writeTrailer(byte[] buf, int offset) throws IOException {
	        writeInt((int)crc.getValue(), buf, offset); 
	        writeInt(def.getTotalIn(), buf, offset + 4); 
	    }

	    protected void writeInt(int i, byte[] buf, int offset) throws IOException {
	        writeShort(i & 0xffff, buf, offset);
	        writeShort((i >> 16) & 0xffff, buf, offset + 2);
	    }

	    protected void writeShort(int s, byte[] buf, int offset) throws IOException {
	        buf[offset] = (byte)(s & 0xff);
	        buf[offset + 1] = (byte)((s >> 8) & 0xff);
	    }
	}
	
	public static class ZipOutputStream extends DeflaterOutputStream implements Comparator<Object[]> {
	    protected static class XEntry {
			public final ZipEntry entry;
			public final long offset;
			public final int flag;
			public XEntry(ZipEntry entry, long offset) {
			    this.entry = entry;
			    this.offset = offset;
			    this.flag = (entry.getMethod() == DEFLATED && (entry.getSize()  == -1 || entry.getCompressedSize() == -1 || entry.getCrc() == -1)) ? 8 : 0;
			}
	    }
	    protected XEntry current;
	    protected Vector<XEntry> xentries = new Vector<XEntry>();
	    protected HashSet<String> names = new HashSet<String>();
	    protected CRC32 crc = new CRC32();
	    protected long written = 0;
	    protected long locoff = 0;
	    protected String comment;
	    protected int method = DEFLATED;
	    protected boolean finished;
	    protected boolean closed = false;
	    protected FlaterStreamPool handler;

	    protected void ensureOpen() throws IOException {
	    	if (closed) throw new IOException("Stream closed");
	    }

	    public ZipOutputStream(OutputStream out) {
	    	super(out == null ? null : out, out == null ? null : new Deflater(Deflater.DEFAULT_COMPRESSION, true), 512);
	    }

	    public ZipOutputStream(OutputStream out, FlaterStreamPool handler, Integer size) {
	    	super(out == null ? null :out, out == null ? null : handler == null ? new Deflater(Deflater.DEFAULT_COMPRESSION, true) : handler.getDeflater(), size != null && size > 0 ? size : 512);
	    	this.handler = handler;
	    }
	    
	    public void setComment(String comment) {
	        if (comment != null && comment.length() > 0xffff/3 && getUTF8Length(comment) > 0xffff) throw new IllegalArgumentException("ZIP file comment too long.");
	        this.comment = comment;
	    }

	    public void setMethod(int method) {
	    	if (method != DEFLATED && method != STORED) throw new IllegalArgumentException("invalid compression method");
	    	this.method = method;
	    }
	    
	    public void setLevel(int level) {
	    	def.setLevel(level);
	    }

	    public void putNextEntry(ZipEntry e) throws IOException {
			ensureOpen();
			if (current != null) closeEntry();
			if (e.getTime() == -1) e.setTime(System.currentTimeMillis());
			if (e.getMethod() == -1) e.setMethod(method);
			switch (e.getMethod()) {
				case STORED: if (e.getSize() == -1) e.setSize(e.getCompressedSize());
				    else if (e.getCompressedSize() == -1) e.setCompressedSize(e.getSize());
				    else if (e.getSize() != e.getCompressedSize()) throw new ZipException("STORED entry where compressed != uncompressed size");
				    if (e.getSize() == -1 || e.getCrc() == -1) throw new ZipException("STORED entry missing size, compressed size, or crc-32");
				case DEFLATED: break;
				default: throw new ZipException("unsupported compression method");
			}
			if (! names.add(e.getName())) throw new ZipException("duplicate entry: " + e.getName());
			current = new XEntry(e, written);
			xentries.add(current);
		    writeLOC(current);
	    }

	    public void closeEntry() throws IOException {
			ensureOpen();
			if (current != null) {
			    ZipEntry e = current.entry;
			    switch (e.getMethod()) {
				    case DEFLATED: def.finish();
						while (!def.finished()) deflate();
						if ((current.flag & 8) == 0) {
						    if (e.getSize() != def.getBytesRead()) throw new ZipException("invalid entry size (expected " + e.getSize() + " but got " + def.getBytesRead() + " bytes)");
						    if (e.getCompressedSize() != def.getBytesWritten()) throw new ZipException("invalid entry compressed size (expected " + e.getCompressedSize() + " but got " + def.getBytesWritten() + " bytes)");
						    if (e.getCrc() != crc.getValue()) throw new ZipException("invalid entry CRC-32 (expected 0x" + Long.toHexString(e.getCrc()) + " but got 0x" + Long.toHexString(crc.getValue()) + ")");
						} else {
						    e.setSize(def.getBytesRead());
						    e.setCompressedSize(def.getBytesWritten());
						    e.setCrc(crc.getValue());
						    writeEXT(e);
						}
						def.reset();
						written += e.getCompressedSize();
						break;
				    case STORED: if (e.getSize() != written - locoff) throw new ZipException("invalid entry size (expected " + e.getSize() + " but got " + (written - locoff) + " bytes)");
						if (e.getCrc() != crc.getValue()) throw new ZipException("invalid entry crc-32 (expected 0x" + Long.toHexString(e.getCrc()) + " but got 0x" + Long.toHexString(crc.getValue()) + ")");
						break;
				    default: throw new ZipException("invalid compression method");
			    }
			    crc.reset();
			    current = null;
			}
	    }

	    public synchronized void write(byte[] b, int off, int len) throws IOException {
			ensureOpen();
		    if (off < 0 || len < 0 || off > b.length - len) throw new IndexOutOfBoundsException();
			else if (len == 0) return;
			if (current == null) throw new ZipException("no current ZIP entry");
			ZipEntry entry = current.entry;
			switch (entry.getMethod()) {
				case DEFLATED: super.write(b, off, len);
				  	break;
				case STORED: written += len;
				    if (written - locoff > entry.getSize()) throw new ZipException("attempt to write past end of STORED entry");
				    out.write(b, off, len);
				    break;
				default: throw new ZipException("invalid compression method");
			}
			crc.update(b, off, len);
	    }

	    public void finish() throws IOException {
			ensureOpen();
			if (finished) return;
			if (current != null) closeEntry();
			if (xentries.size() < 1) throw new ZipException("ZIP file must have at least one entry");
			long off = written;
			for (XEntry xentry : xentries) writeCEN(xentry);
			writeEND(off, written - off);
			finished = true;
	    }

	    public void close() throws IOException {
	        if (!closed) try {
	            super.close();
	            closed = true;
	        } finally {
	        	if (handler == null) def.end();
	        	else handler.setDeflater(def);
	        }
	    }

	    protected void writeLOC(XEntry xentry) throws IOException {
			ZipEntry e = xentry.entry;
			int flag = xentry.flag;
			writeInt(LOCSIG);	    
			writeShort(version(e));     
			writeShort(flag);           
			writeShort(e.getMethod());       
			writeInt(e.getTime());           
			if ((flag & 8) == 8) {
			    writeInt(0);
			    writeInt(0);
			    writeInt(0);
			} else {
			    writeInt(e.getCrc());        
			    writeInt(e.getCompressedSize());      
			    writeInt(e.getSize());       
			}
			byte[] nameBytes = getUTF8Bytes(e.getName());
			writeShort(nameBytes.length);
			writeShort(e.getExtra() != null ? e.getExtra().length : 0);
			writeBytes(nameBytes, 0, nameBytes.length);
			if (e.getExtra() != null) writeBytes(e.getExtra(), 0, e.getExtra().length);
			locoff = written;
	    }

	    protected void writeEXT(ZipEntry e) throws IOException {
			writeInt(EXTSIG);	    
			writeInt(e.getCrc());	    
			writeInt(e.getCompressedSize());	    
			writeInt(e.getSize());	    
	    }

	    protected void writeCEN(XEntry xentry) throws IOException {
			ZipEntry e  = xentry.entry;
			int flag = xentry.flag;
			int version = version(e);
			writeInt(CENSIG);	    
			writeShort(version);	    
			writeShort(version);	    
			writeShort(flag);	    
			writeShort(e.getMethod());	    
			writeInt(e.getTime());	    
			writeInt(e.getCrc());	    
			writeInt(e.getCompressedSize());	    
			writeInt(e.getSize());	    
			byte[] nameBytes = getUTF8Bytes(e.getName());
			writeShort(nameBytes.length);
			writeShort(e.getExtra() != null ? e.getExtra().length : 0);
			byte[] commentBytes;
			if (e.getComment() != null) {
			    commentBytes = getUTF8Bytes(e.getComment());
			    writeShort(commentBytes.length);
			} else {
			    commentBytes = null;
			    writeShort(0);
			}
			writeShort(0);		    
			writeShort(0);		    
			writeInt(0);		    
			writeInt(xentry.offset);    
			writeBytes(nameBytes, 0, nameBytes.length);
			if (e.getExtra() != null) writeBytes(e.getExtra(), 0, e.getExtra().length);
			if (commentBytes != null) writeBytes(commentBytes, 0, commentBytes.length);
	    }

	    protected void writeEND(long off, long len) throws IOException {
			int count = xentries.size();
			writeInt(ENDSIG);	    
			writeShort(0);		    
			writeShort(0);		    
			writeShort(count);	    
			writeShort(count);	    
			writeInt(len);		    
			writeInt(off);		    
			if (comment != null) {	    
			    byte[] b = getUTF8Bytes(comment);
			    writeShort(b.length);
			    writeBytes(b, 0, b.length);
			} else writeShort(0);
	    }

	    protected void writeShort(int v) throws IOException {
			OutputStream out = this.out;
			out.write((v >>> 0) & 0xff);
			out.write((v >>> 8) & 0xff);
			written += 2;
	    }

	    protected void writeInt(long v) throws IOException {
			OutputStream out = this.out;
			out.write((int)((v >>>  0) & 0xff));
			out.write((int)((v >>>  8) & 0xff));
			out.write((int)((v >>> 16) & 0xff));
			out.write((int)((v >>> 24) & 0xff));
			written += 4;
	    }

	    protected void writeBytes(byte[] b, int off, int len) throws IOException {
			super.out.write(b, off, len);
			written += len;
	    }
	    
		@Override
		public int compare(Object[] ret, Object[] para) {
			if (para == null || para.length < 1) return -1;
			else try {
				if ("write".equals(para[0])) {
					if (para.length > 1) if (para[1] instanceof byte[]) write((byte[])para[1]);
					else if (para[1] instanceof Object[]) write((byte[])((Object[])para[1])[0], (Integer)((Object[])para[1])[1], (Integer)((Object[])para[1])[2]);
					else if (para[1] instanceof Integer) write((Integer)para[1]);
				} else if ("closeEntry".equals(para[0])) closeEntry();
				else if ("putNextEntry".equals(para[0])) {
					if (para.length > 1 && para[1] instanceof ZipEntry) putNextEntry((ZipEntry)para[1]);
				} else if ("setComment".equals(para[0])) {
					if (para.length > 1 && para[1] instanceof String) setComment((String)para[1]);
				} else if ("setMethod".equals(para[0])) {
					if (para.length > 1 && para[1] instanceof Integer) setMethod((Integer)para[1]);
				} else if ("setLevel".equals(para[0])) {
					if (para.length > 1 && para[1] instanceof Integer) setLevel((Integer)para[1]);
				} else if ("finish".equals(para[0])) finish();
				else if ("close".equals(para[0])) close();
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
			}
			if (ret != null && ret.length > 0) ret[0] = null;
			return 0;
		}
	}
	
	public static class JarEntry extends ZipEntry  implements Comparator<Object[]> {
		protected Attributes attr;
	    
	    public JarEntry(JarEntry je) {
			super(je);
			this.attr = je.attr;
		}
	    
		public JarEntry(String name) {
			super(name);
		}
		
		public JarEntry(ZipEntry ze) {
			super(ze);
		}

	    public Attributes getAttributes() {
	    	return attr;
	    }

		@Override
		public int compare(Object[] ret, Object[] para) {
			if (ret != null && ret.length > 0) ret[0] = attr;
			return 0;
		} 
	}
	
	public static class JarOutputStream extends ZipOutputStream {
	    protected boolean firstEntry = true;
	    
	    public JarOutputStream(OutputStream out, Manifest man) throws IOException {
			super(out);
			if (man == null) throw new NullPointerException("man");
			ZipEntry e = new ZipEntry(JarFile.MANIFEST_NAME);
			putNextEntry(e);
			man.write(new BufferedOutputStream(this));
			closeEntry();
	    }

	    public JarOutputStream(OutputStream out, FlaterStreamPool handler, Integer size, Manifest man) throws IOException {
			super(out, handler, size);
			if (man == null) throw new NullPointerException("man");
			ZipEntry e = new ZipEntry(JarFile.MANIFEST_NAME);
			putNextEntry(e);
			man.write(new BufferedOutputStream(this));
			closeEntry();
		}
	    
	    public JarOutputStream(OutputStream out, FlaterStreamPool handler, Integer size) {
			super(out, handler, size);
		}

		public JarOutputStream(OutputStream out) {
			super(out);
		}

		public void putNextEntry(ZipEntry ze) throws IOException {
			if (firstEntry) {
			    byte[] edata = ze.getExtra();
	            if (edata == null || !hasMagic(edata)) {
	                if (edata == null) edata = new byte[4];
	                else {
	                    byte[] tmp = new byte[edata.length + 4];
	                    System.arraycopy(edata, 0, tmp, 4, edata.length);
	                    edata = tmp;
	                }
	                set16(edata, 0, JAR_MAGIC); 
	                set16(edata, 2, 0);         
	                ze.setExtra(edata);
			    }
			    firstEntry = false;
			}
			super.putNextEntry(ze);
	    }
	}
	
	public static class JarInputStream extends ZipInputStream {
	    protected Manifest man;
	    protected JarEntry first;
	    protected boolean tryManifest;

	    public JarInputStream(InputStream in) throws IOException {
			super(in);
	        JarEntry e = (JarEntry) super.getNextEntry();
	        if (e != null && e.getName().equalsIgnoreCase("META-INF/")) e = (JarEntry)super.getNextEntry();
	        first = checkManifest(e);
	    }

	    public JarInputStream(InputStream in, FlaterStreamPool handler, Integer size) throws IOException {
			super(in, handler, size);
	        JarEntry e = (JarEntry) super.getNextEntry();
	        if (e != null && e.getName().equalsIgnoreCase("META-INF/")) e = (JarEntry)super.getNextEntry();
	        first = checkManifest(e);
		}

		protected JarEntry checkManifest(JarEntry e) throws IOException {
	        if (e != null && e.getName().equalsIgnoreCase("META-INF/")) e = (JarEntry)super.getNextEntry();
	        if (e != null && JarFile.MANIFEST_NAME.equalsIgnoreCase(e.getName())) {
	            man = new Manifest();
	            byte bytes[] = getBytes(new BufferedInputStream(this));
	            man.read(new ByteArrayInputStream(bytes));
	            closeEntry();
	            return (JarEntry)super.getNextEntry();
	        }
	        return e;
	    }

	    protected byte[] getBytes(InputStream is) throws IOException {
			byte[] buffer = new byte[512];
			ByteArrayOutputStream baos = new ByteArrayOutputStream(2048);
			int n;
			baos.reset();
			while ((n = is.read(buffer, 0, buffer.length)) != -1) baos.write(buffer, 0, n);
			return baos.toByteArray();
	    }

	    public Manifest getManifest() {
	    	return man;
	    }

	    public ZipEntry getNextEntry() throws IOException {
			JarEntry e;
			if (first == null) {
			    e = (JarEntry)super.getNextEntry();
	            if (tryManifest) {
	                e = checkManifest(e);
	                tryManifest = false;
	            }
			} else {
			    e = first;
			    if (first.getName().equalsIgnoreCase("META-INF/INDEX.LIST")) tryManifest = true;
			    first = null;
			}
			return e;
	    }

	    public JarEntry getNextJarEntry() throws IOException {
	    	return (JarEntry)getNextEntry();
	    }

	    public int read(byte[] b, int off, int len) throws IOException {
			int n;
			if (first == null) n = super.read(b, off, len);
			else n = -1;
			return n;
	    }

	    protected ZipEntry createZipEntry(String name) {
			JarEntry e = new JarEntry(name);
			if (man != null) e.attr = man.getAttributes(name);
			return e;
	    }
	    
		@Override
		public int compare(Object[] ret, Object[] para) {
			Object r = null;
			if (para == null || para.length < 1) return -1;
			else try {
				if ("read".equals(para[0])) {
					if (para.length > 1) {
						if (para[1] instanceof byte[]) r = read((byte[])para[1]);
						else if (para[1] instanceof Object[]) r = read((byte[])((Object[])para[1])[0], (Integer)((Object[])para[1])[1], (Integer)((Object[])para[1])[2]);
					}
				} else if ("closeEntry".equals(para[0])) closeEntry();
				else if ("getNextEntry".equals(para[0])) r = getNextEntry();
				else if ("getNextJarEntry".equals(para[0])) r = getNextJarEntry();
				else if ("getManifest".equals(para[0])) r = getManifest();
				else if ("available".equals(para[0])) r = available();
				else if ("close".equals(para[0])) close();
				else if ("markSupported".equals(para[0])) r= markSupported();
				else if ("reset".equals(para[0])) reset();
				else if ("mark".equals(para[0])) {
					if (para.length > 1 && para[1] instanceof Integer) mark((Integer)para[1]);
				} else if ("skip".equals(para[0]) && para.length > 1 && para[1] instanceof Long) r = skip((Long)para[1]);
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
			}
			if (ret != null && ret.length > 0) ret[0] = r;
			return 0;
		}
	}
	
	public static class Manifestor implements IProcessor<JarInputStream, Manifest> {
		protected Boolean recycle = false;
		
		public void setRecycle(Boolean recycle) {
			if (recycle != null) this.recycle = recycle;
		}

		@Override
		public Manifest process(JarInputStream instance) {
			if (recycle) instance.release();
			return instance.man;
		}
	}
	
	public static class ZipInputStream extends InflaterInputStream implements Comparator<Object[]> {
		protected static final String fileEncoding = System.getProperty("sun.zip.encoding");
		protected static final String altEncoding = System.getProperty("sun.zip.altEncoding");
		protected ZipEntry entry;
		protected int flag;
		protected CRC32 crc = new CRC32();
		protected long remaining;
		protected byte[] tmpbuf = new byte[512];
		protected byte[] b = new byte[256];
		protected boolean closed = false;
		protected boolean entryEOF = false;
	    protected FlaterStreamPool handler;

		protected void ensureOpen() throws IOException {
	    	if (closed) throw new IOException("Stream closed");
	    }

	    public ZipInputStream(InputStream in) {
			super(in == null ? null : new PushbackInputStream(in, 512), in == null ? null : new Inflater(true), 512);
	    }

	    public ZipInputStream(InputStream in, FlaterStreamPool handler, Integer size) {
			super(in == null ? null : new PushbackInputStream(in, size != null && size > 0 ? size : 512), in == null ? null : handler == null ? new Inflater(true) : handler.getInflater(), size != null && size > 0 ? size : 512);
			this.handler = handler;
	    }

	    public ZipEntry getNextEntry() throws IOException {
	        ensureOpen();
			if (entry != null) closeEntry();
			crc.reset();
			inf.reset();
			if ((entry = readLOC()) == null) return null;
			if (entry.getMethod() == STORED) remaining = entry.getSize();
		    entryEOF = false;
			return entry;
	    }

	    public void closeEntry() throws IOException {
	        ensureOpen();
	        while (read(tmpbuf, 0, tmpbuf.length) != -1) ;
	        entryEOF = true;
	    }

	    public int available() throws IOException {
	        ensureOpen();
	        if (entryEOF) return 0;
	        else return 1;
	    }

	    public int read(byte[] b, int off, int len) throws IOException {
	        ensureOpen();
	        if (off < 0 || len < 0 || off > b.length - len) throw new IndexOutOfBoundsException();
	        else if (len == 0) return 0;
	        if (entry == null) return -1;
			switch (entry.getMethod()) {
				case DEFLATED: len = super.read(b, off, len);
				    if (len == -1) {
						readEnd(entry); 
						entryEOF = true;
						entry = null;
				    } else crc.update(b, off, len);
				    return len;
				case STORED: if (remaining <= 0) {
				        entryEOF = true;
						entry = null;
						return -1;
				    }
				    if (len > remaining) len = (int)remaining;
				    len = in.read(b, off, len);
				    if (len == -1) throw new ZipException("unexpected EOF");
				    crc.update(b, off, len);
				    remaining -= len;
				    if (remaining == 0 && entry.getCrc() != crc.getValue()) throw new ZipException("invalid entry CRC (expected 0x" + Long.toHexString(entry.getCrc()) + " but got 0x" + Long.toHexString(crc.getValue()) + ")");
				    return len;
				default: throw new ZipException("invalid compression method");
			}
	    }

	    public long skip(long n) throws IOException {
	        if (n < 0) throw new IllegalArgumentException("negative skip length");
	        ensureOpen();
			int max = (int)Math.min(n, Integer.MAX_VALUE);
			int total = 0;
			while (total < max) {
			    int len = max - total;
			    if (len > tmpbuf.length) len = tmpbuf.length;
			    len = read(tmpbuf, 0, len);
			    if (len == -1) {
			    	entryEOF = true;
			    	break;
			    }
			    total += len;
			}
			return total;
	    }
	    
	    protected void release() {
        	if (handler == null) inf.end();
        	else handler.setInflater(inf);
	    }
	    
	    public void close() throws IOException {
	        if (!closed) try {
	        	super.close();
	            closed = true;
	        } finally {
	        	if (handler == null) inf.end();
	        	else handler.setInflater(inf);
	        }
	    }

	    protected ZipEntry readLOC() throws IOException {
			try {
			    readFully(tmpbuf, 0, LOCHDR);
			} catch (EOFException e) {
			    return null;
			}
			if (get32(tmpbuf, 0) != LOCSIG) return null;
			int len = get16(tmpbuf, LOCNAM);
	        int blen = b.length;
	        if (len > blen) {
	            do blen = blen * 2;
	            while (len > blen);
	            b = new byte[blen];
	        }
			readFully(b, 0, len);
			String name = getFileName(b, len);
			ZipEntry e = createZipEntry(name);
			flag = get16(tmpbuf, LOCFLG);
			if ((flag & 1) == 1) throw new ZipException("encrypted ZIP entry not supported");
			e.setMethod(get16(tmpbuf, LOCHOW));
			e.setTime(get32(tmpbuf, LOCTIM));
			if ((flag & 8) == 8) {
			    if (e.getMethod() != DEFLATED) throw new ZipException("only DEFLATED entries can have EXT descriptor");
			} else {
			    e.setCrc(get32(tmpbuf, LOCCRC));
			    e.setCompressedSize(get32(tmpbuf, LOCSIZ));
			    e.setSize(get32(tmpbuf, LOCLEN));
			}
			len = get16(tmpbuf, LOCEXT);
			if (len > 0) {
			    byte[] bb = new byte[len];
			    readFully(bb, 0, len);
			    e.setExtra(bb);
			}
			return e;
	    }

	    protected ZipEntry createZipEntry(String name) {
	    	return new ZipEntry(name);
	    }

	    protected void readEnd(ZipEntry e) throws IOException {
			int n = inf.getRemaining();
			if (n > 0) ((PushbackInputStream)in).unread(buf, len - n, n);
			if ((flag & 8) == 8) {
			    readFully(tmpbuf, 0, EXTHDR);
			    long sig = get32(tmpbuf, 0);
	            if (sig != EXTSIG) { 
	                e.setCrc(sig);
	                e.setCompressedSize(get32(tmpbuf, EXTSIZ - EXTCRC));
	                e.setSize(get32(tmpbuf, EXTLEN - EXTCRC));
	                ((PushbackInputStream)in).unread(tmpbuf, EXTHDR - EXTCRC - 1, EXTCRC);
	            } else {
	                e.setCrc(get32(tmpbuf, EXTCRC));
	                e.setCompressedSize(get32(tmpbuf, EXTSIZ));
	                e.setSize(get32(tmpbuf, EXTLEN));
	            }
			}
			if (e.getSize() != inf.getBytesWritten()) throw new ZipException("invalid entry size (expected " + e.getSize() + " but got " + inf.getBytesWritten() + " bytes)");
			if (e.getCompressedSize() != inf.getBytesRead()) throw new ZipException("invalid entry compressed size (expected " + e.getCompressedSize() + " but got " + inf.getBytesRead() + " bytes)");
			if (e.getCrc() != crc.getValue()) throw new ZipException("invalid entry CRC (expected 0x" + Long.toHexString(e.getCrc()) + " but got 0x" + Long.toHexString(crc.getValue()) + ")");
	    }
	    
		protected void readFully(byte[] b, int off, int len) throws IOException {
			while (len > 0) {
			    int n = in.read(b, off, len);
			    if (n == -1) throw new EOFException();
			    off += n;
			    len -= n;
			}
	    }

		protected String getFileName(byte[] b, int len) throws IOException {
			String name;
			try {
			    if (fileEncoding == null || fileEncoding.equals("") || this instanceof JarInputStream) name = getUTF8String(b, 0, len);
			    else if (fileEncoding.equalsIgnoreCase("default")) name = new String(b, 0, len);
				else try {
			    	name = new String(b, 0, len, fileEncoding);
		  	    } catch (UnsupportedEncodingException UEE) {
		  	    	throw new ZipException("Unable to encode entry " + "name (sun.zip.encoding is " + fileEncoding + ")");
			    }
			} catch (IllegalArgumentException e) {
			    if (altEncoding == null || altEncoding.equals("") || this instanceof JarInputStream) throw e;
			    if (altEncoding.equalsIgnoreCase("default")) name = new String(b, 0, len);
			    else try {
				    name = new String(b, 0, len, altEncoding);
				} catch (UnsupportedEncodingException UEE) {
				    throw new ZipException("Unable to encode entry " + "name (sun.zip.altEncoding is " + altEncoding + ")");
				}
			}
			return name;
	    } 
		
		@Override
		public int compare(Object[] ret, Object[] para) {
			Object r = null;
			if (para == null || para.length < 1) return -1;
			else try {
				if ("read".equals(para[0])) {
					if (para.length > 1) {
						if (para[1] instanceof byte[]) r = read((byte[])para[1]);
						else if (para[1] instanceof Object[]) r = read((byte[])((Object[])para[1])[0], (Integer)((Object[])para[1])[1], (Integer)((Object[])para[1])[2]);
					}
				} else if ("closeEntry".equals(para[0])) closeEntry();
				else if ("getNextEntry".equals(para[0])) r = getNextEntry();
				else if ("available".equals(para[0])) r = available();
				else if ("close".equals(para[0])) close();
				else if ("markSupported".equals(para[0])) r= markSupported();
				else if ("reset".equals(para[0])) reset();
				else if ("mark".equals(para[0])) {
					if (para.length > 1 && para[1] instanceof Integer) mark((Integer)para[1]);
				} else if ("skip".equals(para[0]) && para.length > 1 && para[1] instanceof Long) r = skip((Long)para[1]);
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
			}
			if (ret != null && ret.length > 0) ret[0] = r;
			return 0;
		}
	}
	
    protected static int get16(byte[] b, int off) {
    	return (b[off] & 0xff) | ((b[off+1] & 0xff) << 8);
    }

	protected static long get32(byte b[], int off) {
		return get16(b, off) | ((long)get16(b, off+2) << 16);
    }
    
    protected static void set16(byte[] b, int off, int value) {
		b[off+0] = (byte)value;
		b[off+1] = (byte)(value >> 8);
    }
	
    protected static int getUTF8Length(String s) {
        int count = 0;
        for (int i = 0; i < s.length(); i++) {
            char ch = s.charAt(i);
            if (ch <= 0x7f) count++;
            else if (ch <= 0x7ff) count += 2;
            else count += 3;
        }
        return count;
    }

    protected static byte[] getUTF8Bytes(String s) {
		char[] c = s.toCharArray();
		int len = c.length;
		int count = 0;
		for (int i = 0; i < len; i++) {
		    int ch = c[i];
		    if (ch <= 0x7f) count++;
		    else if (ch <= 0x7ff) count += 2;
		    else count += 3;
		}
		byte[] b = new byte[count];
		int off = 0;
		for (int i = 0; i < len; i++) {
		    int ch = c[i];
		    if (ch <= 0x7f) b[off++] = (byte)ch;
		    else if (ch <= 0x7ff) {
				b[off++] = (byte)((ch >> 6) | 0xc0);
				b[off++] = (byte)((ch & 0x3f) | 0x80);
		    } else {
				b[off++] = (byte)((ch >> 12) | 0xe0);
				b[off++] = (byte)(((ch >> 6) & 0x3f) | 0x80);
				b[off++] = (byte)((ch & 0x3f) | 0x80);
		    }
		}
		return b;
    }
	
    protected static boolean hasMagic(byte[] edata) {
		try {
		    int i = 0;
		    while (i < edata.length) {
				if (get16(edata, i) == JAR_MAGIC) return true;
				i += get16(edata, i + 2) + 4;
		    }
		} catch (ArrayIndexOutOfBoundsException e) {}
		return false;
    }
    
    protected static String getUTF8String(byte[] b, int off, int len) {
		int count = 0;
		int max = off + len;
		int i = off;
		while (i < max) {
		    int c = b[i++] & 0xff;
		    switch (c >> 4) {
			    case 0: case 1: case 2: case 3: case 4: case 5: case 6: case 7: count++;
					break;
			    case 12: case 13: if ((int)(b[i++] & 0xc0) != 0x80) throw new IllegalArgumentException();
					count++;
					break;
			    case 14: if (((int)(b[i++] & 0xc0) != 0x80) || ((int)(b[i++] & 0xc0) != 0x80)) throw new IllegalArgumentException();
					count++;
					break;
			    default: throw new IllegalArgumentException();
		    }
		}
		if (i != max) throw new IllegalArgumentException();
		char[] cs = new char[count];
		i = 0;
		while (off < max) {
		    int c = b[off++] & 0xff;
		    switch (c >> 4) {
			    case 0: case 1: case 2: case 3: case 4: case 5: case 6: case 7: cs[i++] = (char)c;
					break;
			    case 12: case 13: cs[i++] = (char)(((c & 0x1f) << 6) | (b[off++] & 0x3f));
					break;
			    case 14: int t = (b[off++] & 0x3f) << 6;
					cs[i++] = (char)(((c & 0x0f) << 12) | t | (b[off++] & 0x3f));
					break;
			    default: throw new IllegalArgumentException();
		    }
		}
		return new String(cs, 0, count);
    }
    
    protected static int version(ZipEntry e) throws ZipException {
		switch (e.getMethod()) {
			case DEFLATED: return 20;
			case STORED:   return 10;
			default: throw new ZipException("unsupported compression method");
		}
    }
}
