package net.yeeyaa.eight.access.servlet;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;

import javax.servlet.ServletInputStream;
import javax.servlet.ServletRequest;
import javax.servlet.ServletRequestWrapper;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;

import net.yeeyaa.eight.IProcessor;


public class RequestWrapper extends ServletRequestWrapper{
	protected final IProcessor<ServletInputStream, ServletInputStream> stream;
	protected final IProcessor<BufferedReader, BufferedReader> reader;
	
	public RequestWrapper(ServletRequest request, IProcessor<ServletInputStream, ServletInputStream> stream, IProcessor<BufferedReader, BufferedReader> reader) {
		super(request);
		this.stream = stream;
		this.reader = reader;
	}

	public ServletInputStream getInputStream() throws IOException {
		if (stream == null) return super.getInputStream();
		else return stream.process(super.getInputStream());
	}

	public BufferedReader getReader() throws IOException {
		if (reader == null) return super.getReader();
		else return reader.process(super.getReader());
	}

	public static class HttpRequestWrapper extends HttpServletRequestWrapper{
		protected final IProcessor<ServletInputStream, ServletInputStream> stream;
		protected final IProcessor<BufferedReader, BufferedReader> reader;
		
		public HttpRequestWrapper(HttpServletRequest request, IProcessor<ServletInputStream, ServletInputStream> stream, IProcessor<BufferedReader, BufferedReader> reader) {
			super(request);
			this.stream = stream;
			this.reader = reader;
		}

		public ServletInputStream getInputStream() throws IOException {
			if (stream == null) return super.getInputStream();
			else return stream.process(super.getInputStream());
		}

		public BufferedReader getReader() throws IOException {
			if (reader == null) return super.getReader();
			else return reader.process(super.getReader());
		}
	}
	
	public static abstract class ServletInputStreamWrapper extends ServletInputStream {
		protected long count;
		protected long mark;
		protected final long max;
		protected final ServletInputStream input;
		
		public ServletInputStreamWrapper(ServletInputStream input, long max) {
			this.input = input;
			this.max = max;
		}

		public int available() throws IOException {
			return input.available();
		}

		public void close() throws IOException {
			input.close();
		}

		public boolean markSupported() {
			return input.markSupported();
		}

		@Override
		public int readLine(byte[] b, int off, int len) throws IOException {
			int ret = input.readLine(b, off, len);
			if (max >= 0) {
				if (ret > 0) count += ret;
				if (count > max) throw new IOException("Data overflow. The maxLength is " + max);
			}
			return ret;
		}

		@Override
		public long skip(long n) throws IOException {
			return input.skip(n);
		}

		@Override
		public int read(byte[] b) throws IOException {
			int ret = input.read(b);
			if (max >= 0) {
				if (ret > 0) count += ret;
				if (count > max) throw new IOException("Data overflow. The maxLength is " + max);
			}
			return ret;
		}

		@Override
		public int read(byte[] b, int off, int len) throws IOException {
			int ret = input.read(b, off, len);
			if (max >= 0) {
				if (ret > 0) count += ret;
				if (count > max) throw new IOException("Data overflow. The maxLength is " + max);
			}
			return ret;
		}

		@Override
		public synchronized void mark(int readlimit) {
			if (input.markSupported()) {
				input.mark(readlimit);
				mark = count;
			}
		}

		@Override
		public synchronized void reset() throws IOException {
			if (input.markSupported()) {
				input.reset();
				count = mark;
			}
		}

		@Override
		public int read() throws IOException {
			int ret = input.read();
			if (max >= 0) {
				if (ret > 0) count ++;
				if (count > max) throw new IOException("Data overflow. The maxLength is " + max);
			}
			return ret;
		}
	}
	
	public static class BufferedReaderWrapper extends BufferedReader {
		public BufferedReaderWrapper(Reader in, int sz, long max) {
			super(max < 0 ? in : new ReaderWrapper(in, max), sz);
		}

		public BufferedReaderWrapper(Reader in, long max) {
			super(max < 0 ? in : new ReaderWrapper(in, max));
		}
	}
	
	public static class ReaderWrapper extends Reader {
		protected long count;
		protected long mark;
		protected final long max;
		protected final Reader reader;
		
		public ReaderWrapper(Reader reader, long max) {
			this.max = max;
			this.reader = reader;
		}

		public boolean ready() throws IOException {
			return reader.ready();
		}

		public boolean markSupported() {
			return reader.markSupported();
		}

		public void close() throws IOException {
			reader.close();
		}

		@Override
		public long skip(long n) throws IOException {
			return reader.skip(n);
		}

		@Override
		public void mark(int readAheadLimit) throws IOException {
			if (reader.markSupported()) {
				reader.mark(readAheadLimit);
				mark = count;
			}
		}

		@Override
		public void reset() throws IOException {
			if(reader.markSupported()) {
				reader.reset();
				count = mark;
			}
		}

		@Override
		public int read(char[] cbuf, int off, int len)	throws IOException {
			int ret = reader.read(cbuf, off, len);
			if (max >= 0) {
				if (ret > 0) count += ret;
				if (count > max) throw new IOException("Data overflow. The maxLength is " + max);
			}
			return ret;
		}
	}
	
	public static class InputWrapper extends InputStream{
		protected long count;
		protected long mark;
		protected final long max;
		protected final InputStream input;
		
		public InputWrapper(InputStream input, long max) {
			this.input = input;
			this.max = max;
		}

		public int available() throws IOException {
			return input.available();
		}

		public void close() throws IOException {
			input.close();
		}

		public boolean markSupported() {
			return input.markSupported();
		}

		@Override
		public long skip(long n) throws IOException {
			return input.skip(n);
		}

		@Override
		public int read(byte[] b) throws IOException {
			int ret = input.read(b);
			if (max >= 0) {
				if (ret > 0) count += ret;
				if (count > max) throw new IOException("Data overflow. The maxLength is " + max);
			}
			return ret;
		}

		@Override
		public int read(byte[] b, int off, int len) throws IOException {
			int ret = input.read(b, off, len);
			if (max >= 0) {
				if (ret > 0) count += ret;
				if (count > max) throw new IOException("Data overflow. The maxLength is " + max);
			}
			return ret;
		}

		@Override
		public synchronized void mark(int readlimit) {
			if (input.markSupported()) {
				input.mark(readlimit);
				mark = count;
			}
		}

		@Override
		public synchronized void reset() throws IOException {
			if (input.markSupported()) {
				input.reset();
				count = mark;
			}
		}

		@Override
		public int read() throws IOException {
			int ret = input.read();
			if (max >= 0) {
				if (ret > 0) count ++;
				if (count > max) throw new IOException("Data overflow. The maxLength is " + max);
			}
			return ret;
		}
	}
}
