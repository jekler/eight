package net.yeeyaa.eight.access.servlet;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.Writer;

import javax.servlet.ServletOutputStream;
import javax.servlet.ServletResponse;
import javax.servlet.ServletResponseWrapper;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletResponseWrapper;

import net.yeeyaa.eight.IProcessor;


public class ResponseWrapper extends ServletResponseWrapper{
	protected final IProcessor<ServletOutputStream, ServletOutputStream> stream;
	protected final IProcessor<PrintWriter, PrintWriter> writer;
	
	public ResponseWrapper(ServletResponse response, IProcessor<ServletOutputStream, ServletOutputStream> stream, IProcessor<PrintWriter, PrintWriter> writer) {
		super(response);
		this.stream = stream;
		this.writer = writer;
	}

	public ServletOutputStream getOutputStream() throws IOException {
		if (stream == null) return super.getOutputStream();
		else return stream.process(super.getOutputStream());
	}

	public PrintWriter getWriter() throws IOException {
		if (writer == null) return super.getWriter();
		else return writer.process(super.getWriter());
	}

	public static class HttpResponseWrapper extends HttpServletResponseWrapper{
		protected final IProcessor<ServletOutputStream, ServletOutputStream> stream;
		protected final IProcessor<PrintWriter, PrintWriter> writer;
		
		public HttpResponseWrapper(HttpServletResponse response, IProcessor<ServletOutputStream, ServletOutputStream> stream, IProcessor<PrintWriter, PrintWriter> writer) {
			super(response);
			this.stream = stream;
			this.writer = writer;
		}

		public ServletOutputStream getOutputStream() throws IOException {
			if (stream == null) return super.getOutputStream();
			else return stream.process(super.getOutputStream());
		}

		public PrintWriter getWriter() throws IOException {
			if (writer == null) return super.getWriter();
			else return writer.process(super.getWriter());
		}
	}
	
	public abstract static class ServletOutputStreamWrapper extends ServletOutputStream {
		protected long count;
		protected long mark;
		protected final long max;
		protected final ServletOutputStream output;
		
		public ServletOutputStreamWrapper(ServletOutputStream output, long max) {
			this.output = output;
			this.max = max;
		}

		@Override
		public void write(int b) throws IOException {
			if (max >= 0) {
				long c = count + 1;
				if (c > max) throw new IOException("Data overflow. The maxLength is " + max);
				output.write(b);
				count = c;
			} else output.write(b);
		}

		@Override
		public void write(byte[] b) throws IOException {
			if (max >= 0) {
				long c = count + b.length;
				if (count > max) throw new IOException("Data overflow. The maxLength is " + max);
				output.write(b);
				count = c;
			} else output.write(b);
		}

		@Override
		public void write(byte[] b, int off, int len) throws IOException {
			if (max >= 0) {
				long c = count + len;
				if (c > max) throw new IOException("Data overflow. The maxLength is " + max);
				output.write(b, off, len);
				count = c;
			} else output.write(b, off, len);
		}

		@Override
		public void flush() throws IOException {
			output.flush();
		}
		
		public void close() throws IOException {
			output.close();
		}
	}
	
	public static class PrintWriterWrapper extends PrintWriter {
		public PrintWriterWrapper(Writer in, boolean sz, long max) {
			super(max < 0 ? in : new WriterWrapper(in, max), sz);
		}

		public PrintWriterWrapper(OutputStream out, boolean autoFlush, long max) {
			super(max < 0 ? out : new OutputWrapper(out, max), autoFlush);
		}

		public PrintWriterWrapper(OutputStream out, long max) {
			super(max < 0 ? out : new OutputWrapper(out, max));
		}

		public PrintWriterWrapper(Writer in, long max) {
			super(max < 0 ? in : new WriterWrapper(in, max));
		}
	}
	
	public static class WriterWrapper extends Writer {
		protected long count;
		protected long mark;
		protected final long max;
		protected final Writer writer;
		
		public WriterWrapper(Writer writer, long max) {
			this.max = max;
			this.writer = writer;
		}

		@Override
		public void write(char[] cbuf, int off, int len) throws IOException {
			if (max >= 0) {
				long b = count + len;
				if (b > max) throw new IOException("Data overflow. The maxLength is " + max);
				writer.write(cbuf, off, len);
				count = b;
			} else writer.write(cbuf, off, len);
		}

		@Override
		public void flush() throws IOException {
			writer.flush();
		}

		public void close() throws IOException {
			writer.close();
		}
	}
	
	public static class OutputWrapper extends OutputStream{
		protected long count;
		protected long mark;
		protected final long max;
		protected final OutputStream output;
		
		public OutputWrapper(OutputStream output, long max) {
			this.output = output;
			this.max = max;
		}

		@Override
		public void write(byte[] b) throws IOException {
			if (max >= 0) {
				long c = count + b.length;
				if (c > max) throw new IOException("Data overflow. The maxLength is " + max);
				output.write(b);
				count = c;
			} else output.write(b);
		}

		@Override
		public void write(byte[] b, int off, int len) throws IOException {
			if (max >= 0) {
				long c = count + len;
				if (c > max) throw new IOException("Data overflow. The maxLength is " + max);
				output.write(b, off, len);
				count = c;
			} else output.write(b, off, len);
		}

		@Override
		public void write(int b) throws IOException {
			if (max >= 0) {
				long c = count + 1;
				if (c > max) throw new IOException("Data overflow. The maxLength is " + max);
				output.write(b);
				count = c;
			} else output.write(b);
		}

		@Override
		public void flush() throws IOException {
			output.flush();
		}
		
		@Override
		public void close() throws IOException {
			output.close();
		}
	}
}
