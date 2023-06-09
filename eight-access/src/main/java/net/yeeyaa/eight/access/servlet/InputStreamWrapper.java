package net.yeeyaa.eight.access.servlet;

import javax.servlet.ReadListener;
import javax.servlet.ServletInputStream;
import javax.servlet.ServletOutputStream;
import javax.servlet.WriteListener;

import net.yeeyaa.eight.access.servlet.RequestWrapper.ServletInputStreamWrapper;
import net.yeeyaa.eight.access.servlet.ResponseWrapper.ServletOutputStreamWrapper;


public class InputStreamWrapper  extends ServletInputStreamWrapper {
	public InputStreamWrapper(ServletInputStream input, long max) {
		super(input, max);
	}

	public boolean isFinished() {
		return input.isFinished();
	}

	public boolean isReady() {
		return input.isReady();
	}

	public void setReadListener(ReadListener readListener) {
		input.setReadListener(readListener);
	}
	
	public static class OutputStreamWrapper extends ServletOutputStreamWrapper {
		public OutputStreamWrapper(ServletOutputStream output, long max) {
			super(output, max);
		}

		public boolean isReady() {
			return output.isReady();
		}

		public void setWriteListener(WriteListener writeListener) {
			output.setWriteListener(writeListener);
		}
	}
}
