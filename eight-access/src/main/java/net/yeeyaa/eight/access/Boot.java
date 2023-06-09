package net.yeeyaa.eight.access;

import java.util.HashSet;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.common.spring.RestartProcessor;

import org.springframework.context.support.ClassPathXmlApplicationContext;


public class Boot{
	public static void main(String[] args) {
		String begin = null;
		String close = null;
		String context = null;
		HashSet<String> set = new HashSet<String>();
		for(String arg : args) set.add(arg);
		for(String arg : args) if(arg.indexOf("begin:") == 0){
			String[] split = arg.split(":");
			if(split.length > 1) begin = split[1]; 
			set.remove(arg);
		}else if(arg.indexOf("close:") == 0){
			String[] split = arg.split(":");
			if(split.length > 1) close = split[1]; 
			set.remove(arg);
		}else if(arg.indexOf("context:") == 0){
			String[] split = arg.split(":");
			if(split.length > 1) context = split[1]; 
			set.remove(arg);
		}
		args = set.toArray(new String[set.size()]);
		ClassPathXmlApplicationContext c = new ClassPathXmlApplicationContext(args);
		c.start();
		if(context != null) {
			Object b = c.getBean(context);
			if(b instanceof RestartProcessor) {
				RestartProcessor restart = (RestartProcessor) b;
				restart.setArgs(args);
				restart.setBegin(begin);
				restart.setClose(close);
				restart.setContext(c);
				restart.setSelf(context);
			}
		}
		if(begin != null) {
			Object b = c.getBean(begin);
			if(b instanceof IProcessor) ((IProcessor<Object, Object>)b).process(null);
		}
	}
}
