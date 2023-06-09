package net.yeeyaa.eight.core.pipeline.base;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.TypeConvertor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class SyncIf implements IProcessor<Map<String, String>, Void> {
	protected final Logger log;
	protected String regex="\\|";
	protected IProcessor<Object, Object> beanHolder;
	
	public SyncIf() {
		this.log = LoggerFactory.getLogger(SyncIf.class);
	}

	public SyncIf(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(SyncIf.class) : log;
	}
	
	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}
	
	public void setRegex(String regex) {
		if(regex != null) this.regex = regex;
	}
	
	@Override
	public Void process(Map<String, String> paras) {
		try{
			Object groupid = TypeConvertor.getParas(beanHolder, paras, "groupid", regex).toString();
			if(groupid != null){
				String[] ifset = paras.get("if").split(regex+regex);
				Boolean flag;
				Object left = TypeConvertor.parseEntity(beanHolder, ifset[0], regex);
				Object right = TypeConvertor.parseEntity(beanHolder, ifset[1], regex);
				if(ifset.length > 2) {
					if("gt".equals(ifset[2])) flag = ((Comparable)left).compareTo(right) > 0;
					else if("lt".equals(ifset[2])) flag = ((Comparable)left).compareTo(right) < 0;
					else if("ge".equals(ifset[2])) flag = ((Comparable)left).compareTo(right) >= 0;
					else if("le".equals(ifset[2])) flag = ((Comparable)left).compareTo(right) <= 0;
					else if(right != null ) flag = right == left || right.equals(left);
					else flag = left == right || left.equals(right);
				}else if(right != null ) flag = right == left || right.equals(left);
				else flag = left == right || left.equals(right);
				if(flag) synchronized(groupid){
					left = TypeConvertor.parseEntity(beanHolder, ifset[0], regex);
					right = TypeConvertor.parseEntity(beanHolder, ifset[1], regex);
					if(ifset.length > 2) {
						if("gt".equals(ifset[2])) flag = ((Comparable)left).compareTo(right) > 0;
						else if("lt".equals(ifset[2])) flag = ((Comparable)left).compareTo(right) < 0;
						else if("ge".equals(ifset[2])) flag = ((Comparable)left).compareTo(right) >= 0;
						else if("le".equals(ifset[2])) flag = ((Comparable)left).compareTo(right) <= 0;
						else if(right != null ) flag = right == left || right.equals(left);
						else flag = left == right || left.equals(right);
					}else if(right != null ) flag = right == left || right.equals(left);
					else flag = left == right || left.equals(right);
					if(flag){
						Object o = beanHolder.process((String)TypeConvertor.getParas(beanHolder, paras, "bean", regex));
						if (IProcessor.class.isInstance(o)){
							HashMap<String, String> newparas = new HashMap<String, String>();
							for(Entry<String, String> entry : paras.entrySet()) if(entry.getKey().indexOf("i:") == 0) newparas.put(entry.getKey().substring(2), entry.getValue());
							((IProcessor<Map<String, String>, Void>) o).process(newparas);
						}
					}
				}
				if(!flag && paras.get("else") != null){
					Object o = beanHolder.process((String)TypeConvertor.getParas(beanHolder, paras, "else", regex));
					if (IProcessor.class.isInstance(o)){
						HashMap<String, String> newparas = new HashMap<String, String>();
						for(Entry<String, String> entry : paras.entrySet()) if(entry.getKey().indexOf("e:") == 0) newparas.put(entry.getKey().substring(2), entry.getValue());
						((IProcessor<Map<String, String>, Void>) o).process(newparas);
					}
				}
			}
		}catch(Exception e){
			log.error("SyncIf: performing error.", e);
		}
		return null;
	}

}
