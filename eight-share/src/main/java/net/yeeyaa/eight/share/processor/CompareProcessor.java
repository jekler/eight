package net.yeeyaa.eight.share.processor;

import java.lang.reflect.Array;

import net.yeeyaa.eight.IProcessor;


public class CompareProcessor implements IProcessor<Object, Object> {
	protected volatile Object key;
	protected volatile Object value;
	protected IProcessor<Object, Object> next;
	
	public void setKey(Object key) {
		this.key = key;
	}
	
	public void setValue(Object value) {
		this.value = value;
	}
	
	public void setNext(IProcessor<Object, Object> next) {
		this.next = next;
	}

	@Override
	public Object process(Object instance) {
		if (!compare(instance, key)) synchronized(this){
			if (!compare(instance, key)) {
				key = instance;
				value = next.process(instance);
			}
		}
		return value;
	}
	
	public static Boolean compare(Object left, Object right){
		if(left == null ? right == null : left.equals(right)) return true;
		else {
			if(left != null && right != null && left.getClass().isArray() && right.getClass().isArray()){
				int length = Array.getLength(left);
				if(length == Array.getLength(right)) for(int i = 0; i < length; i++) { if(!compare(Array.get(left, i), Array.get(right, i))) return false; }
				else return false;
				return true;
			}
			return false;
		}
	}
}
