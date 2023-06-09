package net.yeeyaa.eight.common.util;

import java.lang.reflect.Method;
import java.util.Collection;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.LinkedList;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CommonTokenStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class PojoSelector implements ICalculator{
	protected static final Logger log = LoggerFactory.getLogger(PojoSelector.class);
	protected Object firstOperand;
	protected Object secondOperand;
	
	public void setFirstOperand(Object firstOperand) {
		this.firstOperand = firstOperand;
	}

	public void setSecondOperand(Object secondOperand) {
		this.secondOperand = secondOperand;
	}

	public static <T> T getFirstValidPojos(Collection<T> pojos, String regex, Object secondOperand , ICalculator calc){
		if (calc == null) calc = new PojoSelector();
		LogicParser parser = new LogicParser(new CommonTokenStream(
				new LogicLexer(new ANTLRStringStream(regex))), calc);
		try {
			for(Object pojo : pojos){
				calc.setFirstOperand(pojo);
				calc.setSecondOperand(secondOperand);
				if (parser.stat()) return (T)pojo;
				parser.reset();
			}
		} catch (Exception e) {
			log.error("PojoSelector getFirstValidPojos error: " + regex + " " + secondOperand, e); 
		}
		return null;
	}
	
	public static <T> Collection<T> getValidPojos(Collection<T> pojos, String regex, Object secondOperand , ICalculator calc){
		LinkedList<T> list = new LinkedList<T> ();
		if (calc == null) calc = new PojoSelector();
		LogicParser parser = new LogicParser(new CommonTokenStream(
				new LogicLexer(new ANTLRStringStream(regex))), calc);
		try {
			for(Object pojo : pojos){
				calc.setFirstOperand(pojo);
				calc.setSecondOperand(secondOperand);
				if (parser.stat()) list.add((T)pojo);
				parser.reset();
			}
		} catch (Exception e) {
			log.error("PojoSelector getValidPojos error: " + regex + " " + secondOperand, e); 
		}
		return list;
	}
	
	protected Object getObjectById(String operandString, Object operandObject) throws Exception{
		if (operandString == null || operandString.length() == 0) throw new Exception("Id cannot be null");
		if ("null".equals(operandString)) return null;
		else if ("true".equals(operandString) || "false".equals(operandString)) return new Boolean(operandString);
		else if (operandString.startsWith("\"")) return operandString.substring(1, operandString.length()-1);
		else if (Character.isDigit(operandString.charAt(0)) || operandString.startsWith(".")) {
			if (operandString.indexOf(".") == -1){
				return new Integer(operandString);
			}
			else {
				return new Double(operandString);
			}
		}
		else {
			Object temp = operandObject;
			for (String method : operandString.split("\\.")){
				Method m = null;
				try{
					m = temp.getClass().getMethod("get"+method);
				}
				catch (NoSuchMethodException e){
					try{
						String altmethod = null;
						if (Character.isLowerCase(method.charAt(0))) 
							altmethod = Character.toUpperCase(method.charAt(0))+method.substring(1);
						else altmethod = Character.toLowerCase(method.charAt(0))+method.substring(1);
						m = temp.getClass().getMethod("get"+altmethod);
					}
					catch (NoSuchMethodException e1){
						try{
							m = temp.getClass().getMethod(method);
						}
						catch (NoSuchMethodException e2){
							try{
								String altmethod = null;
								if (Character.isLowerCase(method.charAt(0))) 
									altmethod = Character.toUpperCase(method.charAt(0))+method.substring(1);
								else altmethod = Character.toLowerCase(method.charAt(0))+method.substring(1);
								m = temp.getClass().getMethod(altmethod);
							}
							catch (NoSuchMethodException e3){
								try{
									m = temp.getClass().getMethod("is"+operandString);
								}
								catch (NoSuchMethodException e4){
									try{
										String altmethod = null;
										if (Character.isLowerCase(method.charAt(0))) 
											altmethod = Character.toUpperCase(method.charAt(0))+method.substring(1);
										else altmethod = Character.toLowerCase(method.charAt(0))+method.substring(1);
										m = temp.getClass().getMethod("is"+altmethod);
									}
									catch (NoSuchMethodException e5){									
									}								
								}							
							}						
						}					
					}
				}
				if (m != null) {
					temp = m.invoke(temp);
				}
				else {
					if (HashMap.class.isInstance(temp)) temp = ((HashMap) temp).get(method);
					else if (Hashtable.class.isInstance(temp)) temp = ((Hashtable) temp).get(method);
					else throw new Exception ("Cannot find method "+method+" in "+temp.getClass());
				}			
			}
			return temp;
		}
	}
	
	public Boolean compare(String firstString, String operator, String secondString) {
		if (firstString == null || firstString.length() == 0 || operator == null || operator.length() == 0 
				|| secondString ==null || secondString.length() == 0) return false;
		try {
			Object left = getObjectById(firstString, firstOperand);
			Object right = getObjectById(secondString, secondOperand);
			if (left == null || right == null){
				if("==".equals(operator)) return left == right;
				else if ("!=".equals(operator)) return left != right;					
				else return false;				
			}
			else if (Number.class.isInstance(left)){
				Double dleft = new Double(((Number)left).doubleValue());
				Double dright = new Double(((Number)right).doubleValue());
				if ("==".equals(operator))  return ((Comparable)dleft).compareTo(dright) == 0;
				else if ("!=".equals(operator))  return ((Comparable)dleft).compareTo(dright) != 0;
				else if (">=".equals(operator))  return ((Comparable)dleft).compareTo(dright) >= 0;
				else if ("<=".equals(operator))  return ((Comparable)dleft).compareTo(dright) <= 0;
				else if (">".equals(operator))  return ((Comparable)dleft).compareTo(dright) > 0;
				else if ("<".equals(operator))  return ((Comparable)dleft).compareTo(dright) < 0;
				else return false;
			}
			else if (Comparable.class.isInstance(left)){
				if ("==".equals(operator))  return ((Comparable)left).compareTo(right) == 0;
				else if ("!=".equals(operator))  return ((Comparable)left).compareTo(right) != 0;
				else if (">=".equals(operator))  return ((Comparable)left).compareTo(right) >= 0;
				else if ("<=".equals(operator))  return ((Comparable)left).compareTo(right) <= 0;
				else if (">".equals(operator))  return ((Comparable)left).compareTo(right) > 0;
				else if ("<".equals(operator))  return ((Comparable)left).compareTo(right) < 0;
				else return false;
			}
			else {
				if("==".equals(operator)) return left.equals(right);
				else if ("!=".equals(operator)) return !left.equals(right);					
				else return false;
			}
		} catch (Exception e) {
			log.error("PojoSelector compare error: " + firstString + " " + operator + " " + secondString, e); 
			return false;
		}
	}
}
