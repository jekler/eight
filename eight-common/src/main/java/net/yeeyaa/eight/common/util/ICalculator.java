package net.yeeyaa.eight.common.util;

public interface ICalculator {
	public Boolean compare(String firstString, String operator, String secondString);
	public void setFirstOperand(Object firstOperand);
	public void setSecondOperand(Object secondOperand);
}
