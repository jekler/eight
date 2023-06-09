package net.yeeyaa.eight.core.task;


import java.io.Serializable;
import java.util.Date;

import net.yeeyaa.eight.core.util.Interval;

public class TaskEntity implements Serializable {
	protected String name;
	protected Object bean;
	protected Long begin;
	protected Interval span;
	protected String remark;
	protected Object parameter;
	protected Boolean ignore;
	protected Short type; 

	public TaskEntity() {}

	public TaskEntity(String name, Object bean, Long begin,	Interval span, String remark, Object parameter, Boolean ignoreerror, Short type) {
		this.name = name;
		this.bean = bean;
		this.begin = begin;
		this.span = span;
		this.remark = remark;
		this.parameter = parameter;
		this.ignore = ignoreerror;
		this.type = type;
	}

	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Object getBean() {
		return bean;
	}

	public void setBean(Object bean) {
		this.bean = bean;
	}

	public String getRemark() {
		return remark;
	}

	public void setRemark(String remark) {
		this.remark = remark;
	}

	public Object getParameter() {
		return this.parameter;
	}

	public void setParameter(Object paras) {
		this.parameter = paras;
	}

	public Long getBegin() {
		return begin;
	}

	public void setBegin(Long begin) {
		this.begin = begin;
	}

	public Date getDate() {
		return new Date(begin == null ? 0 : begin);
	}
	
	public void setDate(Date begin) {
		if (begin == null) this.begin = 0L;
		else this.begin = begin.getTime();
	}
	
	public Interval getSpan() {
		return span;
	}

	public void setSpan(Interval span) {
		this.span = span;
	}

	public Boolean getIgnore() {
		return ignore;
	}

	public void setIgnore(Boolean ignore) {
		this.ignore = ignore;
	}

	public Short getType() {
		return this.type;
	}

	public void setType(Short type) {
		this.type = type;
	}
}
