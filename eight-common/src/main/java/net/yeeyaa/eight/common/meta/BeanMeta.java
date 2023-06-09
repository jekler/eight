package net.yeeyaa.eight.common.meta;

import java.io.Serializable;

import net.yeeyaa.eight.core.util.TypeConvertor;


public class BeanMeta implements Serializable {
	private static final long serialVersionUID = -6659908254970479528L;
	protected final String id;
	protected final Boolean ref;
	protected Object initValue;
	protected Object factory;

	public BeanMeta(IMetaBean meta) {
		factory = meta.getFactory();
		initValue = meta.getInitValue();
		id = TypeConvertor.randomUuid();
		ref = false;
	}

	public BeanMeta(BeanMeta meta) {
		id = meta.id;
		ref = true;
	}
	
	public String getId() {
		return id;
	}

	public Boolean getRef() {
		return ref;
	}

	public Object getFactory() {
		return factory;
	}

	public void setFactory(Object factory) {
		this.factory = factory;
	}

	public Object getInitValue() {
		return initValue;
	}

	public void setInitValue(Object initValue) {
		this.initValue = initValue;
	}
}
