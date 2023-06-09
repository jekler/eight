package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.ITriProcessor;
import net.yeeyaa.eight.IProcessor;

public class AdaptProcessor implements IBiProcessor<Object, Object, Object>, IProcessor<Object, Object> {
	protected ITriProcessor<Object, Object, Object, Object> tri;
	protected IBiProcessor<Object, Object, Object> bi;
	protected Object para;
	protected Boolean adapt;

	public void setAdapt(Boolean adapt) {
		this.adapt = adapt;
	}

	public void setBi(IBiProcessor<Object, Object, Object> bi) {
		this.bi = bi;
	}

	public void setTri(ITriProcessor<Object, Object, Object, Object> tri) {
		this.tri = tri;
	}

	public void setPara(Object para) {
		this.para = para;
	}

	@Override
	public Object perform(Object first, Object content) {
		if (adapt == null) return tri.operate(first, content, para);
		else if (adapt) return tri.operate(first, para, content);
		else return tri.operate(para, first, content);
	}

	@Override
	public Object process(Object instance) {
		if (adapt == null) return bi.perform(instance, para);
		else return bi.perform(para, instance);
	}
}
