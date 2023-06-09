package net.yeeyaa.eight.access.webservice.jaxws;

import javax.activation.DataHandler;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name = "streamResponse", namespace = "http://webservice.access.eight.yeeyaa.net/")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "streamResponse", namespace = "http://webservice.access.eight.yeeyaa.net/")
public class StreamResponse {
	@XmlElement(name = "return", namespace = "")
	protected DataHandler _return;

	public DataHandler getReturn() {
		return this._return;
	}

	public void setReturn(DataHandler _return) {
		this._return = _return;
	}
}
