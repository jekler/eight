---
title: 思维实验 - 元件和组件
permalink: /docs/zh/element-and-component
key: docs-zh-element-and-component
---

由本章我们就要进入eight的世界，理解她的编码、组合、配置、运行、调整等等过程，逐渐明晰她的世界观与方法论的独特之处。同样的，我们也从一些简单的小例子入手，浅显地呈现出这些不同。

### 什么是元件
初次看到eight的代码会让人感到困惑。它们都无头无尾，不知起始和结束。不仅如此，它们相互还毫无关联，几乎见不到有任何一个类会去引用另外一个，无法跟踪思维脉络。还不仅仅如此，每个类写得都很抽象，不知道它们都是用来做什么的。比如这样的：
~~~ java
public class OverflowProcessor<T, R> implements IProcessor<T, R> {
	protected IProcessor<T, R> proxy;
	protected Integer max;
	
	public void setMax(Integer max) {
		if (max != null && max > 0) this.max = max;
	}

	public void setProxy(IProcessor<T, R> proxy) {
		this.proxy = proxy;
	}

	@Override
	public R process(T in) {
		if (max != null && in != null) if (in.getClass().isArray() && Array.getLength(in) > max || in instanceof String && ((String)in).length() > max 
				|| in instanceof Map && ((Map) in).size() > max || in instanceof Collection && ((Collection) in).size() > max) throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, "Data overflow. The maxLength is " + max);
		return proxy.process(in);
	}
}
~~~

但就是这样的代码，组成了eight万变的系统。它们是元件，是eight世界里最基本的颗粒，是构成各种模块的原子。

这种代码是做什么的？为什么要写成这样？这就需要继续我们前面的话题：接口是事物之间的联系，那么事物又是什么？

在eight的世界观里，事物是由更小事物联系而成的整体。非但事物之间的联系有着基本的形态，事物本身也由基本的颗粒组成，只不过这些基本颗粒数量庞大，难以列举。基本颗粒本身也是事物，只不过因为它们更基本，所以他们就更抽象，让人难明就里；又因为它们更基本，所以它们就更普适，可以使用到各种地方；也因为它们更基本，所以必须组合之后才具备具象，才能成为一个可以描述的“物”或“模块”。不错，这才是eight的全貌，事物本身就是由事物和联系组成。这些组成事物的基本颗粒，就是`元件（element）`{:.info}。
![元件](/eight/assets/images/element.gif){:.rounded width="720px" style="display:block; margin-left:auto; margin-right:auto"}

上面那段代码，不过是描述了一个常见的“过程”，当输入的参数（适配了若干基本类型）大于设定的阈值时，就异常退出，而如果符合设定，则交由被代理的“过程”继续执行。在用途上经常会嵌入到一段处理逻辑中进行合法性校验。这里提示一下，`eight所约定的共识，本质上是事物分解的方式`{:.info}，将事物分割成一块块切口规整的局部，任何一个局部均能跟性状对应的另一块拼图对接，产生不同的效果。前章所述的动态性只是特征的一部分。而此处的嵌入则是在修改事物本身的处理逻辑了。设想一下，如果我们的Search模块有了功能调整，对于查询字段长度超过1k的查询不予处理，其实我们不必改写代码的，将OverflowProcessor前置于Search就可以了。另外注意一下setMax这个方法，eight里所有的set均能参数化配置，可以在运行时动态注入并即时生效的。也即如果某一时刻我们需要修改阈值到10k，那么这会是一瞬间的事情，也许花费数纳秒。

OverflowProcessor是一个典型的元件，完整体现了eight编码的特征，eight几乎所有的代码都差不多这样：
- 必要约束
	- 对于任一元件，对于不包含在本元件内部的`未确定`{:.warning}引用和操作（包括但不限于对象、方法或过程等），应使用共识接口所定义的一个或多个接口和方法来`描述`{:.warning}；
	- 对于任一元件，若向外部提供服务和操作（包括但不限于对象、方法或过程等），则这些服务和操作应以实现共识接口所定义的一个或多个接口和方法来提供；
- 可选约束
	- 除有明确稳定依赖关系，元件不建议继承或引用其它的类型；
	- 元件的属性字段若需调整，应以set方法提供；

这里解释一下这几条约束的含义。第一条约束的含义是立足“自我”的coding，目标是领域自主（俗称我的地盘我做主）。所谓`未确定`{:.warning}，第一层含义是指写代码时外界关联尚未确定，常见做法是与服务提供方商定接口（当然大家已经知道，依赖是在这个时刻产生的），但eight不建议此种做法。eight建议开发者立足于模块本身，以理性思维分析当前领域问题的范畴，对范畴内存在的外部关联，套用共识模型去假设，然后用代码对域外事物进行`描述`{:.warning}。eight不建议过多过于详尽的协商和沟通，协商与沟通如有必要，也应停留在对模块领域的划分上，过于深入的沟通容易带来开发者思维的相互侵入而导致`潜依赖`{:.error}的发生，是系统不健壮的病因。

这里可能会有疑问：缺乏了解导致无法对接怎么办？原则上如果双方的模块均具备`领域内完备`{:.info}，也即该有的功能想到也做到了，那么对不上就应当是`联系方式`{:.info}上的，这种问题使用后述的适配技术比较容易解决的。如果功能上都出现无法对接，则至少必有一方在领域内业务实现上出现偏差，这并非沟通带来的，而是其对业务领域缺乏足够认知带来的（想想斯宾诺莎）。至于要追究是哪个模块的问题，通常把模块和业务拿出来就能够评审（毕竟大多数开发者是存在共识的）。也可以从实证角度出发，看看这些模块与其它环境的对接状况，但这不一定准确也未必有合适的环境。

第二层含义很简单，即便已经确知了外部模块的做法，也应当使用共识接口。原因很明白，因为对方也受约束，对外提供的是共识接口。这样至少保证在形式上双方无依赖关系，从而使得运行时一方的修改和调整不必影响到对方。

第三层含义更为深刻一点，什么算是`未确定`{:.warning}？是当前未确定还是以后有可能不确定的都算呢？eight的建议是`变化`{:.error}就是`未确定`{:.warning}，只要存在可变的部分，就是当前领域所`未确定`{:.warning}的，都应该去`描述`{:.warning}，而非固化。

对于`未确定`{:.warning}部分的指代，必须是共识接口中的一个或多个，对其使用须通过共识方法进行。这些指代往往会以属性形式通过set方法注入。

第二条约束的含义就很简单明了——遵从规则。既然这个世界要求我们须以这种模样彼此联结，我们就得长成这样。我们的领域对外界有何价值？有什么作用？就需要以外界能够明了的方式展示。

第三条可选约束，目的是避免依赖之网的形成。当然这不是绝对的，如果领域本身有稳定必须的外部依赖，例如一个元件负责redis调用，那么集成redis的工具类是应当的。第四条则是spring的依赖反向的要求，spring的作用其实与eight一致，用于解耦。后面可以看见spring在eight体系中的地位不逊于OSGi。

综合上述几条，可见eight的设计目标，是要`迫使`{:.error}:ghost:开发者实现独立、内聚而又普适的模块，做出可以积累的有价值而被广泛使用的模块。弱化沟通其实是一个副作用（虽然企业的管理者会喜欢听到它，这客观上能带来更大程度的开发解耦。但这并不是eight的主要目标，只要能够保证模块的独立性，深入沟通也未尝不可），如果模块是普适的，必定是对外感知要少的，毕竟我们不能与每一个使用者去沟通决定模块内部逻辑。

还是以OverflowProcessor为例，按它自身的观点：在“我”的世界中，我是一个代理审核员，我知道我代理另一个IProcessor，但我不知道也不想知道它是谁，是做什么的，管它呢！我与世界发生联系的方式就是我也是一个IProcessor，所以会有各种Object通过process(Object in)进来，我不知道他们从哪来，是些什么，管它呢！但如果它们是我所能处理的几种类型，我就要干活了。我得判断它们是否符合规则，规则是什么？不能超过max。max是多少？我不知道，管他呢！到时候会有人告诉我。符合规则的我转交给那个...管它呢！不符合的我就抛个异常扔掉了事。我就是我，我对外界一无所知，但我能做好我该做的事情。什么？我做的不是你想要的？那你为什么要把我组装进去？

### 组件的构成
eight提供了十几个包共计数百个元件。元件毕竟只是细小的单元，还未能构成有意义的整体事物，我们需要把它们拼装起来。感谢社区，这里有天然的拼装工具那就是spring，她的设计真是棒极了。深入使用你们会发现，eight对组装要求相当高，有很多不同寻常的配置方式，但没有什么是不能通过spring做到的，真是神奇的杰作:heart:。

举个小例子，给大家看一段配置，这是一个完整的模块。
~~~ xml
<bean class="net.yeeyaa.eight.dispatch.Dispatcher" name="preprocessor">
	<property name="pattern" value="([\w.$]+)@"/>	
	<property name="dispatcher" ref="callback"/>
	<property name="cache" value="true"/>
</bean>

<bean class="net.yeeyaa.eight.core.processor.CallbackProcessor" id="callback">
    <property name="processor" ref="beanHolder"/>
    <property name="paras" value="next"/>
</bean>

<bean factory-bean="callback" factory-method="getValue" name="nextname" scope="prototype"/>

<bean class="net.yeeyaa.eight.core.processor.UniversalProxy" id="next" scope="prototype">
	<property name="invoker">
		<bean class="net.yeeyaa.eight.osgi.runtime.ProxyBean">
			<property name="invoker" ref="invoker"/>
			<property name="name" ref="nextname"/>
		</bean> 		
	</property>
</bean>
~~~

关键的两个类的代码如下，都很简短：
~~~ java
public class CallbackProcessor implements IProcessor<Object, Object> {
	protected ThreadLocal<Object> local = new ThreadLocal<Object>();
	protected IProcessor<Object, Object> processor;
	protected Object paras;
	protected IProcessor<Object, Object> valuePrcoessor;

	public void setProcessor(IProcessor<Object, Object> processor) {
		this.processor = processor;
	}

	public void setValuePrcoessor(IProcessor<Object, Object> valuePrcoessor) {
		this.valuePrcoessor = valuePrcoessor;
	}

	public void setParas(Object paras) {
		this.paras = paras;
	}

	public Object getValue() {
		if (valuePrcoessor == null) return local.get();
		else return valuePrcoessor.process(local.get());
	}

	@Override
	public Object process(Object paras) {
		this.local.set(paras);
		Object ret = processor.process(this.paras); 
		this.local.remove(); 
		return ret;
	}
}

public class Dispatcher implements IProcessor<Object, Object>{
	protected IProcessor<String, IProcessor<Object, Object>> dispatcher;
	protected Boolean cache = false;
	protected Pattern pattern;
	protected ConcurrentHashMap<String, IProcessor<Object, Object>> processors;
	protected String error;
	
	public void setError(String error) {
		this.error = error;
	}

	public void setDispatcher(IProcessor<String, IProcessor<Object, Object>> dispatcher) {
		this.dispatcher = dispatcher;
	}

	public void setCache(Boolean cache) {
		if(Boolean.TRUE.equals(cache)) {
			processors = new ConcurrentHashMap<String, IProcessor<Object, Object>>();
			this.cache = cache;
		}
	}

	public void setPattern(String pattern) {
		if(pattern != null) this.pattern = Pattern.compile(pattern);
	}

	@Override
	public Object process(Object msg) {	
		if(msg != null){
			String key;
			if (msg instanceof ServiceMsg) key = ((ServiceMsg) msg).name;
			else key = new JSONObject(msg.toString()).getJSONObject("msg").getString("name");
			Matcher matcher = pattern.matcher(key);
			if(matcher.find()){
				String name = matcher.group(1);
				IProcessor<Object, Object> processor = null;
				if(cache) processor = processors.get(name);
				if(processor == null) processor = dispatcher.process(name);
				if(processor != null){
					msg = processor.process(msg);
					if(cache) processors.put(name, processor);
				}else if(error == null) throw new PlatformException(PlatformError.ERROR_PARAMETERS);
				else return error;
			}else if(error == null) throw new PlatformException(PlatformError.ERROR_PARAMETERS);
			else return error;
		}
		return msg;
	}
}
~~~

说明一下，beanHolder基本上可以理解为beanFactory，依靠它可以取到当前springContext里的bean。ProxyBean那段是个通用配置了，以后经常会见面，因为它是`域外世界`{:.info}的连接点，它会对接一个link，我们的模块通过它与未知的外界模块发生联结。所以它伪装为IUniversal（UniversalProxy），也即可以是eight世界里的任何一种接口。但这不重要，它在这个问题里是无关痛痒的小角色，唯一需要注意的是它靠name这个属性来确定调用哪个外部link。

所有的信息都完整提供了。现在的问题是大家知道这个模块是做什么的吗？为何如此配置？运行时入口在哪里？经过了何种处理得到何种结果？怎么做到的？大家有兴趣的可以给我留言喔~~

eight中这类奇妙的配置方式还有很多。

spring也协助eight承担模块的配置参数化，使得eight的模块参数可以运行时动态调整。就是最常见的配置方式了，例如：
~~~ xml
<bean class="net.yeeyaa.eight.osgi.runtime.BundleHttpService" destroy-method="destroy" id="httpService" init-method="initialize">
    <constructor-arg ref="log"/>
    <property name="context" ref="context"/>
    <property name="services"> 
	   <list>	
		   <bean class="net.yeeyaa.eight.osgi.runtime.BundleHttpService$ServiceInfo">
		   		<property name="alias" value="${framework.test.index.alias:/index}"/>
		   		<property name="name" value="/html"/>			
		   </bean>				   	   		   
	   </list>		   
    </property>  	
</bean>
~~~

这里有个alias配置被参数化为${framework.test.index.alias:/index}，也即默认值是'/index'。这个类是用来发布http服务的，alias是服务对应的url，通过修改framework.test.index.alias参数我们能够在运行时动态的把服务移动到任何一个url上。

~~~ xml
<bean class="net.yeeyaa.eight.access.http.HttpServer" id="httpServer">
    <constructor-arg ref="log"/>
    <property name="https" value="false"/>
	<property name="executor" ref="pool"/>
	<property name="port" value="${framework.http.httpServer.port:7241}"/>
	<property name="keyfile">
		<bean class="net.yeeyaa.eight.common.storage.MockStorage">
            <property name="resource">
                <value>classpath:META-INF/config/http.key</value>
            </property>
        </bean>
	</property>
	<property name="password" value="simulator"/>    	
</bean>
~~~
这是另一个例子，言简意赅，通过名字就能看出来framework.http.httpServer.port是用来设置端口号的了。也即我们能够运行时调整服务绑定的端口号。

事实上`未确定`{:.warning}的事物，并不仅限于“自我”与外界的联系，也包括“自我”运行时的“状态”，这些属于“自我”但又可变的东西也是需要外化的。在eight框架下，你可以将任何一个`未确定`{:.warning}状态抛给运行时去设置，eight还保证它们在运行时可改变。

以上的参数调整是需要重载一个springContext的（注意不需要重载bundle或Classloader），一般会有亚毫秒消耗。对于修改了Class的重新发布，消耗大一些，一般视模块大小会有数毫秒至数十毫秒的消耗（毕竟只是局部加载新类和创建服务，数十毫秒变更的模块应该是很大了），服务器性能好应该会更快。同时，新模块加载过程中旧模块将保持服务，加载后则被释放回收。

有没有更快的变化方式呢？如果参数并不需要带来服务重新设定（比如前面那个max只与业务判定相关就可以，而framework.http.httpServer.port则绑定port，需要对应服务重新初始化，所以不符合要求），可以暴露为jmx参数，例如前述那个formatter：
~~~ xml
<bean class="org.springframework.jmx.export.MBeanExporter" id="exporter" lazy-init="false">
	<property name="beans">
		<map>
			<entry key="bean:name=${framework.test.formatter:framework.test.formatter}" value-ref="formatter"/>
		</map>
	</property>
	<property name="assembler">
		<bean class="org.springframework.jmx.export.assembler.MethodNameBasedMBeanInfoAssembler">
			<property name="managedMethods">
				<value>setMode</value>
			</property>
		</bean>
	</property>
</bean>	 
<bean class="net.yeeyaa.test.format.Formatter" id="formatter">    
	<property name="processor">
		<bean class="net.yeeyaa.eight.core.processor.UniversalProxy">
			<property name="invoker">
				<bean class="net.yeeyaa.eight.osgi.runtime.ProxyBean$$Proxy">
					<property name="invoker" ref="invoker"/>
					<property name="name" value="${framework.test.formatter.processor:processor}"/>
				</bean> 		
			</property>
		</bean> 
	</property> 		   	  
</bean>
~~~
调用jmx接口进行setMode基本上就是个本地调用了，花费应该就是几个时钟周期，不需要重新加载任何东西。这都是可以通过配置绑定的，不需要coding时作任何决定，最多要求coding时对于`未确定`{:.warning}留下处理接口，所以给属性尽可能设置set方法是个好主意，谁知道什么时候那个属性需要变一变呢？。

元件使用spring联系成一个整体，此时这个整体已经有了明确的领域内的业务含义，这个东西就是组件。组件是我们在运行时能够动态加载、更新、卸载、设置参数等的基本单元。组件间的联系与对接，使用OSGi（Felix iPojo）框架。对于Felix和iPojo的介绍会是一件工作量很大的事情，大家有兴趣可以去看看[iPojo](http://svn.apache.org/repos/asf/felix/releases/org.apache.felix.ipojo-1.0.0/doc/ipojo-in-10-minutes.html)，在此我不再展开。

我们将一组元件通过spring联结成块，再用felix打包成bundle，这就是组件的物理形态了。

### 模块的边界
到这里可能大家会有疑问：为什么一会用spring组装，一会又用OSGi呢？弄两级结构这么复杂。spring组装后元件的关联关系不就固定了吗？还怎么保持动态性？如果她不用来响应变化，她存在的价值是什么？这其实就涉及到一个方法论上的难题：到底什么是事物？什么是联系？标准何在？如果无法区分，模块的边界就无法确定。

我们前面论述所谓事物本身也是由更小的事物相互联系组成，一级级探寻下去直到基本颗粒——元件。那元件肯定不可再分了，它是边界吗？不行，从原理上它缺乏意义不构成实体，仅仅是某个局部；从实践上惊人数量的元件作为单独模块运行是不可想象的。那么究竟到哪一级算是模块的边界呢？

还是拿我们自己作为例子，你与身边的人关系稳定吗？如果不够稳定，今天你能够合作的朋友明天会不会形同陌路？模块如果固化着你们彼此之间的联系，到时候就需要改写了。那是不是只要两个独立的个体就一定需要拆开成独立的模块呢？不一定，想想伯牙与子期的故事，人与人也可以休戚与共；反过来，单独的个体就一定稳定吗？如果有人午夜十二点会突然变成另外一个人呢？听起来是不是有点恐怖？灰姑娘这个可怜的女人显然没有稳定的自我，她的实质都是靠外在的装扮赋予她的，如果她确信王子爱上的是她本身，她为何会逃呢？

到这里大家应该能够明白我的意思了。经验主义者并不认为有明确的模块边界的分割线，但模块却有明确的划分标准——变化。如果稳定的联系就可以固化为一个模块，这样我们系统的模块数量就会更少，往往意味着更容易管理和维护；如果不稳定的联系就需要外化，这样我们在变化时付出的代价就会小，这同样有利于管理和维护，还便于开发。然后变化与不变化则又是因人而异因环境而异的，甚至不同时期变化都不一样。稳定只是相对的，唯一不变的只有变化本身。`模块的边界只能在实践中取得`{:.info}，这就是经验主义的态度。

这样，大家明白为何会有spring与OSGi两套组装框架了吧？eight本身的世界观并不区分模块的分割点，它从下到上都是一致的，由基本颗粒通过共识接口，层层联结成整体系统，这些联系是否稳定不在她认识范畴之内。而实践中，spring与OSGi的分割在变化点上，也即模块的边界处，内部联系靠spring，外部联系靠OSGi，但这个边界并不那么确定。spring自然相对固定，但如果变化不幸发生于模块内部，需要对现有模块再切割，分离出不稳定的联系，spring的配置式组装能够很容易的在切割处分离，并且连联结方式都一致呢（共识接口）。反之，如果实践证明两个模块之间的关系非常稳定，则可以合并成一个模块来获得更为简洁的系统结构和更为方便的维护管理，spring自然而然的承担起装配任务。所以采用spring是在变化与固化之间求得的折衷。目标仍是迅速响应变化。

说到这里，就可以顺便谈谈为什么会以罕见的方式提供eight的元件库了。不错，`核心目的还是将普遍性与偶然性分离，从而使系统能够快速应对变化`{:.info}。
![你说这个谁懂啊?](/eight/assets/images/who-knows.png){:.rounded width="480px" style="display:block; margin-left:auto; margin-right:auto"}

好吧，具体说来还是要为模块化开发提供基本的支撑。eight要超越以往所有的框架提供一个高度动态的系统，在现有技术条件下所要面对挑战很多，开发者的需求即是其一。eight要给开发者足够容易的实现方式，又要防止开发者不自觉的形成依赖之网，基础库就必须要仔细设计。

这样的基础库要达到两个目标，第一个是够用（当然是相对的），也即开放者进入eight环境后，不需要对eight底座增加基础lib包，靠环境提供的依赖就够。这样一方面是防止各种包被引入基础环境导致依赖网复杂化以及系统的不稳定；另一方面OSGi环境下的包管理和版本管理是件很头疼的事，尽量别让用户去做，更别提最近这些年OSGi化的包根本不好找；还有一个重要方面是eight会作为一个基础平台被分发到大规模集群中，如果基础运行环境也变来变去，每个项目都不一样，那么易用性就大打折扣，用k8s之类容器还好，简陋的集群如何部署就是个问题。

当然，要说这个问题并不是太致命。毕竟OSGi的bundle本身就可以自带lib动态加载，把自己需要的东西都装在模块里带上，根本不需要往基础环境里塞东西。所以即便eight没有覆盖所有的lib需求，目标还是一定能够达到的。但这样做必然使模块变得沉重，加载过程代价更高，还有一个缺陷是模块间难以`互通`{:.warning}。不同bundle是不同的Classloader，即便加载同一个类（二进制意义上的），相互之间是不认的，如果发生数据传递（当然不建议这么做）就会类型异常。

所以，eight提供基础库封装常见的lib就有必要了，毕竟模块进入环境，唯三的依赖就只能是：1）java核心库；2）共识接口；3）eight基础库。当然eight本身会使用的一些常见库底座里也能访问到，但一般不建议模块们直接引用，毕竟这些库是可能会变的。

这就可以解释为何eight的元件抽象又独立了，因为：1）用法太多样。比如有的做个search就好，有的就是假定你对结果做了过滤，你必须能变化组合以满足这些需求；2）又要保持稳定不变。这个太容易理解了，eight的基础库动了可不仅仅是集群部署的难题，它不知被嵌到哪些地方去干什么了呢。所以必须要求高度抽象的单纯，任何改动都需要考虑其内在的一致性。

第二点，eight的元件库使用方式跟通常lib不太一样，eight是用来搭架子的。通常lib的使用是new一个对象，这需要代码对lib有认知，认知就是依赖嘛。eight不太一样，将一个模块比喻成房子，eight的基础库要达成的目标是把屋架龙骨都搭好，更好一点的墙壁门窗都装好，地板吊顶都铺好，就差进家具。区别是一个要求你盖房子，一个你就是家具。所以架子建好后，业务模块对环境的认知可就少多了，往往手头上拿着几个IProcessor或IResource，也不管它们从哪里来，觉得需要就有了呗。于是进入“全能自我”开发状态，想怎么用怎么用。eight就成为那个事事满足的“世界”了。eight要以有限的库满足这些需要，不高度抽象是不行的。spring被各种罕见使用也是一个体现。所以，就eight来说，如何配置是门学问。

好处当然也有很多了。除了上述那些获得高度动态系统啦，获得开发时的保姆环境啦，获得模块内变化的可切割性啦，能让模块轻装上阵啦，光是效率提升就是很显著的。毕竟eight一共几百个类常用的几十个反复使用完成着以往巨大框架的任务，往往每个都被new了几十上百个，估计每段代码都hotspot了吧。另外配置也没想象的那么麻烦，比之代码它可是太容易copy-paste了，用点心入了门其实模式化特别强，更别说一旦定好架子也就只需要添砖加瓦了。

最后，你完全可以不按上述原则开发，你完全可以丢开那些抽象的元件和看不懂的配置，eight更不会要求你按元件的方式写代码，那是基础库的事（当然你也许也想构建更多适合自身业务的基础库，这并不是禁区）。你可以按你习惯的写法写一个完整的模块，一堆类在代码里紧密联系相互调用，再在bundle里带上一堆lib自成一体。你只需要保证：
- 对于不包含在本模块内部的`未确定`{:.warning}引用和操作（包括但不限于对象、方法或过程等），应使用共识接口所定义的一个或多个接口和方法来`描述`{:.warning}；
- 对于模块向外部提供的服务和操作（包括但不限于对象、方法或过程等），应以实现共识接口所定义的一个或多个接口和方法来提供；
- 最重要的，你的模块间不要交叉引用和依赖，也别往基础环境里丢lib。

那你就可以作为一个模块在eight中运行，并且与其它模块互通且互不依赖，它们的改变影响不了你，你也影响不了它们。但至于你自己的模块是不是方便参数化重载，是否容易切割，是否容易应对未知变化，那就是另一个问题了。

最后，在思维中做一个实验，前面那个Search与UserInterface在eight中应该是什么形态？当Search服务需要增加一个dir查询参数，这个变化应该如何响应？