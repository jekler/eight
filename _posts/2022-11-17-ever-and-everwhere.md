---
title: 无所不在无时不在 - 联系、运行与永恒的变化
permalink: /docs/zh/ever-and-everwhere
key: docs-zh-ever-and-everwhere
mermaid: true
---
<style>
.flowchart-link{stroke: green !important;}
#flowchart-pointEnd {fill: green !important;stroke: green !important;}
text.actor > tspan {fill: green !important;font-size: 16px !important;font-weight:bold !important;}
#arrowhead path {fill: green !important;}   
.messageText {fill: green !important;font-size: 16px !important;font-weight:bold !important;}
.messageLine0 {stroke: green !important;}
.messageLine1 {stroke: green !important;}
.relation {stroke: green !important;stroke-width: 2 !important;}
</style>

### 事物的联系
我们继续上一章的话题，关于Search的那个小插曲。其实第一种情况很简单，大家基本能想到。UserInterface与Search分别“自闭”开发：
~~~ java
public class UserInterface implements IProcessor<String, String>
	protected IProcessor<String, Integer> finder;
	
	public void setFinder(IProcessor<String, Integer> finder) {
		this.finder = finder;
	}

	public String process(String keyword){
		return keyword + " is: " + finder.process(keyword);
	}
}

public class Search implements IProcessor<String, Integer>
	public Integer process(String keyword){
		do something search...
	}
}
~~~

查询一个关键词的次数，双方的开发者很默契的想到输入String和返回Integer的IProcessor，这种基本共识几乎是自然的，不这么想倒是有点难度（该把不这么想的码农拖出去祭旗:stuck_out_tongue_closed_eyes:）。当然对于UserInterface的实现者来说，可能会在自己的视角里将其命名为finder，但这不重要。同时需要注意的是Search接口不见了，直接就是Search类。`因为他们都必须遵守仅能使用共识接口的约定，共识接口对人思维的约束和塑造也就体现出来了`{:.info}。

接下来，业务变成了正则表达式、这个无所谓了，属于Search模块的事情它自己弄吧，完事了自己模块更新一把。因为彼此无依赖，UserInterface根本不知道发生什么事情。

然后呢，Search需要对接多个服务了，所以需要每个服务调用时提供额外参数来指定查询目录。这该怎么弄？

大家是否觉得这简直是送分题：
~~~ java
public class UserInterface implements IProcessor<String, String>
	protected IBiProcessor<String, String, Integer> finder;
	protected String path = "defaultPath";
	
	public void setFinder(IBiProcessor<String, String, Integer> finder) {
		this.finder = finder;
	}
	
	public void setPath(String path) {
		if (path != null) this.path = path;
	}	

	public String process(String keyword){
		return keyword + " is: " + finder.perform(keyword, path);
	}
}

public class Search implements IBiProcessor<String, String, Integer>
	public Integer perform(String keyword, String dir){
		do something search...
	}
}
~~~
这样不就OK了？UserInterface和Search分别使用二元processor，然后一起更新一把。因为模块相互无依赖，UserInterface对外接口也保持不变，这事就这么神不知鬼不觉的过去了。喔，对了，为了装成UserInterface跟Search不认识的样子，这个目录在UserInterface里叫path，在Search里叫dir。perfect！

完美吗？才怪！UserInterface真的不认识Search吗？那Search调整服务方式关它什么事？它为何要了解这些，又从何了解这些？诸位在使用eight，要逐渐养成切割事物的能力，要有明确的边界意识。`该做的一定做好，不该做的分毫不动，谁的事情谁负责`{:.error}。有人或许会问，这也没啥吧，对方调整下接口，知道需要多给个路径举手之劳吧？况且也没啥影响，不都是几毫秒搞定吗？先不谈UserInterface是如何知道这个修改的（eight不鼓励频繁沟通），也不谈开发和升级成本。你怎知道UserInterface模块现在就只跟这个Search模块对接呢？它可能被发布到各处，在其他环境里Search那边不需要这个参数怎么办？你怎知道UserInterface就是你开发的呢？它的代码在毛里求斯怎么办？

归根到底，Search的路径与UserInterface要达成的业务究竟何干呢？为何要将它所需要的path嵌入到UserInterface的逻辑中去呢？如果UserInterface不能保持自身的稳定，又怎能成为其它模块可以信赖的伙伴呢（包括其它环境中的Search）？

到此有人就该困惑了。按道理你说的没错，但这事怎么办才好呢？你要由Search来负责？它也负责不了啊！它怎知道一个调用从哪过来的？这也不是它的业务范畴啊。

是啊，这既不是UserInterface的领域，也并非Search的领域，究竟是归谁负责呢？

这件事情发生在UserInterface与Search发生联系的过程中，双方的业务都没错，但是双方的认知（共识）发生了偏差。所以，这是这两者间的联系的锅，该由联系去适配。eight提供了相关的元件专干这事：
~~~ java
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
~~~
可以伪装成IBiProcessor或IProcessor来代理ITriProcessor或IBiProcessor，根据adapt参数，把预制好的para注入调用过程（这里是path）。于是UserInterface不用改一行代码，Search也别管这参数从何而来了。

何必这样画蛇添足呢？至于非得在事物中间塞上一套联系？须得明白，这是它唯一容身之地。联系既不属于A，也不属于B，更何况它并不稳定，随时可以跟随A或B的变化而消亡，而模块需要保持作为自我的独立性和稳定性。除非你绝对肯定UserInterface和Search间的绑定关系（那样你还不如把两模块合并，都属于模块内业务想怎么弄怎么弄，都不需要装作不认识了），否则不应当将其中之一的业务由另一方配合承担。即便模块尚未开发就已经知道实际情况，开发时也应当在脑子里过滤掉以排除潜在依赖，这也是eight不建议深度沟通的原因所在（顾虑太多容易丧失自我）。

那么到底...
### 什么是联系
人是经验与记忆的载体，人又有各自的愿望和动力，这些共同构造了人的实质。不同的人出于天然的不同，构成了种种不同的认知。沟通就一定有效吗？有时可以，有时未必。就算UserInterface知道Search需要一个额外的path又怎样？你怎知道它想的不是perform(String path, String keyword)呢？人，终究是无法完全相互理解的，这道天堑不可逾越。

那么什么使人相互协作，共同生存下去呢？人与人之间的联系。这些联系并不属于单独的某个人，只存在于他们彼此相互羁绊的时刻。人和人总有这样那样的差异，他们的接口总是难以完全对接，这是无可奈何又习以为常的事情。但那应该怎么办呢？人们很自然的习惯去适应彼此，在彼此羁绊之中调整自己与对方的关系。想想看你们自己与人的相处，有任何两种关系是完全一样吗？但这种联系又并不属于人们的其中一个，它也并不稳定，如果对象消失，那它也就不复存在了。如果这种联系是人本身的一部分，那人就一无所有了。
![羁绊](/eight/assets/images/relation.gif){:.rounded width="480px" style="display:block; margin-left:auto; margin-right:auto"}

如果实在就是适配不了呢？！那就一拍两散，没有谁必须与谁永远相伴下去。

这就是为什么eight要外化联系的原因。但说这么多，这联系究竟是什么？它长什么样？回答这个问题前，我们还是把整个eight的运行时结构勾勒出来，有个直观感受吧。
![eight](/eight/assets/images/eight.gif){:.rounded width="720px" style="display:block; margin-left:auto; margin-right:auto"}

联系就在里面，那个实体为link的组件。

### 运行与变化
eight与其它框架不同之处在于它多出一种时态——装配时。联系是在装配时发生的。这个时态与运行时没有固定边界，它可能在运行时之前，也可能在运行时之中，所以它既有静态形态，也有动态形态。eight在进入运行态前，往往需要配置各种联系。
![装配时](/eight/assets/images/equiptime.gif){:.rounded width="720px" style="display:block; margin-left:auto; margin-right:auto"}

这些我们前面都已经了解了，用spring做配置是不用介绍了。接下来主要介绍运行时的动态联结，那些package、bundle、component、config、instance和link们的关系。
- 首先是基础底座了，基础底座就是felix框架了，内置的三大基础libs分别是1）java runtime libs；2)共识接口；3)eight基础库。当然eight本身还依赖着一些常用的工具libs，但这一般对上层运行的bundle不可见。同时使用者也可能自行开发一些元件库。这些构成了eight的最底层，第三方libs往往会有各种交叉依赖，所以这层变动是很困难的，升级往往意味着重启。
- 其上是bundle层了。eight中的bundle间已经不存在交叉依赖了，全是垂直依赖关系，保证任何一个bundle在物理上不依赖于与其平级的bundle，这样依赖之网也就被解除了。所以当bundle里的代码或配置修改后，需要重载时，其余的bundle不受影响。当然它们都依赖着底层libs，尤其是共识接口。一旦底层变动...呃，那本来就是要重启系统了，所以保持底层相对稳定很重要。但如果底层没有它们需要的libs呢？前面介绍过，可以在bundle里自带干粮。
- 然后是component，到这里已经离开物理层进入逻辑层了。component就对应eight里的组件，关于component与instance的关系和故事大家可以参阅前面的iPojo介绍。值得注意的是，bundle里可以定义零到多个component，但一般建议只定义一个，否则多个component必然在生命周期上存在耦合性，这在大多数情况下都是应该避免的。
- 通过前面介绍我们知道，component里面往往是一套springContext，大量的bean相关关联构成一个相对稳定而有意义的整体，当然就如前面所说那样，也可以完全不用元件和spring。因为bundle本身是无依赖的，component彼此也无依赖。
- eight基础库本身定义了很多component，这是eight平台的一部分，维持着运行时的各项服务。如bundle、component、config、instance等的动态加载和动态更新、线程池管理、消息管理等。另外跟元件类似，提供了很多可选的组件，用来搭建系统的框架。
- 以上都是静态结构，再往上就进入运行着的部分了。instance之于component，相当于object之于class。可以以component为模板生成多个instance实例。那么生成过程中那个config文件是做什么用的呢？那相当于构造函数的参数，spring的各项配置和其它一些环境参数，如instance名称，类型，标签，优先级等等均在此配置。不同的config生成的instance各不相同。
- instance生成后，config就被注入springContext，然后springContext启动，各项bean生成并联结，就运行并可以对外提供服务了。值得注意的是，与通常意义上的模块（库、包、系统的一部分代码等）不同，`eight的模块是活的模块，它本身自成一体的独立存在和运行，不依整体或他者而生灭`{:.info}，这一点可以理解为容器化里的某个服务。
- config是个文本文件，它被eight所监控，任何对它配置项的修改将导致该instance重启（注意不是bundle也不是component重启，不需要丢弃类加载器重新加载类）。而对bundle的修改会导致其中所有component生成的instance重启。
- link是联系。它本身是一个bundle对应的component创建的instance，它也由config配置和维护，并随config修改而变化。link内部并不运行springContext，相反它利用OSGi的筛选器筛选其它instance并把自身注入到两端的instance中，从而完成桥梁联结的作用。引用方一端调用，将通过IUniversal接口传递到服务方，从而完成事物间的协作。
- link可以配置脚本执行简单的适配工作，目前支持groovy。对于复杂的适配，则可能需要在事物中间增加适配component，当然这种component会带来额外的开发，也不稳定，会随关系的消失而灭失。所以祈祷我们的模块开发者共识差异不要太大吧。
- 最后的注意事项，出于Java本身的特质，尽管解除了类依赖，Object的依赖仍可能存在。所以应严格控制bundle内的类型通过对外服务扩散到系统其它模块。一般的解决方式有几种：1）推荐的，以几个基础库提供的基本数据结构，如Java自带的String，Map，List，Json等向外提供数据，反正你传内部类给别人别人也看不懂（注意，容器化系统之所以能够服务间类型依赖解耦用的正是这种方式，只不过它通过串行化反串行化的网络传输强制实现）。2）如果服务将一些内部类以共识接口的方式提供给外界，比如某个服务回传一个IProcessor给调用方，调用方则用此IProcessor做下阶段处理，则尽可能使用eight元件库提供的共识代理，这样即便对方持有该代理，代理也遵循空值-查询-弱引用规则，不会持有内部类句柄；3）调用方对得到的非基础类型应遵循随用随取，阅后即焚原则，不应长期持有。

以上这些，就是关于eight的入门介绍，就此暂告一段落。更多相关内容，可以参阅相关的[技术文档](/eight/assets/images/eightV1.0.pdf)和[介绍文档](/eight/assets/images/eight.pdf)。