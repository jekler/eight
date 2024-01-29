---
title: 用wifi棒子操纵ROS2机器人
permalink: /docs/zh/eight-on-openstick
key: docs-zh-eight-on-openstick
mermaid: true
---
<style>
.flowchart-link{stroke: green !important;}
#flowchart-pointEnd {fill: green !important;stroke: green !important;}
#flowchart-pointStart {fill: green !important;stroke: green !important;}
text.actor > tspan {fill: green !important;font-size: 16px !important;font-weight:bold !important;}ugutuuuu
#arrowhead path {fill: green !important;}   
.messageText {fill: green !important;font-size: 16px !important;font-weight:bold !important;}
.messageLine0 {stroke: green !important;}
.messageLine1 {stroke: green !important;}
.relation {stroke: green !important;stroke-width: 2 !important;}
th {
	background: #dddddd;
	word-wrap: break-word;
	text-align: center;
}
tr:nth-child(odd) {   
  background-color: #c0e1ff;
  color: #222; 
}
tr:nth-child(even) {
  background-color: #fef6de; 
  color: #222;
}
</style>

本章属于人类迷惑行为:rofl::rofl:

### 设备简介

这次的主角是。。。一根棒子，喏，就它

![wifi棒子](/eight/assets/images/stick1.jpg){:.rounded width="720px" style="display:block; margin-left:auto; margin-right:auto"}

![wifi棒子](/eight/assets/images/stick2.jpg){:.rounded width="720px" style="display:block; margin-left:auto; margin-right:auto"}

自从高通MSM8916的Mainlining开源后，各种古董设备就被玩出了花。10年前的红米手机变身klipper上位机后，身价也是水涨船高。而国内的运营商们，出于内卷的原因，向市场投放了一批售价仅9.9元的4g wifi棒子。有好事者拆开后发现芯片竟是Snapdragon 410，就开发了一个openstick项目将Mainlining迁移到棒子上了。然后棒子立马被抢购一空，我也跟风买了一根。

配置不高，Snapdragon 410是高通第一款64位arm处理器，上市于2013年底，距今整整10年。Cortex-A53架构，四核1.2g。棒子给它搭载了512m内存和4g的rom，勉强算够用。openstick是基于debian 11（bullseye）的，刷机教程满网都是我就不放链接了。当初刷完机后剩余2.5g左右空间，自带了一个openjdk 1.8，于是就跑了个eight，几个示例都能运行。然后...，就丢一边落灰了。为啥呢？因为实在想不到应用场景。这东西除了个供电的usb接口（当然可以驱动成一个无线网卡）就没有一个外设接口，所以不能像树莓派那样做设备服务器；又因为资源太少，连个正经数据库跑起来也挺艰难，所以又做不了应用服务器。网上倒是有卖扩展坞的，卖的比棒子还贵几倍，有那钱不如出去吃烤串。于是，跑过eight后，想不出有啥用，只能丢一边去了，可惜了性能还算不错的cpu。

前几天翻古物堆又翻出它来，突然想到了ROS。`ROS实质上是个网络总线的域控制器`{:.error}，只要有网络，就能将同一子网内的所有ROS外设们驱动起来，相当于拿网络当I/O接口。那么，这不就完美的扩展了棒子的外设吗？棒子正好是个4g热点，天生是带wifi子网的，如果上面能跑Eight on ROS，那不就可以操纵无人车了吗？

说干就干，开刷ROS2。可仔细一看傻眼了，ROS2确实支持debian bullseye，但却是tier 3，不提供二进制包，只能自己编译。剩下的选项就是棒子本地编译还是交叉编译了。本地编译资源实在太少，但交叉编译搭环境又太麻烦，稍不小心还容易出错，设备的依赖库也是个问题。于是心一横，还是委屈一下棒子吧:sweat_smile:。

接下来，就是一场噩梦。希望大家不要去尝试，后面我会把编译好的镜像放出来给各位刷机(镜像就在之前虚拟机存放的云空间里：[百度云盘](https://pan.baidu.com/s/1n36sOWviHADT4HxKs9jFfw)，提取码为 bo5c)。

具体说来，要编译ROS2的全家桶，再加rcljava，对内存和磁盘空间需求很高。棒子本身的资源根本不够。内存的需求是至少2g，磁盘需要20g左右。该如何解决呢？先解决内存，使用棒子rom剩余空间，设一个2g左右的swap，然后挂载虚拟内存。于是基本上没有磁盘了，要编译只能挂载网络文件系统，openstick是不支持的，但内核有fuse，所以apt install sshfs就可以了，然后拿一台linux服务器当磁盘服务器，虽然会比较慢，但也算是解决了磁盘空间问题。接着就是漫长难熬，且反复失败的编译过程，整整花了5天。这是一场行为艺术，棒子也是九死一生。接下来记录一下编译的若干要点，还是强烈建议大家不要去尝试。

- 第一点是关于python3的。棒子安装的python3是删除了主文件和库文件的。需要apt install --reinstall libpython3.9-stdlib python3.9-minimal python3.9-minimal。同时注意安装的python在/usr/bin下预制的软连接是错误指向，删除后重新ln到python3.9上。

- 第二点是因为众所周知的原因，github是不能正常访问的。所以需要设置某种代理，然后设置git config --global https.proxy。但是在安装过程中的很多下载并不通过git，而是直接采用curl或wget，这需要安装proxychains-ng才行。安装后前置命令，如proxychains4 -f /etc/proxychains.conf rosdep update。

- 第三点是在使用colcon build时，有些部件是巨大的，比如rclcpp，它大量使用c++的template以致于编译时占用内存过高，此时并行编译会内存不够导致系统宕机。一般单个进程就需要2g内存（这也是前面要求至少配置2g内存的原因），如果编译不过去，给加上--executor sequential编译参数，这样会单进程顺序编译。

- 第四点，mcap_vendor这个包需要下载v0.8.0的包，但是由于不明原因，src里的散列校验码与实际下载包不符，需要修改src里的校验码，从da39a3ee5e6b4b0d3255bfef95601890afd80709到b44637791da2c9c1cec61a3ba6994f1ef63a228c。

- 第五点，原来openstick自带了openjdk 1.8但是没有配置JAVA_HOME，需要在编译rcljava前export该参数

看起来问题似乎不多，但是编译起来还是会遇见各种状况，这是毫无意义的事情，所以还是用我的镜像吧。

### 系统部署和使用

主要参考[https://wiki.debian.org/DebianScience/Robotics/ROS2](https://wiki.debian.org/DebianScience/Robotics/ROS2)和[https://docs.ros.org/en/rolling/Installation/Ubuntu-Development-Setup.html#build-the-code-in-the-workspace](https://docs.ros.org/en/rolling/Installation/Ubuntu-Development-Setup.html#build-the-code-in-the-workspace)两篇文档。

具体说来，首先
~~~ shell
sudo apt install -y colcon python3-rosdep2 vcstool
~~~

这几个包是编译和安装ros2所必须的工具。vcstool负责根据配置下载和生成项目源代码，而rosdep则根据源码下载对应的依赖库，colcon就是用来编译源码的了。

之后创建一个安装目录，注意目录里创建一个src子目录，然后
~~~ shell
proxychains4 -f /etc/proxychains.conf wget https://raw.githubusercontent.com/ros2/ros2/master/ros2.repos
~~~

注意，之后按照文档要求是将某些部件用sed去除了，这个可视情况而定。但文档本身是不正确的，去除太多了以致于编译时找不到依赖，编译时发现ament、ignition等几个部件是不能去除的。我在编译时一个都没去掉，全部编译了。接下来是
~~~ shell
vcs import src < ros2.repos
~~~

下载源码后，配置依赖
~~~ shell
rosdep init
proxychains4 -f /etc/proxychains.conf rosdep update
proxychains4 -f /etc/proxychains.conf rosdep install --from-paths src --ignore-src -y --skip-keys "fastcdr rti-connext-dds-6.0.1 urdfdom_headers"
~~~

如果没有安装好ros-dev-tools请安装，否则就开始编译吧，噩梦开始了。
~~~ shell
source install/setup.bash
colcon build --merge-install
~~~

偶尔遇见下载错误编译失败，就需要切换到
~~~ shell
proxychains4 -f /etc/proxychains.conf colcon build --merge-install
~~~

祝你好运。

编译好之后，注意source install/setup.bash，然后就可以编译rcljava了。注意提前设置好JAVA_HOME，编译过程跟前面两章的方法一样，没啥特别的，就不再赘述了。

这时候已经可以打开机器人小车了，让它加入棒子的热点中，然后，同样开启底盘和摄像头，就可以ros2 topic list看看当前子网内的可用消息队列。
~~~ shell
root@openstick:~# ros2 topic list
/PowerVoltage
/camera/color/camera_info
/camera/color/image_raw
/camera/color/image_raw/compressed
/camera/color/image_raw/compressedDepth
/camera/depth/camera_info
/camera/depth/color/points
/camera/depth/image_raw
/camera/depth/image_raw/compressed
/camera/depth/image_raw/compressedDepth
/camera/depth/points
/camera/extrinsic/depth_to_color
/cmd_vel
/diagnostics
/joint_states
/mobile_base/sensors/imu_data
/none
/odom
/parameter_events
/robot_description
/robotpose
/robotvel
/rosout
/set_pose
/tf
/tf_static
~~~

如果没有出现以上的topic，也许是因为你没有设置合适的ROS_DOMAIN_ID，参考前面章节的做法把它在.bashrc里设置好。

rcljava编译好后，再source /root/ros2_java/install/local_setup.bash，然后把eight-seat丢进去，跟前面一样，运行
~~~ shell
java -Dframework.boot.scanner.node=nodeName -Dfile.encoding=UTF8 -Dframework.web.user=xxxx -Dframework.web.password=pppp -Dframework.web.url=https://www.yeeyaa.net/api -Djdk.util.zip.disableZip64ExtraFieldValidation=true -cp eight-seat-1.0.0.jar:$CLASSPATH aQute.launcher.pre.EmbeddedLauncher
~~~

OK，eight在棒子上跑起来了。此时我们就可以去[https://www.yeeyaa.net/](https://www.yeeyaa.net/)绑定节点了

![绑定节点](/eight/assets/images/deploy-to-openstick.jpg){:.rounded width="1280px" style="display:block; margin-left:auto; margin-right:auto"}

这是我的账号绑定列表，表里面ros-openstick就是给棒子起的节点名称，绑定上了ros-robot-control这个应用。而ros-wheeltec则是小车，上面同样绑定该应用。可见这两款设备的arch都是aarch64，也就是arm64的。比较特别的是winnt4与win98虚拟机是x86架构的，也就是虚拟机虚拟化时降级到了32位。

里面还有个特别的存在，就是那个loongson，架构为mipsel。那就是龙芯了，就是它。

![龙芯笔记本](/eight/assets/images/lemote.jpg){:.rounded width="720px" style="display:block; margin-left:auto; margin-right:auto"}

这是一款生不逢时的上网本，搭载着采用了MIPS-III指令集的龙芯2f cpu。龙芯2系列是龙芯公司第一款64位cpu，生于2007年。彼时的中国，自主研发cpu是一种愚公移山之举，艰难而又看不到光明。这款上网本也就是该公司第一款面向终端消费者的笔记本，品牌叫逸珑，生产于2008年，这也是一次失败的商业尝试。简陋的用户界面、孱弱的性能、贫瘠的生态以及不为人所关注的影响力都让它在那个时代不合时宜。该款产品出货量不多，但库存到5年后都清理不掉，现在却已经成为一种历史见证品。我在2013年购得该机，是更少见的8101版本。不同于产量更多的8089，搭载的是10英寸屏幕，颇具收藏价值。喏，就是这位爱不释手的先生的同款，这款本仔细用来是挺可爱的。

![龙芯笔记本](/eight/assets/images/lemote1.jpg){:.rounded width="720px" style="display:block; margin-left:auto; margin-right:auto"}
![龙芯笔记本](/eight/assets/images/lemote2.jpg){:.rounded width="720px" style="display:block; margin-left:auto; margin-right:auto"}

当初为了改善龙芯的生态，龙芯的工程师们亲手操刀把jdk 1.6的运行时迁移到了mipsel指令集上，但却再没有余力实现jit，所以运行效率较为低下（jit对eight影响颇大，eight的元件和组件因为高度复用几乎都会被jit优化）。这在当时也是龙芯2f上唯一一个jdk版本。eight做出大量反向移植，要将最低支持版本锁定在1.6，其中一个原因，是希望她运行在国产的芯片上，向黑暗中无畏前行的创业者们致敬。龙芯已经今非昔比，也不再沿用mips指令集，但筚路蓝缕的精神长存。

![龙芯笔记本](/eight/assets/images/eloong5.jpg){:.rounded width="720px" style="display:block; margin-left:auto; margin-right:auto"}

回到主题上来，弄完这一切后就剩eight了。eight的特点是只要轮到她，事情就要结束了。没错，系统下发后，打开`http://棒子ip:7241/sub/ros.html`{:.info}，同样的配方，同样的味道，开车吧朋友们，向着自己的“大脑”撞过去:innocent:。

![棒子自杀](/eight/assets/images/eight-on-openstick.gif){:.rounded width="720px" style="display:block; margin-left:auto; margin-right:auto"}

这里说一下不知是410的功效问题还是openstick自带的jdk没做性能优化，或者是网络性能不佳（补充说明，确实是角落处网络信号不佳，也有可能与黄铜散热片有关）。在进行图片采集和传输时卡顿比较严重，但此时只有单核负载重，可以考虑在代码上做些优化，使用线程池来处理这些图片消息。另外，410满负荷运行还是挺能发热的，所以不得不花了8元重金为它求得两枚黄铜散热片。

最后给上一张全家福，每位上面都运行着eight。

![eight全家福](/eight/assets/images/family.jpg){:.rounded width="1280px" style="display:block; margin-left:auto; margin-right:auto"}

### 意义

这事情起始于无聊，结局却不那么无聊。仔细想起来，棒子的这个场景还是颇具意义的。

![eight无处不在](/eight/assets/images/eight-everywhere.png){:.rounded width="1280px" style="display:block; margin-left:auto; margin-right:auto"}

如图，它其实提供了一种广域的系统部署和操纵模式。

- wifi棒子（或类似产品）在部署了ROS2后，等同于神经中枢，能够控制同一子网（往往就是其热点）内的各种ROS设备，覆盖范围视网络质量而定，通常在数公顷范围内

- 子网内其它设备无须具备强大的计算能力，只须成本低廉的单片机（如esp32，带wifi和蓝牙，某宝零售价在10元/片上下），能够联网和部署microros，即可将控制权移交给神经中枢。既可以降低成本，也便于中枢统一协调运行

- 在部署eight后，棒子可以通过自带的4g（或其它联网方式），在移动网络覆盖的范围内联系到互联网上的中央控制系统。由是，中央控制系统（大脑）便掌控了分布于各地的中枢神经的最高控制权，也就可以控制各个末梢神经（边缘节点）的运行（但并不必须现场指挥）

- 由于eight是动态部署系统，而且体积极小（模块往往在数十至数百KB范围内），一旦部署脱网仍可运行。所以很适合网络带宽差，网络条件不稳定的场景（如移动网络）

- 将系统动态下发，而不是采取系统运行在云端，同时进行数据上交的好处是在保证业务逻辑随时根据现场状况而变的前提下，大大降低带宽需求，避免网络造成系统失能的影响。这对基于不稳定网络环境的应用场景是必须的

- 系统下发后在部署点上本地运行，既减少带宽占用，又构造了更为安全稳定的指挥体系，提高了现场反应速度，也避免了干扰

- 指挥中枢（棒子）一般具备一定的性能（cpu，gpu，ram等），可以运行一些复杂的算法（如基于opencv的机器视觉、模式识别等），这样可以充分利用部署点的计算能力来现场指挥，无须凡事向上请示

- 只有特定的，需要采集上交的数据才过滤处理后提交，减轻了服务端压力和网络开销

也就是，我们能够通过一根棒子，在世界任何一个地方，在数公顷范围内，花几秒钟就能搭建起一套远程控制体系，既能自动运作，又能远程管控，还能随时变化。这一切，只需要9块9（网费自理）。