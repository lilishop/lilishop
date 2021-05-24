## Lilishop B2B2C商城系统

##### 官方公众号 & 开源不易，如有帮助请点Star
![image-20210511171611793](https://pickmall.cn/assets/imgs/h5-qrcode.png)

[![star](https://gitee.com/beijing_hongye_huicheng/lilishop/badge/star.svg?theme=dark)](https://gitee.com/beijing_hongye_huicheng/lilishop/stargazers)


### 介绍
**官网**：https://pickmall.cn

Lilishop 是一款Java开发，基于SpringBoot研发的B2B2C多用户商城，前端使用 Vue、uniapp开发 **系统全端全部代码开源**

产品前后端分离、支持分布式部署。

商城展示端包含 PC、H5、微信小程序、APP。

商城包含 会员模块、**第三方登录模块**、**第三方支付模块**、**楼层装修模块**、订单模块、分销模块、文章模块、系统设置模块、流量分析模块

系统包含各种中间件、搜索引擎、多级缓存、分布式事务、分布式任务调度等，支持Docker，支持k8s。是一款高性能，支持高并发等商城系统。

开箱即用，简单配置即可部署一套属于您的系统。

### 文档

**产品文档**（需求、架构、使用、部署、开发）：https://docs.pickmall.cn


### 项目链接(gitee)

**Java后台**：https://gitee.com/beijing_hongye_huicheng/lilishop.git

**Vue后台前端**： https://gitee.com/beijing_hongye_huicheng/lilishop-ui.git

**Uni-app**：https://gitee.com/beijing_hongye_huicheng/lilishop-uniapp.git

**docker一键部署**：https://gitee.com/beijing_hongye_huicheng/docker.git

### 项目链接(github)

**Java后台**：https://github.com/hongyehuicheng/lilishop.git

**Vue后台前端**： https://github.com/hongyehuicheng/lilishop-ui.git

**Uni-app**：https://github.com/hongyehuicheng/lilishop-uniapp.git

**docker一键部署**：https://github.com/hongyehuicheng/docker.git

### 演示地址

**运营后台**：https://admin-b2b2c.pickmall.cn 账号：admin/123456

**店铺后台**：https://store-b2b2c.pickmall.cn 账号：13011111111/111111

**用户前台**：https://pc-b2b2c.pickmall.cn

**移动端**：https://m-b2b2c.pickmall.cn

![image-20210511171611793](https://pickmall.cn/assets/imgs/h5-qrcode.png)

### 3行命令搭建本地环境

温馨提示：由于服务较多，如果笔记本环境启动内存没有32g可能无法启动成功（macbookpro 2020 16g内存启动无法成功），台式机在16g内存、AMD 3700x 的ubuntu系统成功运行。

##### 下载docker脚本
`git clone https://gitee.com/beijing_hongye_huicheng/docker.git `
##### 部署基础环境
`docker-compose up -d`

##### 部署应用
`docker-compose -f docker-compose-application.yml up -d`



PS:单独部署的话，数据库文件访问这里：https://gitee.com/beijing_hongye_huicheng/docker/tree/master/init/mysql

##### 各个地址

| API           | 地址            |
| -------------- | --------------- |
| 买家API    | http://127.0.0.1:8888     |
| 商家API     | http://127.0.0.1:8889      |
| 管理端API       | http://127.0.0.1:8887   |
| 通用API    | http://127.0.0.1:8890     |

| 前端演示         | 地址            |
| -------------- | --------------- |
| PC       | http://127.0.0.1:10000     |
| WAP        | http://127.0.0.1:10001      |
| 商家       | http://127.0.0.1:10002   |
| 管理端       | http://127.0.0.1:10003     |





### 功能列表



#### 平台功能

![平台功能](https://pickmall.cn/assets/imgs/other/managerList.jpg)



#### 商家端功能

![商家端功能](https://pickmall.cn/assets/imgs/other/storeList.jpg)





### 功能展示



#### 移动端

<img src="https://pickmall.cn/assets/imgs/other/app.gif" alt="管理端功能展示" style="zoom:50%;" />

<img src="https://pickmall.cn/assets/imgs/other/app.gif" alt="移动端功能展示" style="zoom:50%;" />



#### 管理端

![管理端功能展示](https://pickmall.cn/assets/imgs/other/manager.gif)


### 技术选型

#### 架构图

![架构](https://lili-system.oss-cn-beijing.aliyuncs.com/docs/%E6%9E%B6%E6%9E%84.png)

##### Java后台

| 说明           | 框架            | 说明           |               |
| -------------- | --------------- | -------------- | ------------- |
| 基础框架       | Spring Boot     | MVC框架        | Spring MVC    |
| 持久框架       | Mybatis-Plus    | 程序构建       | Maven         |
| 关系型数据库   | MySQL           | 消息中间件AMQP | RocketMQ      |
| 缓存           | Redis +MongoDB  | 搜索引擎       | Elasticsearch |
| 安全框架       | Spring Security | 数据库连接池   | Druid         |
| 数据库分库分表 | sharding        | 定时任务       | xxl-job       |
| 负载均衡       | Nginx           | 静态资源       | 阿里云OSS     |
| 短信           | 阿里云短信      | 认证           | JWT           |
| 日志处理       | Log4j           | 接口规范       | RESTful       |

##### 前端-运营后台、店铺后台

| 说明       | 框架       | 说明       | 框架    |
| ---------- | ---------- | ---------- | ------- |
| 构建工具   | webpack    | JS版本     | ES6     |
| 基础JS框架 | Vue.js     | 视频播放器 | Dplayer |
| 路由管理   | Vue Router | 状态管理   | Vuex    |
| 基础UI库   | iView      | UI界面基于 | iView   |
| 网络请求   | axios      |            |         |

##### 前端-移动端

| 说明      | 架构    | 说明     | 架构    |
| --------- | ------- | -------- | ------- |
| 基础UI库  | uViewui | 基础框架 | uni-app |
| CSS预处理 | scss    | 地图引擎 | amap    |

### 升级计划

#### 计划每个月发布一个版本，具体时间可能有出入

时间：2021年6月15日

```
新增功能：
1.微信小程序直播
2.优惠券活动
3.新人赠券
4.准确发券
5.用户等级
6.数据导出
7.订单批量
8.APP版本升级检测
9.积分商城

功能优化：
1.优惠券有效期增加类型：设置领取后*内有效。
2.秒杀活动设置为每天开启，需设置秒杀活动开启时间。
3.店铺配送模板，配送地区如果选择省份则下方的市级地址不展示。
4.店铺配送模板支持，店铺包邮。
5.普通商品设置去除卖家承担运费。

```

时间：2021年7月15日

```
新增功能：
1.会员权益
2.支持用户升级会员
3.供求单
4.IM：腾讯云智服
5.服务商品
6.店铺支持订单核销
7.店铺自提点
功能优化：
1.用户分享商城、关注店铺、邀请新用户可获取积分、经验值。
```

时间：2021年8月16日

```
新增功能：
1.微淘功能
2.店铺移动端
3.店铺发货单
```

时间：2021年9月15日

```
新增功能：
增加供应商功能
```

### 版本升级

```
后续会持续版本升级，修复bug，完善功能，覆盖更多业务场景 o2o/b2b/s2b2b2c/跨境电商

后续会考虑推出微服务/中台等 企业级版本
```

### 技术亮点


    1.后端框架基于Springboot，构建基于maven，持久层使用MyBatisPlus。使用elasticsearch、redis、mongodb、rocketmq 等各种中间健。都是主流架构，轻松应对各种环境。
    
    2.支持集群、分布式，支持docker 轻松部署，解决各种复杂场景！
    
    3.代码模块清晰，主要分为三端api（买家、卖家、管理），各端API互相隔离，自己鉴权，自己操作业务。
    
    4.使用阿里开源的RocketMQ，基于mq解决各种并发场景，解决事务一致性，解决搞并发延迟场景问题。
    
    5.项目使用多级缓存，应用不同场景，redis缓存业务数据、mongodb缓存关系型多对多关系问题、nginx缓存高频访问低频修改的页面。
    
    6.支持各种联合登陆，支持各种客户端的支付问题，灵活配置灵活开启。
    
    7.内置完善的楼层装修机制，各种拖拉拽，维护跳转页面或外网，即便是一个什么都不懂的运营也可以轻松掌握。
    
    8.内置阿里短信接口，可以在线申请短信模版。内置阿里oss系统，可以对文件执行各种操作。oss商家端资源相互隔离。
    
    10.强大的统计报表，统计效果，可以实现各个场景，包含在线人数，历史在线人数，活跃人数等信息。
    
    11.标准Api接口、提供swagger文档，快速二开。
    
    12.分布式调度任务中心，解决分布式定时任务多次执行问题。
    
    13.代码注释完善，快速上手。
    
    14.非移动端采用IView框架，各种自定义插件、选择器实现。移动端采用uniapp，一次编写，全端使用
    
    15.已经对接好各种第三方插件，支持各种复杂等联合登陆，联合支付等场景。


### 开源须知
1.仅允许用于个人学习研究使用.

2.禁止将本开源的代码和资源进行任何形式任何名义的出售.

3.限制商用，如果需要商业使用请联系我们。QQ3409056806.

### 交流群

**QQ群**：961316482

