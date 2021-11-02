## 🔥 Lilishop B2B2C商城系统

##### 🌹  开源不易，如有帮助请点Star 



#### 欢迎交流需求，交流业务，交流技术（基础问题自行解决，进群先看文档后提问）

##### 交流 qq 1群 961316482（已满）

##### 交流 qq 2群 875294241



##### 商城 公众号/小程序体验，扫描二维码

![image-20210511171611793](https://pickmall.cn/assets/imgs/h5-qrcode.png)

[![star](https://gitee.com/beijing_hongye_huicheng/lilishop/badge/star.svg?theme=dark)](https://gitee.com/beijing_hongye_huicheng/lilishop/stargazers)
&nbsp;&nbsp;![github](https://img.shields.io/github/stars/hongyehuicheng/lilishop.svg?style=social&logo=#181717)


### 🔥 商城介绍
**官网**：https://pickmall.cn

Lilishop 商城系统 基于SpringBoot 研发，B2B2C多用户商城系统，前端使用 Vue、uniapp开发 **系统全端全部代码开源**

业务兼容O2O商城/B2B商城/B2B2C商城/F2B2C商城/S2B2C商城。支持小程序商城、H5商城、APP商城、 PC商城。


商城前后端分离、支持分布式部署。

商城包含 会员模块、**第三方登录模块**、**第三方支付模块**、**楼层装修模块**、订单模块、分销模块、文章模块、系统设置模块、流量分析模块

商城包含各种中间件、搜索引擎、多级缓存、分布式事务、分布式任务调度等，支持Docker，支持k8s。是一款高性能，支持高并发的商城系统。

##### 商城 API/消费者 聚合版
api不需要单独部署，只需启动一个jar包就可以正常运转 如有需要，可以点击跳转https://gitee.com/beijing_hongye_huicheng/lilishop-simplify

### ☃️ 商城 开发/使用/常见问题 帮助文档

https://docs.pickmall.cn

### 💧 开源商城项目地址(gitee)

**API（商城所有API）**：https://gitee.com/beijing_hongye_huicheng/lilishop.git

**UI（商城管理端/商家端/买家PC端）**： https://gitee.com/beijing_hongye_huicheng/lilishop-ui.git

**uniapp（商城移动端，支持小程序/APP/H5）**：https://gitee.com/beijing_hongye_huicheng/lilishop-uniapp.git

**docker一键部署（商城部署脚本）**：https://gitee.com/beijing_hongye_huicheng/docker.git

### 💧 开源商城项目地址(github)

**API（商城所有API）**：https://github.com/hongyehuicheng/lilishop.git

**UI（商城管理端/商家端/买家PC端）**： https://github.com/hongyehuicheng/lilishop-ui.git

**uniapp（商城移动端，支持小程序/APP/H5）**：https://github.com/hongyehuicheng/lilishop-uniapp.git

**docker一键部署（商城部署脚本）**：https://github.com/hongyehuicheng/docker.git


☃️ UI 项目下3个文件夹 buyer：买家PC端，seller：商家端，manager：后台管理端


### 💧 演示地址(手机验证码为 ‘111111’)

**商城管理端**：https://admin-b2b2c.pickmall.cn 账号：admin/123456

**商城店铺后台**：https://store-b2b2c.pickmall.cn 账号：13011111111/111111

**商城PC页面**：https://pc-b2b2c.pickmall.cn

**商城移动端（请使浏览器手机模式，或者用手机浏览器打开）**：https://m-b2b2c.pickmall.cn

**小程序/公众号**：扫描二维码

![image-20210511171611793](https://pickmall.cn/assets/imgs/h5-qrcode.png)

### 🚙 3行命令搭建本地商城（注：只能本机访问，如需调整，请自行操作镜像）

温馨提示：由于服务中间件较多，如果笔记本环境启动内存没有32g可能无法启动成功（macbookpro 2020 16g内存启动无法成功），台式机在16g内存、AMD 3700x 的ubuntu系统成功运行。



#####  docker环境安装 [点击跳转](https://docs.pickmall.cn/deploy/%E8%BF%90%E8%A1%8C%E7%8E%AF%E5%A2%83%E5%87%86%E5%A4%87.html)



##### 下载docker-compose脚本
`git clone https://gitee.com/beijing_hongye_huicheng/docker.git `

##### 部署商城所需中间件
`docker-compose up -d`

##### 部署商城应用
`docker-compose -f docker-compose-application.yml up -d`



PS:商城数据库单独部署 https://gitee.com/beijing_hongye_huicheng/docker/tree/master/init/mysql 这里有与tag版本一致的sql，根据tag获取sql，如果使用master代码，则需要在lilishop项目根目录的DB目录中，获取对应的升级sql。

##### 商城 API/UI 地址

| API           | 地址            |
| -------------- | --------------- |
| 商城买家API  | http://127.0.0.1:8888     |
| 商城商家API   | http://127.0.0.1:8889      |
| 商城管理端API     | http://127.0.0.1:8887   |
| 商城基础API  | http://127.0.0.1:8890     |

| 前端演示         | 地址            |
| -------------- | --------------- |
| 商城PC端    | http://127.0.0.1:10000     |
| 商城WAP端     | http://127.0.0.1:10001      |
| 商城卖家端    | http://127.0.0.1:10002   |
| 商城管理端     | http://127.0.0.1:10003     |





### ⚾️ 功能列表



#### 🥎 商城平台功能

![平台功能](https://pickmall.cn/assets/imgs/other/managerList.jpg)



#### 🥎 商城卖家功能

![商家端功能](https://pickmall.cn/assets/imgs/other/storeList.jpg)





### 🧩 商城前端功能展示



#### ⚽️ 商城移动端

<img src="https://pickmall.cn/assets/imgs/other/app.gif" alt="移动端功能展示" style="zoom:50%;" />



#### ⚽️ 商城管理端

![管理端功能展示](https://pickmall.cn/assets/imgs/other/manager.gif)


### 商城技术选型

#### 🥅 架构图

![架构](https://lili-system.oss-cn-beijing.aliyuncs.com/docs/%E6%9E%B6%E6%9E%84.png)

##### 🕹 后台技术选型

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

##### 🖥 前端-运营后台、店铺后台

| 说明       | 框架       | 说明       | 框架    |
| ---------- | ---------- | ---------- | ------- |
| 构建工具   | webpack    | JS版本     | ES6     |
| 基础JS框架 | Vue.js     | 视频播放器 | Dplayer |
| 路由管理   | Vue Router | 状态管理   | Vuex    |
| 基础UI库   | iView      | UI界面基于 | iView   |
| 网络请求   | axios      |            |         |

##### 📱前端-移动端

| 说明      | 架构    | 说明     | 架构    |
| --------- | ------- | -------- | ------- |
| 基础UI库  | uViewui | 基础框架 | uni-app |
| CSS预处理 | scss    | 地图引擎 | amap    |

### 🌟 版本升级

```
商城后续会持续版本升级，修复bug，完善功能，覆盖更多业务场景 o2o/b2b/s2b2b2c/跨境电商

后续会考虑推出微服务商城系统/商城中台等
```


### ⚠️ 开源须知
1.仅允许用于个人学习研究使用.

2.禁止将本开源的代码和资源进行任何形式任何名义的出售.

3.软件受国家计算机软件著作权保护（登记号：2021SR0805085）。

4.限制商用，如果需要商业使用请联系我们。QQ3409056806.


### 🐧 交流群

##### 官方qq 1群 961316482（已满）
##### 官方qq 2群 875294241

