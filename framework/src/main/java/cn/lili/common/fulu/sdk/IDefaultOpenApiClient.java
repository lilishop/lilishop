package cn.lili.common.fulu.sdk;



import cn.lili.common.fulu.model.CommonRequest;

import java.util.concurrent.Future;

/**
 * 默认OpenApi客户请求接口
 *
 * @Auther: chenYing
 * @Date: 2019/8/19 0019 16:40
 */
public interface IDefaultOpenApiClient {

  /**
   * 执行请求，同步方法
   *
   * @return String
   */
  String excute();

  /**
   * 执行请求，异步方法
   *
   * @return Future<String>
   */
  Future<String> excuteAsync();

  /**
   * 设置业务参数
   *
   * @param bizContent
   */
  @Deprecated
  void setBizContent(String bizContent);

  /**
   * 设置业务参数
   */
  void setBizObject(CommonRequest bizModel);

}
