package cn.lili.common.fulu.core.http;

import java.util.concurrent.Future;

/**
 * @Auther: chenYing
 * @Date: 2019/8/19 0019 16:15
 */
public interface IFuluClient {
  /**
   * 同步请求openapi2.0
   *
   * @param postData
   * @return String
   */
  String send(final String postData);


  /**
   * 异步请求openapi2.0
   *
   * @param postData
   * @return Future<String>
   */
  Future<String> sendAsync(final String postData);

}
