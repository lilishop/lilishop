package cn.lili.common.fulu.core.http;


import cn.lili.common.fulu.core.utils.HttpUtil;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

/**
 * @Auther: chenYing
 * @Date: 2019/8/19 0019 16:16
 */
public class FuluClient implements IFuluClient {
  private static volatile ExecutorService executor;
  private String url;

  public FuluClient() {
    this.url = "http://openapi.fulu.com/api/getway";
  }

  public FuluClient(String url) {
    this.url = url;
  }

  /**
   * 创建线程池
   */
  private static void createThreadPool() {
    if (executor == null) {
      synchronized (FuluClient.class) {
        if (executor == null) {
          executor = Executors.newCachedThreadPool();
        }
      }

    }
  }

  @Override
  public String send(final String postData) {
    String result = HttpUtil.sendPostJson(this.url, postData);
    return result;
  }

  @Override
  public Future<String> sendAsync(final String postData) {
    createThreadPool();
    return executor.submit(new Callable<String>() {
      @Override
      public String call() throws Exception {
        String result = HttpUtil.sendPostJson(url, postData);
        return result;
      }
    });
  }
}
