package cn.lili.common.fulu.core.utils;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

public class JSONUtil {
  private static Gson gson = new Gson();

  /**
   * 将 JSON 字符串转为 Java 对象
   */
  public static <T> T fromJSON(String json, Class<T> type) {
    T obj;
    try {
      obj = gson.fromJson(json, type);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
    return obj;
  }

  /**
   * json字符串转list或者map
   */
  public static <T> T fromJSON(String json, TypeToken<T> typeToken) {
    return gson.fromJson(json, typeToken.getType());
  }

  /**
   * 将 Java 对象转为 JSON 字符串
   */
  public static <T> String toJSON(T obj) {
    String jsonStr;
    try {
      jsonStr = gson.toJson(obj);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
    return jsonStr;
  }
}
