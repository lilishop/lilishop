package cn.lili.common.fulu.core.utils;

import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * @Auther: chenYing
 * @Date: 2019/8/19 0019 15:17
 */
public class DateFormatUtil {
  private static final String DATE_FORMAT = "yyyy-MM-dd HH:mm:ss";

  private DateFormatUtil() {
  }

  public static String currentDateTime() {
    return new SimpleDateFormat(DATE_FORMAT).format(new Date());
  }
}
