package cn.lili.common.fulu.model;

/**
 * 用户信息input dto
 *
 * @Auther: chenYing
 * @Date: 2019/8/19 0019 16:06
 */
public class InputUserDto extends CommonRequest {

  public InputUserDto() {
    super();
    setMethod("fulu.user.info.get");
  }
}
