package cn.lili.common.fulu.core;

/**
 * @Auther: chenYing
 * @Date: 2019/8/26 0026 15:08
 */
public interface MethodConst {
  //卡密下单接口method方法
  String OPEN_API_CARD_ORDER_ADD = "fulu.order.card.add";
  // 根据话费查询归属地和城市编码，面值，城市等信息
  String OPEN_API_CHECK_PHONE = "fulu.mobile.info.get";
  // 直充下单接口method方法
  String OPEN_API_DIRECT_ORDER_ADD = "fulu.order.direct.add";
  // 获取商品信息接口method方法
  String OPEN_API_GOODS_GET = "fulu.goods.info.get";
  // 获取商品模板信息接口method方法
  String OPEN_API_GOODS_TEMPLATE_GET = "fulu.goods.template.get";
  // 查单接口method方法
  String OPEN_API_ORDER_GET = "fulu.order.info.get";
  // 话费下单接口method方法
  String OPEN_API_PHONE_ORDER_ADD = "fulu.order.mobile.add";
  // 流量下单接口method方法
  String OPEN_API_TRAFFIC_ORDER_ADD = "fulu.order.data.add";
  // 获取用户信息接口method方法
  String OPEN_API_USER_INFO_GET = "fulu.user.info.get";
  //获取商品列表method方法
  String OPEN_API_GOODS_LIST = "fulu.goods.list.get";


}
