package cn.lili.modules.kdBrid.service;

/**
 * 快递鸟电子面单业务层实现
 *
 * @author chc
 * @since 2022-4-12 10:12:43
 */
public interface KdNiaoService {
    /**
     * 生成电子面单
     * @param orderSn 订单编号
     * @param logisticsId 物流公司
     * @return 电子面单模板
     */
    String createElectronicsFaceSheet(String orderSn, String logisticsId) throws Exception;
}
