package cn.lili.modules.promotion.service;

import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.promotion.entity.dto.GiftCardCashActivityDTO;
import cn.lili.modules.promotion.entity.vos.PickupCardSkuVO;

import java.util.List;

/**
 * 提货卡活动规则与商品展示服务，仅负责选品规则、商品资格及回显，不依赖订单或交易服务。
 *
 * @author Bulbasaur
 * @since 2026-11-06
 */
public interface PickupCardRuleService {

    /**
     * 校验活动规则及候选商品，并归一化兑换种数和提货卡面额。
     *
     * @param dto 待保存的活动参数，校验通过后原地更新归一化字段
     */
    void validateActivity(GiftCardCashActivityDTO dto);

    /**
     * 校验候选集合及兑换种数；提交兑换时额外校验所选 SKU 不重复且属于候选集合。
     *
     * @param mode     ONE 任选一种、ALL 全部兑换、ANY 任选指定种数
     * @param count    ANY 模式指定的 SKU 种数，ONE 和 ALL 按模式计算
     * @param allowed  候选 SKU ID 集合，须为 1～50 个不同 SKU
     * @param selected 兑换提交的 SKU ID 集合，每种一件
     * @param redeem   是否执行提交兑换的选品校验
     */
    void validateSelection(String mode, Integer count, List<String> allowed, List<String> selected, boolean redeem);

    /**
     * 根据候选 SKU 生成实时展示数据，失效商品保留占位，不返回内部成本字段。
     *
     * @param skuIds 候选 SKU ID 的 JSON 字符串数组；为空时返回空列表
     * @return 按配置顺序返回的商品及当前可选状态，不预占库存
     */
    List<PickupCardSkuVO> goods(String skuIds);

    /**
     * 判断 SKU 是否为审核通过且已上架的普通实物商品，并校验销售价合法性。
     *
     * @param sku 商品 SKU，可为空；此方法不检查库存数量
     * @return 商品未删除且价格为有限非负数等资格均满足时返回 true
     */
    boolean isValidSku(GoodsSku sku);
}
