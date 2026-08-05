package cn.lili.modules.distribution.service;

import cn.lili.modules.distribution.entity.dos.DistributionShareEvent;
import cn.lili.modules.distribution.entity.dto.DistributionShareEventDTO;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 分销商品分享行为记录服务。
 * <p>
 * 记录分销员分享商品及用户通过分享链接访问的行为，用于归因与统计。
 * </p>
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
public interface DistributionShareEventService extends IService<DistributionShareEvent> {

    /**
     * 记录分销员分享商品事件。
     *
     * @param dto 分享事件参数
     */
    void recordShare(DistributionShareEventDTO dto);

    /**
     * 服务端记录商品分享（如生成海报），并返回 shareId 供访问归因。
     *
     * @param distributionId  分销员ID
     * @param goodsId         商品ID
     * @param skuId           SKU ID
     * @param channel         分享渠道
     * @param triggerPage     触发页面
     * @param shareButtonType 分享按钮类型
     * @return 分享标识
     */
    String recordGoodsShare(String distributionId, String goodsId, String skuId,
                            String channel, String triggerPage, String shareButtonType);

    /**
     * 服务端记录分享链接访问（商品详情页进入时调用）。
     *
     * @param distributionId 分销员ID
     * @param goodsId        商品ID
     * @param skuId          SKU ID
     * @param shareId        分享标识（可选）
     * @param visitorId      访客标识
     * @param memberId       会员ID（已登录时）
     */
    void recordGoodsVisit(String distributionId, String goodsId, String skuId,
                          String shareId, String visitorId, String memberId);
}
 