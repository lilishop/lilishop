package cn.lili.modules.distribution.service;

import cn.lili.modules.distribution.entity.dto.DistributionFeaturedGoodsSettingSaveDTO;
import cn.lili.modules.distribution.entity.vos.DistributionFeaturedGoodsItemVO;
import cn.lili.modules.distribution.entity.vos.DistributionFeaturedGoodsSettingVO;

import java.util.List;

/**
 * 分销主推商品服务
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
public interface DistributionFeaturedGoodsService {

    /**
     * 获取主推商品设置详情（含自定义商品预览）。
     *
     * @return 设置详情
     */
    DistributionFeaturedGoodsSettingVO getSettingDetail();

    /**
     * 保存主推商品设置。
     *
     * @param dto 设置参数
     */
    void saveSetting(DistributionFeaturedGoodsSettingSaveDTO dto);

    /**
     * 买家端获取分销中心展示的主推商品列表。
     *
     * @return 主推商品列表
     */
    List<DistributionFeaturedGoodsItemVO> listFeaturedForBuyer();
}
