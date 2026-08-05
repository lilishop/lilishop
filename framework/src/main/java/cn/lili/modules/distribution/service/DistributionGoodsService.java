package cn.lili.modules.distribution.service;

import cn.lili.modules.distribution.entity.dos.DistributionGoods;
import cn.lili.modules.distribution.entity.dto.DistributionGoodsAuditDTO;
import cn.lili.modules.distribution.entity.dto.DistributionGoodsBatchCheckDTO;
import cn.lili.modules.distribution.entity.dto.DistributionGoodsSearchParams;
import cn.lili.modules.distribution.entity.vos.DistributionGoodsPosterVO;
import cn.lili.modules.distribution.entity.vos.DistributionGoodsShareVO;
import cn.lili.modules.distribution.entity.vos.DistributionGoodsVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 分销商品业务层。
 * <p>
 * 负责商家选品加入分销池、平台审核、佣金设置及分销员选品查询等能力。
 * </p>
 *
 * @author pikachu
 * @since 2020-03-24
 */
public interface DistributionGoodsService extends IService<DistributionGoods> {

    /**
     * 管理端分页查询分销商品（含 SKU、品牌、类目等扩展信息）。
     *
     * @param distributionGoodsSearchParams 查询条件
     * @return 分销商品分页
     */
    IPage<DistributionGoodsVO> goodsPage(DistributionGoodsSearchParams distributionGoodsSearchParams);

    /**
     * 按条件查询分销商品列表。
     *
     * @param distributionGoodsSearchParams 查询条件
     * @return 分销商品列表
     */
    List<DistributionGoods> getDistributionGoodsList(DistributionGoodsSearchParams distributionGoodsSearchParams);

    /**
     * 按条件查询单条分销商品。
     *
     * @param distributionGoodsSearchParams 查询条件
     * @return 分销商品
     */
    DistributionGoods getDistributionGoods(DistributionGoodsSearchParams distributionGoodsSearchParams);

    /**
     * 按条件删除分销商品。
     *
     * @param distributionGoodsSearchParams 删除条件
     * @return 是否删除成功
     */
    boolean deleteDistributionGoods(DistributionGoodsSearchParams distributionGoodsSearchParams);

    /**
     * 根据分销商品 ID 获取详情。
     *
     * @param id 分销商品 ID
     * @return 分销商品
     */
    DistributionGoods distributionGoodsVO(String id);

    /**
     * 根据 SKU ID 获取分销商品。
     *
     * @param skuId SKU ID
     * @return 分销商品
     */
    DistributionGoods distributionGoodsVOBySkuId(String skuId);

    /**
     * 批量根据 SKU ID 获取分销商品。
     *
     * @param skuIds SKU ID 集合
     * @return 分销商品列表
     */
    List<DistributionGoods> distributionGoods(List<String> skuIds);

    /**
     * 商家将 SKU 加入分销商品池并设置佣金。
     *
     * @param skuId      SKU ID
     * @param commission 佣金金额
     * @param storeId    店铺 ID
     * @return 分销商品
     */
    DistributionGoods checked(String skuId, Double commission, String storeId);

    /**
     * 商家批量将 SKU 加入分销商品池并设置佣金。
     *
     * @param batchDTO 批量选品参数
     * @param storeId  店铺 ID
     * @return 分销商品列表
     */
    List<DistributionGoods> batchChecked(DistributionGoodsBatchCheckDTO batchDTO, String storeId);

    /**
     * 修改分销商品佣金。
     *
     * @param distributionGoods 分销商品
     * @param commission        佣金金额
     * @return 分销商品
     */
    DistributionGoods updateCommission(DistributionGoods distributionGoods, Double commission);

    /**
     * 平台审核分销商品（通过/拒绝）。
     * <p>仅审核通过的商品可被分销员选品并参与佣金结算。</p>
     *
     * @param auditDTO 审核参数
     */
    void auditDistributionGoods(DistributionGoodsAuditDTO auditDTO);

    /**
     * 获取分销商品分享上下文（推广图文）
     *
     * @param skuId          规格 ID
     * @param goodsId        商品 ID
     * @param distributionId 分销员 ID
     * @return 分享上下文
     */
    DistributionGoodsShareVO getShareContext(String skuId, String goodsId, String distributionId);

    /**
     * 获取分销商品海报上下文（生成葵花码并记录分享行为）。
     *
     * @param skuId          规格 ID
     * @param goodsId        商品 ID
     * @param distributionId 分销员 ID
     * @return 海报上下文
     */
    DistributionGoodsPosterVO getPosterContext(String skuId, String goodsId, String distributionId);

    /**
     * 记录分享链接访问（商品详情页进入时调用）。
     *
     * @param skuId          规格 ID
     * @param goodsId        商品 ID
     * @param distributionId 分销员 ID
     * @param shareId        分享标识（可选）
     */
    void recordShareVisit(String skuId, String goodsId, String distributionId, String shareId);

}
