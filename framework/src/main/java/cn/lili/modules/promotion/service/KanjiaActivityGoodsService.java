package cn.lili.modules.promotion.service;


import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.KanjiaActivityGoods;
import cn.lili.modules.promotion.entity.dto.KanjiaActivityGoodsDTO;
import cn.lili.modules.promotion.entity.dto.KanjiaActivityGoodsOperationDTO;
import cn.lili.modules.promotion.entity.dto.search.KanjiaActivityGoodsParams;
import cn.lili.modules.promotion.entity.vos.kanjia.KanjiaActivityGoodsListVO;
import cn.lili.modules.promotion.entity.vos.kanjia.KanjiaActivityGoodsVO;
import com.baomidou.mybatisplus.core.metadata.IPage;

import java.util.List;


/**
 * 砍价业务层
 *
 * @author qiuqiu
 * @date 2021/7/1 9:45 上午
 */
public interface KanjiaActivityGoodsService extends AbstractPromotionsService<KanjiaActivityGoods> {


    /**
     * 添加砍价活动商品
     *
     * @param kanJiaActivityGoodsDTOS 砍价商品
     * @return 是否添加成功
     */
    Boolean add(KanjiaActivityGoodsOperationDTO kanJiaActivityGoodsDTOS);

    /**
     * 查询砍价活动商品分页信息
     *
     * @param kanJiaActivityGoodsParams 砍价活动商品
     * @param pageVO                    分页信息
     * @return 砍价商品
     */
    IPage<KanjiaActivityGoodsListVO> kanjiaGoodsVOPage(KanjiaActivityGoodsParams kanJiaActivityGoodsParams, PageVO pageVO);

    /**
     * 查询砍价活动商品
     *
     * @param goodsId 砍价活动商品id
     * @return 砍价活动商品信息
     */
    KanjiaActivityGoodsDTO getKanjiaGoodsDetail(String goodsId);

    /**
     * 根据SkuId获取正在进行中的砍价商品
     *
     * @param skuId 商品规格Id
     * @return 砍价商品
     */
    KanjiaActivityGoods getKanjiaGoodsBySkuId(String skuId);

    /**
     * 查询砍价活动商品VO
     *
     * @param id 砍价活动商品ID
     * @return 砍价活动商品
     */
    KanjiaActivityGoodsVO getKanJiaGoodsVO(String id);

    /**
     * 修改看见商品信息
     *
     * @param kanjiaActivityGoodsDTO 砍价商品信息
     * @return 是否修改成功
     */
    boolean updateKanjiaActivityGoods(KanjiaActivityGoodsDTO kanjiaActivityGoodsDTO);

    /**
     * 根据商品id删除促销活动
     * @param goodsIds
     */
    void deleteByGoodsIds(List<String> goodsIds);
}