package cn.lili.modules.promotion.service;


import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.KanJiaActivity;
import cn.lili.modules.promotion.entity.dos.KanJiaActivityGoods;
import cn.lili.modules.promotion.entity.dto.KanJiaActivityGoodsDTO;
import cn.lili.modules.promotion.entity.dto.KanJiaActivityGoodsOperationDTO;
import cn.lili.modules.promotion.entity.vos.KanJiaActivityGoodsParams;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;


/**
 * 砍价业务层
 *
 * @author qiuqiu
 * @date 2021/7/1 9:45 上午
 */
public interface KanJiaActivityGoodsService extends IService<KanJiaActivityGoods> {


    /**
     * 添加砍价活动商品
     *
     * @param kanJiaActivityGoodsDTOS 砍价商品
     * @return 是否添加成功
     */
    boolean add(KanJiaActivityGoodsOperationDTO kanJiaActivityGoodsDTOS);

    /**
     * 查询砍价活动商品分页信息
     *
     * @param kanJiaActivityGoodsParams 砍价活动商品
     * @param pageVO                    分页信息
     * @return 砍价商品
     */
    IPage<KanJiaActivityGoodsDTO> getForPage(KanJiaActivityGoodsParams kanJiaActivityGoodsParams, PageVO pageVO);

    /**
     * 查询砍价活动商品
     *
     * @param goodsId 砍价活动商品id
     * @return 砍价活动商品信息
     */
    KanJiaActivityGoodsDTO getKanJiaGoodsDetail(String goodsId);

    /**
     * 修改看见商品信息
     *
     * @param kanJiaActivityGoodsDTO 砍价商品信息
     * @return 是否修改成功
     */
    boolean updateKanJiaActivityGoods(KanJiaActivityGoodsDTO kanJiaActivityGoodsDTO);

    /**
     * 删除砍价商品
     *
     * @param ids 砍价商品ids
     * @return 是否删除成功
     */
    boolean deleteKanJiaGoods(List<String> ids);

    /**
     * 根据skuID查询当前进行的砍价商品信息
     *
     * @param skuId 商品skuId
     * @return
     */
    KanJiaActivityGoodsDTO getKanJiaGoodsBySku(String skuId);

}