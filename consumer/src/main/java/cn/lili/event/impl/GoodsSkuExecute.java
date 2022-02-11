package cn.lili.event.impl;


import cn.lili.event.GoodsCommentCompleteEvent;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.entity.dos.MemberEvaluation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 商品SKU变化
 *
 * @author Chopper
 * @since 2020-07-03 11:20
 */
@Service
public class GoodsSkuExecute implements GoodsCommentCompleteEvent {

    /**
     * 商品
     */
    @Autowired
    private GoodsSkuService goodsSkuService;


    @Override
    public void goodsComment(MemberEvaluation memberEvaluation) {
        goodsSkuService.updateGoodsSkuCommentNum(memberEvaluation.getSkuId());
    }
}
