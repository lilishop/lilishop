package cn.lili.event.impl;


import cn.lili.event.GoodsCommentCompleteEvent;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.entity.dos.MemberEvaluation;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 商品SKU变化
 *
 * @author Chopper
 * @date 2020-07-03 11:20
 */
@Service
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class GoodsSkuExecute implements  GoodsCommentCompleteEvent {

    //商品
    private final GoodsSkuService goodsSkuService;


    @Override
    public void goodsComment(MemberEvaluation memberEvaluation) {
        goodsSkuService.updateGoodsSkuCommentNum(memberEvaluation.getSkuId());
    }
}
