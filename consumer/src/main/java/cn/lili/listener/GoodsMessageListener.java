package cn.lili.listener;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.ArrayUtil;
import com.alibaba.fastjson2.JSON;
import cn.lili.common.aop.annotation.RetryOperation;
import cn.lili.common.exception.RetryException;
import cn.lili.event.GoodsCommentCompleteEvent;
import cn.lili.modules.distribution.entity.dos.DistributionGoods;
import cn.lili.modules.distribution.entity.dto.DistributionGoodsSearchParams;
import cn.lili.modules.distribution.service.DistributionGoodsService;
import cn.lili.modules.distribution.service.DistributionSelectedGoodsService;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsCompleteMessage;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.entity.dos.FootPrint;
import cn.lili.modules.member.entity.dos.MemberEvaluation;
import cn.lili.modules.member.service.FootprintService;
import cn.lili.modules.member.service.GoodsCollectionService;
import cn.lili.modules.promotion.service.PromotionService;
import cn.lili.rocketmq.tags.GoodsTagsEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.common.message.MessageExt;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;

/**
 * 商品消息
 */
@Component
@Slf4j
@RocketMQMessageListener(topic = "${lili.data.rocketmq.goods-topic}", consumerGroup = "${lili.data.rocketmq.goods-group}")
public class GoodsMessageListener implements RocketMQListener<MessageExt> {

    @Autowired
    private GoodsService goodsService;

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Autowired
    private FootprintService footprintService;

    @Autowired
    private GoodsCollectionService goodsCollectionService;

    @Autowired
    private List<GoodsCommentCompleteEvent> goodsCommentCompleteEvents;

    @Autowired
    private DistributionGoodsService distributionGoodsService;

    @Autowired
    private DistributionSelectedGoodsService distributionSelectedGoodsService;

    @Autowired
    private PromotionService promotionService;

    @Override
    @RetryOperation
    public void onMessage(MessageExt messageExt) {
        switch (GoodsTagsEnum.valueOf(messageExt.getTags())) {
            case VIEW_GOODS:
                FootPrint footPrint = JSON.parseObject(new String(messageExt.getBody()), FootPrint.class);
                footprintService.saveFootprint(footPrint);
                break;
            case GENERATOR_GOODS_INDEX:
            case GENERATOR_STORE_GOODS_INDEX:
            case UPDATE_GOODS_INDEX_PROMOTIONS:
            case DELETE_GOODS_INDEX_PROMOTIONS:
            case UPDATE_GOODS_INDEX:
            case UPDATE_GOODS_INDEX_FIELD:
            case GOODS_AUDIT:
                log.debug("忽略已移除的 Elasticsearch 索引消息: {}", messageExt.getTags());
                break;
            case GOODS_DELETE:
                try {
                    String goodsIdsJsonStr = new String(messageExt.getBody());
                    promotionService.removeByGoodsIds(goodsIdsJsonStr);
                } catch (Exception e) {
                    log.error("删除商品促销信息执行异常，商品信息: {}", new String(messageExt.getBody()), e);
                }
                break;
            case DOWN:
                promotionService.removeByGoodsIds(new String(messageExt.getBody()));
                break;
            case SKU_DELETE:
                List<String> skuIds = JSON.parseArray(new String(messageExt.getBody()), String.class);
                goodsCollectionService.deleteSkuCollection(skuIds);
                break;
            case STORE_GOODS_DELETE:
                log.debug("忽略已移除的店铺商品索引删除消息");
                break;
            case CATEGORY_GOODS_NAME:
                goodsService.categoryGoodsName(new String(messageExt.getBody()));
                break;
            case GOODS_COMMENT_COMPLETE:
                MemberEvaluation memberEvaluation = JSON.parseObject(new String(messageExt.getBody()), MemberEvaluation.class);
                for (GoodsCommentCompleteEvent goodsCommentCompleteEvent : goodsCommentCompleteEvents) {
                    try {
                        goodsCommentCompleteEvent.goodsComment(memberEvaluation);
                    } catch (Exception e) {
                        log.error("评价{},在{}业务中，状态修改事件执行异常", new String(messageExt.getBody()), goodsCommentCompleteEvent.getClass().getName(), e);
                    }
                }
                break;
            case BUY_GOODS_COMPLETE:
                goodsBuyComplete(messageExt);
                break;
            default:
                log.error("商品执行异常：{}", new String(messageExt.getBody()));
                break;
        }
    }

    private void goodsBuyComplete(MessageExt messageExt) {
        String goodsCompleteMessageStr = new String(messageExt.getBody());
        List<GoodsCompleteMessage> goodsCompleteMessageList = JSON.parseArray(goodsCompleteMessageStr, GoodsCompleteMessage.class);
        for (GoodsCompleteMessage goodsCompleteMessage : goodsCompleteMessageList) {
            Goods goods = goodsService.getById(goodsCompleteMessage.getGoodsId());
            if (goods != null) {
                if (goods.getBuyCount() == null) {
                    goods.setBuyCount(0);
                }
                int buyCount = goods.getBuyCount() + goodsCompleteMessage.getBuyNum();
                goodsService.updateGoodsBuyCount(goodsCompleteMessage.getGoodsId(), buyCount);
            } else {
                log.error("商品Id为[{}]的商品不存在，更新商品失败！", goodsCompleteMessage.getGoodsId());
            }
            GoodsSku goodsSku = goodsSkuService.getById(goodsCompleteMessage.getSkuId());
            if (goodsSku != null) {
                if (goodsSku.getBuyCount() == null) {
                    goodsSku.setBuyCount(0);
                }
                int buyCount = goodsSku.getBuyCount() + goodsCompleteMessage.getBuyNum();
                goodsSkuService.updateGoodsSkuBuyCount(goodsSku.getId(), buyCount);
            } else {
                log.error("商品SkuId为[{}]的商品不存在，更新商品失败！", goodsCompleteMessage.getSkuId());
            }
        }
    }
}
