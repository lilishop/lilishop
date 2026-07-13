package cn.lili.listener;

import cn.hutool.core.text.CharSequenceUtil;
import com.alibaba.fastjson2.JSON;
import cn.lili.common.aop.annotation.RetryOperation;
import cn.lili.event.GoodsCommentCompleteEvent;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsCompleteMessage;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.entity.dos.FootPrint;
import cn.lili.modules.member.entity.dos.MemberEvaluation;
import cn.lili.modules.member.service.FootprintService;
import cn.lili.modules.member.service.GoodsCollectionService;
import cn.lili.modules.promotion.service.PromotionService;
import cn.lili.message.QueueMessage;
import cn.lili.rocketmq.tags.GoodsTagsEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 商品消息
 */
@Component
@Slf4j
public class GoodsMessageListener {

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
    private PromotionService promotionService;

    @RetryOperation
    public void onMessage(QueueMessage messageExt) {
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
            case RESET_GOODS_INDEX:
            case STORE_GOODS_DELETE:
                log.debug("已忽略 Elasticsearch 索引类消息: {}", messageExt.getTags());
                break;
            case GOODS_AUDIT:
                // 由 WxChannelsGoodsSyncListener 处理
                break;
            case GOODS_DELETE:
                try {
                    String goodsIdsJsonStr = new String(messageExt.getBody());
                    promotionService.removeByGoodsIds(goodsIdsJsonStr);
                } catch (Exception e) {
                    log.error("商品删除消息，清理促销信息失败: {}", new String(messageExt.getBody()), e);
                }
                break;
            case DOWN:
                promotionService.removeByGoodsIds(new String(messageExt.getBody()));
                break;
            case SKU_DELETE:
                List<String> skuIds = JSON.parseArray(new String(messageExt.getBody()), String.class);
                goodsCollectionService.deleteSkuCollection(skuIds);
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
                        log.error("消费商品评价完成消息失败，消息体:{}，处理器:{}", new String(messageExt.getBody()), goodsCommentCompleteEvent.getClass().getName(), e);
                    }
                }
                break;
            case BUY_GOODS_COMPLETE:
                goodsBuyComplete(messageExt);
                break;
            default:
                log.error("未知商品消息标签，消息体:{}", new String(messageExt.getBody()));
                break;
        }
    }

    private void goodsBuyComplete(QueueMessage messageExt) {
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
                log.error("商品Id为[{}]的商品不存在，无法更新购买数", goodsCompleteMessage.getGoodsId());
            }
            GoodsSku goodsSku = goodsSkuService.getById(goodsCompleteMessage.getSkuId());
            if (goodsSku != null) {
                if (goodsSku.getBuyCount() == null) {
                    goodsSku.setBuyCount(0);
                }
                int buyCount = goodsSku.getBuyCount() + goodsCompleteMessage.getBuyNum();
                goodsSkuService.updateGoodsSkuBuyCount(goodsSku.getId(), buyCount);
            } else {
                log.error("商品SkuId为[{}]的SKU不存在，无法更新购买数", goodsCompleteMessage.getSkuId());
            }
        }
    }
}
