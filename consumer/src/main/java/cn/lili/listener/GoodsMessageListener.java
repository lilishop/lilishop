package cn.lili.listener;

import cn.hutool.json.JSONUtil;
import cn.lili.common.rocketmq.tags.GoodsTagsEnum;
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
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.service.EsGoodsIndexService;
import cn.lili.modules.store.service.StoreService;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.common.message.MessageExt;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 商品消息
 *
 * @author paulG
 * @since 2020/12/9
 **/
@Component
@Slf4j
@RocketMQMessageListener(topic = "${lili.data.rocketmq.goods-topic}", consumerGroup = "${lili.data.rocketmq.goods-group}")
public class GoodsMessageListener implements RocketMQListener<MessageExt> {

    //ES商品
    @Autowired
    private EsGoodsIndexService goodsIndexService;
    //店铺
    @Autowired
    private StoreService storeService;
    //商品
    @Autowired
    private GoodsService goodsService;
    //商品
    @Autowired
    private GoodsSkuService goodsSkuService;
    //用户足迹
    @Autowired
    private FootprintService footprintService;
    //商品收藏
    @Autowired
    private GoodsCollectionService goodsCollectionService;
    //商品评价
    @Autowired
    private List<GoodsCommentCompleteEvent> goodsCommentCompleteEvents;
    @Override
    public void onMessage(MessageExt messageExt) {

        switch (GoodsTagsEnum.valueOf(messageExt.getTags())) {
            //查看商品
            case VIEW_GOODS:
                FootPrint footPrint = JSONUtil.toBean(new String(messageExt.getBody()), FootPrint.class);
                footprintService.saveFootprint(footPrint);
                break;
            //生成索引
            case GENERATOR_GOODS_INDEX:
                String goodsIndexJsonStr = new String(messageExt.getBody());
                List<EsGoodsIndex> goodsIndices = JSONUtil.toList(JSONUtil.parseArray(goodsIndexJsonStr), EsGoodsIndex.class);
                for (EsGoodsIndex goodsIndex : goodsIndices) {
                    log.info("生成商品索引" + goodsIndex);
                    this.goodsIndexService.addIndex(goodsIndex);
                }
                break;
            //审核商品
            case GOODS_AUDIT:
            //删除商品
            case GOODS_DELETE:
                storeService.updateStoreGoodsNum(new String(messageExt.getBody()));
                break;
            //规格删除
            case SKU_DELETE:
                String message = new String(messageExt.getBody());
                List<String> skuIds = JSONUtil.toList(message, String.class);
                goodsCollectionService.deleteSkuCollection(skuIds);
                break;
            //收藏商品
            case GOODS_COLLECTION:
                storeService.updateStoreCollectionNum(new String(messageExt.getBody()));
                break;
            //商品评价
            case GOODS_COMMENT_COMPLETE:
                MemberEvaluation memberEvaluation = JSONUtil.toBean(new String(messageExt.getBody()), MemberEvaluation.class);
                for (GoodsCommentCompleteEvent goodsCommentCompleteEvent : goodsCommentCompleteEvents) {
                    try {
                        goodsCommentCompleteEvent.goodsComment(memberEvaluation);
                    } catch (Exception e) {
                        log.error("评价{},在{}业务中，状态修改事件执行异常",
                                new String(messageExt.getBody()),
                                goodsCommentCompleteEvent.getClass().getName(),
                                e);
                    }
                }
                break;
            //购买商品完成
            case BUY_GOODS_COMPLETE:
                String goodsCompleteMessageStr = new String(messageExt.getBody());
                List<GoodsCompleteMessage> goodsCompleteMessageList = JSONUtil.toList(JSONUtil.parseArray(goodsCompleteMessageStr), GoodsCompleteMessage.class);
                for (GoodsCompleteMessage goodsCompleteMessage : goodsCompleteMessageList) {
                    Goods goods = goodsService.getById(goodsCompleteMessage.getGoodsId());
                    if (goods != null) {
                        //更新商品购买数量
                        if (goods.getBuyCount() == null) {
                            goods.setBuyCount(0);
                        }
                        int buyCount = goods.getBuyCount() + goodsCompleteMessage.getBuyNum();
                        LambdaUpdateWrapper<Goods> updateWrapper = new LambdaUpdateWrapper<>();
                        updateWrapper.eq(Goods::getId, goodsCompleteMessage.getGoodsId());
                        updateWrapper.set(Goods::getBuyCount, buyCount);
                        goodsService.update(updateWrapper);
                    } else {
                        log.error("商品Id为[" + goodsCompleteMessage.getGoodsId() + "的商品不存在，更新商品失败！");
                    }
                    GoodsSku goodsSku = goodsSkuService.getById(goodsCompleteMessage.getSkuId());
                    if (goodsSku != null) {
                        //更新商品购买数量
                        if (goodsSku.getBuyCount() == null) {
                            goodsSku.setBuyCount(0);
                        }
                        int buyCount = goodsSku.getBuyCount() + goodsCompleteMessage.getBuyNum();
                        goodsSku.setBuyCount(buyCount);
                        goodsSkuService.update(goodsSku);
                        goodsIndexService.updateIndexBuyNum(goodsCompleteMessage.getSkuId(), buyCount);
                    } else {
                        log.error("商品SkuId为[" + goodsCompleteMessage.getGoodsId() + "的商品不存在，更新商品失败！");
                    }
                }
                break;
        }
    }
}
