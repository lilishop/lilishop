package cn.lili.listener;

import cn.hutool.json.JSONUtil;
import cn.lili.common.rocketmq.tags.GoodsTagsEnum;
import cn.lili.event.GoodsCommentCompleteEvent;
import cn.lili.modules.distribution.entity.dos.DistributionGoods;
import cn.lili.modules.distribution.entity.dos.DistributionSelectedGoods;
import cn.lili.modules.distribution.service.DistributionGoodsService;
import cn.lili.modules.distribution.service.DistributionSelectedGoodsService;
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
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
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

    /**
     * ES商品
     */
    @Autowired
    private EsGoodsIndexService goodsIndexService;
    /**
     * 店铺
     */
    @Autowired
    private StoreService storeService;
    /**
     * 商品
     */
    @Autowired
    private GoodsService goodsService;
    /**
     * 商品Sku
     */
    @Autowired
    private GoodsSkuService goodsSkuService;
    /**
     * 用户足迹
     */
    @Autowired
    private FootprintService footprintService;
    /**
     * 商品收藏
     */
    @Autowired
    private GoodsCollectionService goodsCollectionService;
    /**
     * 商品评价
     */
    @Autowired
    private List<GoodsCommentCompleteEvent> goodsCommentCompleteEvents;
    /**
     * 分销商品
     */
    @Autowired
    private DistributionGoodsService distributionGoodsService;
    /**
     * 分销员-商品关联表
     */
    @Autowired
    private DistributionSelectedGoodsService distributionSelectedGoodsService;

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
                break;
            //删除商品
            case GOODS_DELETE:
                deleteGoods(messageExt);
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
                this.goodsBuyComplete(messageExt);
                break;
            default:
                log.error("商品执行异常：", new String(messageExt.getBody()));
                break;
        }
    }

    /**
     * 删除商品
     * 1.更新店铺的商品数量
     * 2.删除分销员-分销商品绑定关系
     * 3.删除分销商品
     *
     * @param messageExt 消息
     */
    private void deleteGoods(MessageExt messageExt) {
        Goods goods = JSONUtil.toBean(new String(messageExt.getBody()), Goods.class);
        //更新店铺商品数量
        storeService.updateStoreGoodsNum(goods.getStoreId());

        //删除获取分销商品
        DistributionGoods distributionGoods = distributionGoodsService.getOne(new LambdaQueryWrapper<DistributionGoods>()
                .eq(DistributionGoods::getGoodsId, goods.getId()));

        //删除分销商品绑定关系
        distributionSelectedGoodsService.remove(new LambdaQueryWrapper<DistributionSelectedGoods>()
                .eq(DistributionSelectedGoods::getDistributionGoodsId, distributionGoods.getId()));

        //删除分销商品
        distributionGoodsService.removeById(distributionGoods.getId());
    }

    /**
     * 商品购买完成
     * 1.更新商品购买数量
     * 2.更新SKU购买数量
     * 3.更新索引购买数量
     *
     * @param messageExt
     */
    private void goodsBuyComplete(MessageExt messageExt) {
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
                goodsService.update(new LambdaUpdateWrapper<Goods>()
                        .eq(Goods::getId, goodsCompleteMessage.getGoodsId())
                        .set(Goods::getBuyCount, buyCount));
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
    }
}
