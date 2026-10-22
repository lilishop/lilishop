package cn.lili.trigger.executor;

import cn.hutool.json.JSONUtil;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.trigger.TimeTriggerExecutor;
import cn.lili.trigger.message.GoodsMarketMessage;
import cn.lili.trigger.model.TimeExecuteConstant;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 商品定时上架执行器
 */
@Slf4j
@Component(TimeExecuteConstant.GOODS_UPPER_EXECUTOR)
public class GoodsUpperTimeTriggerExecutor implements TimeTriggerExecutor {

    @Autowired
    private GoodsService goodsService;

    @Override
    public void execute(Object object) {
        GoodsMarketMessage message = JSONUtil.toBean(JSONUtil.parseObj(object), GoodsMarketMessage.class);
        if (message == null || message.getGoodsIds() == null || message.getGoodsIds().isEmpty()) {
            return;
        }
        List<String> goodsIds = message.getGoodsIds();
        //仅执行与商品当前计划时间一致的任务；计划被修改/取消后，残留的旧任务自动失效
        if (message.getTriggerTime() != null) {
            goodsIds = goodsService.listByIds(goodsIds).stream()
                    .filter(goods -> goods.getScheduledUpperTime() != null
                            && goods.getScheduledUpperTime().getTime() == message.getTriggerTime())
                    .map(Goods::getId)
                    .toList();
            if (goodsIds.isEmpty()) {
                log.info("定时上架任务已失效（计划时间不匹配），跳过：{}", message.getGoodsIds());
                return;
            }
        }
        log.info("定时上架商品：{}", goodsIds);
        goodsService.updateGoodsMarketAble(goodsIds, GoodsStatusEnum.UPPER, message.getReason() == null ? "定时上架" : message.getReason());
    }
}

