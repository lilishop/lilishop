package cn.lili.timetask.handler.impl.goods;

import cn.hutool.core.convert.Convert;
import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.member.service.MemberEvaluationService;
import cn.lili.timetask.handler.EveryDayExecute;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

/**
 * 商品定时器
 *
 * @author Chopper
 * @since 2021/3/18 3:23 下午
 */
@Component
public class GoodsExecute implements EveryDayExecute {
    /**
     * 会员评价
     */
    @Autowired
    private MemberEvaluationService memberEvaluationService;
    /**
     * 商品
     */
    @Autowired
    private GoodsService goodsService;

    /**
     * 查询已上架的商品的评价数量并赋值
     */
    @Override
    public void execute() {

        //查询上次统计到本次的评价数量
        List<Map<String, Object>> list = memberEvaluationService.memberEvaluationNum(DateUtil.yesterday(), new DateTime());

        for (Map<String, Object> map : list) {
            goodsService.addGoodsCommentNum(Convert.toInt(map.get("num").toString()), map.get("goods_id").toString());
        }

    }
}
