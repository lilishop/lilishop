package cn.lili.event.impl;

import cn.lili.event.StoreSettingChangeEvent;
import cn.lili.modules.distribution.service.DistributionGoodsService;
import cn.lili.modules.distribution.service.DistributionOrderService;
import cn.lili.modules.goods.service.DraftGoodsService;
import cn.lili.modules.member.service.MemberEvaluationService;
import cn.lili.modules.message.service.StoreMessageService;
import cn.lili.modules.order.aftersale.service.AfterSaleService;
import cn.lili.modules.order.order.service.OrderComplaintService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.order.service.ReceiptService;
import cn.lili.modules.order.order.service.StoreFlowService;
import cn.lili.modules.promotion.service.*;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.service.BillService;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @author ftyy
 * @description: 店铺名称修改同步对应场景
 * @date 2023/4/24
 */
@Service
public class StoreChangeEvent implements StoreSettingChangeEvent {
    /**
     * 优惠券活动表
     */
    @Autowired
    private CouponActivityService couponActivityService;

    /**
     * 砍价活动商品
     */
    @Autowired
    private KanjiaActivityGoodsService kanjiaActivityGoodsService;

    /**
     * 积分商品
     */
    @Autowired
    private PointsGoodsService pointsGoodsService;

    /**
     * 促销商品
     */
    @Autowired
    private PromotionGoodsService promotionGoodsService;

    /**
     * 秒杀活动活动
     */
    @Autowired
    private SeckillService seckillService;

    /**
     * 优惠券
     */
    @Autowired
    private CouponService couponService;

    /**
     * 满优惠活动
     */
    @Autowired
    private FullDiscountService fullDiscountService;

    /**
     * 拼团
     */
    @Autowired
    private PintuanService pintuanService;

    /**
     * 秒杀活动
     */
    @Autowired
    private SeckillApplyService seckillApplyService;
    /**
     * 订单
     */
    @Autowired
    private OrderService orderService;

    /**
     * 店铺流水
     */
    @Autowired
    private StoreFlowService storeFlowService;

    /**
     * 售后
     */
    @Autowired
    private AfterSaleService afterSaleService;

    /**
     * 订单投诉
     */
    @Autowired
    private OrderComplaintService orderComplaintService;
    /**
     * 发票
     */
    @Autowired
    private ReceiptService receiptService;

    /**
     * 会员优惠券
     */
    @Autowired
    private MemberCouponService memberCouponService;

    /**
     * 店铺消息
     */
    @Autowired
    private StoreMessageService storeMessageService;

    /**
     * 会员评价
     */
    @Autowired
    private MemberEvaluationService memberEvaluationService;

    /**
     * 结算单
     */
    @Autowired
    private BillService billService;
    /**
     * 分销商品
     */
    @Autowired
    private DistributionGoodsService distributionGoodsService;
    /**
     * 分销订单
     */
    @Autowired
    private DistributionOrderService distributionOrderService;

    /**
     * 草稿商品
     */
    @Autowired
    private DraftGoodsService draftGoodsService;


    /**
     * 店铺名称修改 同步更新相关店铺名称
     *
     * @param store 店铺信息
     */
    @Override
    public void storeSettingChange(Store store) {

        UpdateWrapper updateWrapper = new UpdateWrapper<>()
                .eq("store_id", store.getId())
                .set("store_name", store.getStoreName());

        //修改会员优惠券中店铺名称
        memberCouponService.update(updateWrapper);
        //修改优惠券活动中店铺名称
        couponActivityService.update(updateWrapper);
        //修改砍价活动商品中店铺名称
        kanjiaActivityGoodsService.update(updateWrapper);
        //修改积分商品中店铺名称
        pointsGoodsService.update(updateWrapper);
        //修改促销商品中店铺名称
        promotionGoodsService.update(updateWrapper);
        //修改秒杀活动活动中店铺名称
        seckillService.update(updateWrapper);
        //修改优惠券中店铺名称
        couponService.update(updateWrapper);
        //修改满优惠活动中店铺名称
        fullDiscountService.update(updateWrapper);
        //修改拼团中店铺名称
        pintuanService.update(updateWrapper);
        //修改秒杀活动中店铺名称
        seckillApplyService.update(updateWrapper);
        //修改发票中店铺名称
        receiptService.update(updateWrapper);
        //修改订单中的店铺名称
        orderService.update(updateWrapper);
        //修改店铺流水中店铺名称
        storeFlowService.update(updateWrapper);
        //修改售后中店铺名称
        afterSaleService.update(updateWrapper);
        //修改订单投诉中店铺名称
        orderComplaintService.update(updateWrapper);
        //修改店铺消息中的店铺名称
        storeMessageService.update(updateWrapper);
        //修改会员评价中店铺名称
        memberEvaluationService.update(updateWrapper);
        //修改结算单中店铺名称
        billService.update(updateWrapper);
        //修改分销订单中店铺名称
        distributionOrderService.update(updateWrapper);
        //修改分销商品中店铺名称
        distributionGoodsService.update(updateWrapper);
        //修改草稿商品中店铺名称
        draftGoodsService.update(updateWrapper);
    }
}
