package cn.lili.timetask.handler.impl.promotion;

import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dos.Seckill;
import cn.lili.modules.promotion.entity.enums.MemberCouponStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.vos.CouponVO;
import cn.lili.modules.promotion.entity.vos.PintuanVO;
import cn.lili.modules.promotion.service.*;
import cn.lili.modules.search.service.EsGoodsIndexService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.SeckillSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.timetask.handler.EveryDayExecute;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.google.gson.Gson;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 促销活动每日定时器
 *
 * @author Chopper
 * @date 2021/3/18 3:23 下午
 */
@Component

public class PromotionEverydayExecute implements EveryDayExecute {

    //Mongo
    @Autowired
    private MongoTemplate mongoTemplate;
    //es
    @Autowired
    private EsGoodsIndexService esGoodsIndexService;
    //满额活动
    @Autowired
    private FullDiscountService fullDiscountService;
    //拼团
    @Autowired
    private PintuanService pintuanService;
    //优惠券
    @Autowired
    private CouponService couponService;
    //会员优惠券
    @Autowired
    private MemberCouponService memberCouponService;
    //促销商品
    @Autowired
    private PromotionGoodsService promotionGoodsService;
    //设置
    @Autowired
    private SettingService settingService;
    @Autowired
    private SeckillService seckillService;



    /**
     * 将已过期的促销活动置为结束
     */
    @Override
    public void execute() {

        Query query = new Query();
//        结束条件 活动关闭/活动结束
        query.addCriteria(Criteria.where("promotionStatus").ne(PromotionStatusEnum.END.name())
                .orOperator(Criteria.where("promotionStatus").ne(PromotionStatusEnum.CLOSE.name())));
        query.addCriteria(Criteria.where("endTime").lt(new Date()));

        List<String> promotionIds = new ArrayList<>();
        //关闭满减活动
        endFullDiscount(promotionIds,query);
        //关闭拼团活动
        endPintuan(promotionIds,query);
        //关闭优惠券
        endCoupon(promotionIds,query);
        //每日新增秒杀活动
        addSeckill();
        promotionGoodsService.update(this.getUpdatePromotionGoodsWrapper(promotionIds));
    }

    /**
     * 添加秒杀活动
     * 从系统设置中获取秒杀活动的配置
     * 添加30天后的秒杀活动
     */
    private void addSeckill(){
        Setting setting = settingService.get(SettingEnum.SECKILL_SETTING.name());
        SeckillSetting seckillSetting=new Gson().fromJson(setting.getSettingValue(), SeckillSetting.class);
        Seckill seckill=new Seckill(seckillSetting.getHours(),seckillSetting.getSeckillRule());
        seckillService.saveSeckill(seckill);
    }

    /**
     * 修改满额活动下的商品
     * @param promotionIds 促销活动ID
     * @param query 查询Wrapper
     */
    private void endFullDiscount(List<String> promotionIds,Query query){
        //关闭满减活动
        List<FullDiscountVO> fullDiscountVOS = mongoTemplate.find(query, FullDiscountVO.class);
        if (!fullDiscountVOS.isEmpty()) {
            List<String> ids = new ArrayList<>();
            for (FullDiscountVO vo : fullDiscountVOS) {
                vo.setPromotionStatus(PromotionStatusEnum.END.name());
                if (vo.getPromotionGoodsList() != null && !vo.getPromotionGoodsList().isEmpty()) {
                    for (PromotionGoods promotionGoods : vo.getPromotionGoodsList()) {
                        promotionGoods.setPromotionStatus(PromotionStatusEnum.END.name());
                        esGoodsIndexService.deleteEsGoodsPromotionByPromotionId(promotionGoods.getSkuId(), vo.getId());
                    }
                }
                mongoTemplate.save(vo);
                ids.add(vo.getId());
            }
            fullDiscountService.update(this.getUpdatePromotionWrapper(ids));
            promotionIds.addAll(ids);
        }
    }

    /**
     * 修改拼团活动下的商品
     * @param promotionIds 促销活动ID
     * @param query 查询Wrapper
     */
    private void endPintuan(List<String> promotionIds,Query query){
        List<PintuanVO> pintuanVOS = mongoTemplate.find(query, PintuanVO.class);
        if (!pintuanVOS.isEmpty()) {
            //准备修改活动的id
            List<String> ids = new ArrayList<>();
            for (PintuanVO vo : pintuanVOS) {
                vo.setPromotionStatus(PromotionStatusEnum.END.name());
                if (vo.getPromotionGoodsList() != null && !vo.getPromotionGoodsList().isEmpty()) {
                    for (PromotionGoods promotionGoods : vo.getPromotionGoodsList()) {
                        promotionGoods.setPromotionStatus(PromotionStatusEnum.END.name());
                        esGoodsIndexService.deleteEsGoodsPromotionByPromotionId(promotionGoods.getSkuId(), vo.getId());
                    }
                }
                mongoTemplate.save(vo);
                ids.add(vo.getId());
            }
            pintuanService.update(this.getUpdatePromotionWrapper(ids));
            promotionIds.addAll(ids);
        }
    }

    /**
     * 修改优惠券下的商品
     * @param promotionIds 促销活动ID
     * @param query 查询Wrapper
     */
    private void endCoupon(List<String> promotionIds,Query query){
        List<CouponVO> couponVOS = mongoTemplate.find(query, CouponVO.class);
        if (!couponVOS.isEmpty()) {
            List<String> ids = new ArrayList<>();
            for (CouponVO vo : couponVOS) {
                vo.setPromotionStatus(PromotionStatusEnum.END.name());
                if (vo.getPromotionGoodsList() != null && !vo.getPromotionGoodsList().isEmpty()) {
                    for (PromotionGoods promotionGoods : vo.getPromotionGoodsList()) {
                        promotionGoods.setPromotionStatus(PromotionStatusEnum.END.name());
                        esGoodsIndexService.deleteEsGoodsPromotionByPromotionId(promotionGoods.getSkuId(), vo.getId());
                    }
                }
                mongoTemplate.save(vo);
                ids.add(vo.getId());
            }
            couponService.update(this.getUpdatePromotionWrapper(ids));
            LambdaUpdateWrapper<MemberCoupon> memberCouponLambdaUpdateWrapper = new LambdaUpdateWrapper<MemberCoupon>()
                    .in(MemberCoupon::getCouponId, ids)
                    .eq(MemberCoupon::getMemberCouponStatus, MemberCouponStatusEnum.NEW.name())
                    .set(MemberCoupon::getMemberCouponStatus, MemberCouponStatusEnum.EXPIRE.name());
            memberCouponService.update(memberCouponLambdaUpdateWrapper);
            promotionIds.addAll(ids);
        }
    }

    /**
     * 获取促销修改查询条件 修改活动状态
     * @param ids 促销活动ID
     * @return 促销活动商品查询Wrapper
     */
    private UpdateWrapper getUpdatePromotionWrapper(List<String> ids) {
        UpdateWrapper updateWrapper = new UpdateWrapper<>();
        updateWrapper.in("id", ids);
        updateWrapper.set("promotion_status", PromotionStatusEnum.END.name());
        return updateWrapper;
    }
    /**
     * 获取商品的促销修改查询条件 修改商品状态
     * @param ids 促销活动ID
     * @return 促销活动商品修改Wrapper
     */
    private UpdateWrapper getUpdatePromotionGoodsWrapper(List<String> ids) {
        UpdateWrapper updateWrapper = new UpdateWrapper<>();
        updateWrapper.in("promotion_id", ids);
        updateWrapper.set("promotion_status", PromotionStatusEnum.END.name());
        return updateWrapper;
    }
}
