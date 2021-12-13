package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderSearchParams;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.promotion.entity.dos.Pintuan;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.entity.vos.PintuanMemberVO;
import cn.lili.modules.promotion.entity.vos.PintuanShareVO;
import cn.lili.modules.promotion.entity.vos.PintuanVO;
import cn.lili.modules.promotion.entity.vos.PromotionGoodsSearchParams;
import cn.lili.modules.promotion.mapper.PintuanMapper;
import cn.lili.modules.promotion.service.PintuanService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.tools.PromotionTools;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 拼团业务层实现
 *
 * @author Chopper
 * @since 2020/8/21
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class PintuanServiceImpl extends AbstractPromotionsServiceImpl<PintuanMapper, Pintuan> implements PintuanService {

    /**
     * 促销商品
     */
    @Autowired
    private PromotionGoodsService promotionGoodsService;
    /**
     * 规格商品
     */
    @Autowired
    private GoodsSkuService goodsSkuService;
    /**
     * 会员
     */
    @Autowired
    private MemberService memberService;
    /**
     * 订单
     */
    @Autowired
    private OrderService orderService;

    /**
     * 获取当前拼团的会员
     *
     * @param pintuanId 拼图id
     * @return 当前拼团的会员列表
     */
    @Override
    public List<PintuanMemberVO> getPintuanMember(String pintuanId) {
        List<PintuanMemberVO> members = new ArrayList<>();
        Pintuan pintuan = this.getById(pintuanId);
        if (pintuan == null) {
            log.error("拼团活动为" + pintuanId + "的拼团活动不存在！");
            return new ArrayList<>();
        }
        OrderSearchParams searchParams = new OrderSearchParams();
        searchParams.setOrderStatus(OrderStatusEnum.PAID.name());
        searchParams.setPromotionId(pintuanId);
        searchParams.setOrderPromotionType(PromotionTypeEnum.PINTUAN.name());
        searchParams.setParentOrderSn("");
        searchParams.setMemberId("");
        List<Order> orders = orderService.queryListByParams(searchParams);
        //遍历订单状态为已支付，为团长的拼团订单
        for (Order order : orders) {
            Member member = memberService.getById(order.getMemberId());
            PintuanMemberVO memberVO = new PintuanMemberVO(member);
            //获取已参团人数
            this.setMemberVONum(memberVO, pintuan.getRequiredNum(), order.getSn());
            memberVO.setOrderSn(order.getSn());
            members.add(memberVO);
        }
        return members;
    }

    /**
     * 从mongo中查询拼团活动详情
     *
     * @param id 拼团ID
     * @return 拼团活动详情
     */
    @Override
    public PintuanVO getPintuanVO(String id) {
        Pintuan pintuan = this.getById(id);
        if (pintuan == null) {
            log.error("拼团活动id[" + id + "]的拼团活动不存在！");
            throw new ServiceException(ResultCode.PINTUAN_NOT_EXIST_ERROR);
        }
        PintuanVO pintuanVO = new PintuanVO(pintuan);
        PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
        searchParams.setPromotionId(pintuan.getId());
        pintuanVO.setPromotionGoodsList(promotionGoodsService.listFindAll(searchParams));
        return pintuanVO;
    }

    /**
     * 获取拼团分享信息
     *
     * @param parentOrderSn 拼团团长订单sn
     * @param skuId         商品skuId
     * @return 拼团分享信息
     */
    @Override
    public PintuanShareVO getPintuanShareInfo(String parentOrderSn, String skuId) {
        PintuanShareVO pintuanShareVO = new PintuanShareVO();
        pintuanShareVO.setPintuanMemberVOS(new ArrayList<>());
        //查找团长订单和已和当前拼团订单拼团的订单
        List<Order> orders = orderService.queryListByPromotion(PromotionTypeEnum.PINTUAN.name(), PayStatusEnum.PAID.name(), parentOrderSn, parentOrderSn);
        this.setPintuanOrderInfo(orders, pintuanShareVO, skuId);
        //如果为根据团员订单sn查询拼团订单信息时，找到团长订单sn，然后找到所有参与到同一拼团的订单信息
        if (!orders.isEmpty() && pintuanShareVO.getPromotionGoods() == null) {
            List<Order> parentOrders = orderService.queryListByPromotion(PromotionTypeEnum.PINTUAN.name(), PayStatusEnum.PAID.name(), orders.get(0).getParentOrderSn(), orders.get(0).getParentOrderSn());
            this.setPintuanOrderInfo(parentOrders, pintuanShareVO, skuId);
        }
        return pintuanShareVO;
    }

    /**
     * 更新促销状态
     * 如果要更新促销状态为关闭，startTime和endTime置为空即可
     *
     * @param ids       促销id集合
     * @param startTime 开始时间
     * @param endTime   结束时间
     * @return 是否更新成功
     */
    @Override
    public boolean updateStatus(List<String> ids, Long startTime, Long endTime) {
        if (startTime != null && endTime != null) {
            for (String id : ids) {
                Pintuan pintuan = this.getById(id);
                QueryWrapper<Pintuan> queryWrapper = PromotionTools.checkActiveTime(new Date(startTime), new Date(endTime), PromotionTypeEnum.PINTUAN, pintuan.getStoreId(), id);
                int sameNum = this.count(queryWrapper);
                //当前时间段是否存在同类活动
                if (sameNum > 0) {
                    throw new ServiceException(ResultCode.PROMOTION_SAME_ACTIVE_EXIST);
                }
            }
        }

        return super.updateStatus(ids, startTime, endTime);
    }

    /**
     * 检查促销参数
     *
     * @param promotions 促销实体
     */
    @Override
    public void checkPromotions(Pintuan promotions) {
        QueryWrapper<Pintuan> queryWrapper = PromotionTools.checkActiveTime(promotions.getStartTime(), promotions.getEndTime(), PromotionTypeEnum.PINTUAN, promotions.getStoreId(), promotions.getId());
        int sameNum = this.count(queryWrapper);
        //当前时间段是否存在同类活动
        if (sameNum > 0) {
            throw new ServiceException(ResultCode.PROMOTION_SAME_ACTIVE_EXIST);
        }
        super.checkPromotions(promotions);
    }

    /**
     * 更新促销商品信息
     *
     * @param promotions 促销实体
     */
    @Override
    public void updatePromotionsGoods(Pintuan promotions) {
        super.updatePromotionsGoods(promotions);
        if (!PromotionsStatusEnum.CLOSE.name().equals(promotions.getPromotionStatus())
                && PromotionsScopeTypeEnum.PORTION_GOODS.name().equals(promotions.getScopeType())
                && promotions instanceof PintuanVO) {
            PintuanVO pintuanVO = (PintuanVO) promotions;
            this.updatePintuanPromotionGoods(pintuanVO);
        }
        if (promotions.getEndTime() == null && promotions.getStartTime() == null) {
            //过滤父级拼团订单，根据父级拼团订单分组
            Map<String, List<Order>> collect = orderService.queryListByPromotion(promotions.getId())
                    .stream().filter(i -> CharSequenceUtil.isNotEmpty(i.getParentOrderSn()))
                    .collect(Collectors.groupingBy(Order::getParentOrderSn));
            this.isOpenFictitiousPintuan(promotions, collect);
        }
    }

    /**
     * 更新促销信息到商品索引
     *
     * @param promotions 促销实体
     */
    @Override
    public void updateEsGoodsIndex(Pintuan promotions) {
        Pintuan pintuan = JSONUtil.parse(promotions).toBean(Pintuan.class);
        super.updateEsGoodsIndex(pintuan);
    }

    /**
     * 当前促销类型
     *
     * @return 当前促销类型
     */
    @Override
    public PromotionTypeEnum getPromotionType() {
        return PromotionTypeEnum.PINTUAN;
    }

    /**
     * 根据订单信息，从中提取出拼团信息，设置拼团信息
     *
     * @param orders         订单列表
     * @param pintuanShareVO 拼团信息
     * @param skuId          商品skuId（用于获取拼团商品信息）
     */
    private void setPintuanOrderInfo(List<Order> orders, PintuanShareVO pintuanShareVO, String skuId) {
        for (Order order : orders) {
            Member member = memberService.getById(order.getMemberId());
            PintuanMemberVO memberVO = new PintuanMemberVO(member);
            if (CharSequenceUtil.isEmpty(order.getParentOrderSn())) {
                memberVO.setOrderSn("");
                PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
                searchParams.setPromotionStatus(PromotionTypeEnum.PINTUAN.name());
                searchParams.setPromotionId(order.getPromotionId());
                searchParams.setSkuId(skuId);
                PromotionGoods promotionGoods = promotionGoodsService.getPromotionsGoods(searchParams);
                if (promotionGoods == null) {
                    throw new ServiceException(ResultCode.PINTUAN_GOODS_NOT_EXIST_ERROR);
                }
                pintuanShareVO.setPromotionGoods(promotionGoods);
                Pintuan pintuanById = this.getById(order.getPromotionId());
                //获取已参团人数
                this.setMemberVONum(memberVO, pintuanById.getRequiredNum(), order.getSn());
            }
            pintuanShareVO.getPintuanMemberVOS().add(memberVO);
        }
    }

    private void setMemberVONum(PintuanMemberVO memberVO, Integer requiredNum, String orderSn) {
        int count = this.orderService.queryCountByPromotion(PromotionTypeEnum.PINTUAN.name(), PayStatusEnum.PAID.name(), orderSn, orderSn);
        //获取待参团人数
        int toBoGrouped = requiredNum - count;
        memberVO.setGroupNum(requiredNum);
        memberVO.setGroupedNum(count);
        memberVO.setToBeGroupedNum(toBoGrouped);
    }

    /**
     * 从指定订单列表中检查是否开始虚拟成团
     *
     * @param pintuan 拼团活动信息
     * @param collect 检查的订单列表
     */
    private void isOpenFictitiousPintuan(Pintuan pintuan, Map<String, List<Order>> collect) {
        //成团人数
        Integer requiredNum = pintuan.getRequiredNum();

        for (Map.Entry<String, List<Order>> entry : collect.entrySet()) {
            //是否开启虚拟成团
            if (Boolean.FALSE.equals(pintuan.getFictitious()) && entry.getValue().size() < requiredNum) {
                //如果未开启虚拟成团且已参团人数小于成团人数，则自动取消订单
                String reason = "拼团活动结束订单未付款，系统自动取消订单";
                if (CharSequenceUtil.isNotEmpty(entry.getKey())) {
                    this.orderService.systemCancel(entry.getKey(), reason);
                } else {
                    for (Order order : entry.getValue()) {
                        this.orderService.systemCancel(order.getSn(), reason);
                    }
                }
            } else if (Boolean.TRUE.equals(pintuan.getFictitious())) {
                this.fictitiousPintuan(entry, requiredNum);
            }
        }
    }

    /**
     * 虚拟成团
     *
     * @param entry       订单列表
     * @param requiredNum 必须参团人数
     */
    private void fictitiousPintuan(Map.Entry<String, List<Order>> entry, Integer requiredNum) {
        Map<String, List<Order>> listMap = entry.getValue().stream().collect(Collectors.groupingBy(Order::getPayStatus));
        //未付款订单
        List<Order> unpaidOrders = listMap.get(PayStatusEnum.UNPAID.name());
        //未付款订单自动取消
        if (unpaidOrders != null && !unpaidOrders.isEmpty()) {
            for (Order unpaidOrder : unpaidOrders) {
                unpaidOrder.setOrderStatus(OrderStatusEnum.CANCELLED.name());
                unpaidOrder.setCancelReason("拼团活动结束订单未付款，系统自动取消订单");
            }
            orderService.updateBatchById(unpaidOrders);
        }
        List<Order> paidOrders = listMap.get(PayStatusEnum.PAID.name());
        //如待参团人数大于0，并已开启虚拟成团
        if (!paidOrders.isEmpty()) {
            //待参团人数
            int waitNum = requiredNum - paidOrders.size();
            //添加虚拟成团
            for (int i = 0; i < waitNum; i++) {
                Order order = new Order();
                BeanUtil.copyProperties(paidOrders.get(0), order);
                order.setMemberId("-1");
                order.setMemberName("参团人员");
                orderService.save(order);
                paidOrders.add(order);
            }
            for (Order paidOrder : paidOrders) {
                paidOrder.setOrderStatus(OrderStatusEnum.UNDELIVERED.name());
            }
            orderService.updateBatchById(paidOrders);
        }
    }

    /**
     * 更新记录的促销商品信息
     *
     * @param pintuan 拼团信息
     */
    private void updatePintuanPromotionGoods(PintuanVO pintuan) {

        if (pintuan.getPromotionGoodsList() != null && !pintuan.getPromotionGoodsList().isEmpty()) {
            List<PromotionGoods> promotionGoods = PromotionTools.promotionGoodsInit(pintuan.getPromotionGoodsList(), pintuan, PromotionTypeEnum.PINTUAN);
            for (PromotionGoods promotionGood : promotionGoods) {
                if (goodsSkuService.getGoodsSkuByIdFromCache(promotionGood.getSkuId()) == null) {
                    log.error("商品[" + promotionGood.getGoodsName() + "]不存在或处于不可售卖状态！");
                    throw new ServiceException();
                }
                //查询是否在同一时间段参与了拼团活动
                Integer count = promotionGoodsService.findInnerOverlapPromotionGoods(PromotionTypeEnum.SECKILL.name(), promotionGood.getSkuId(), pintuan.getStartTime(), pintuan.getEndTime(), pintuan.getId());
                //查询是否在同一时间段参与了限时抢购活动
                count += promotionGoodsService.findInnerOverlapPromotionGoods(PromotionTypeEnum.PINTUAN.name(), promotionGood.getSkuId(), pintuan.getStartTime(), pintuan.getEndTime(), pintuan.getId());
                if (count > 0) {
                    log.error("商品[" + promotionGood.getGoodsName() + "]已经在重叠的时间段参加了秒杀活动或拼团活动，不能参加拼团活动");
                    throw new ServiceException("商品[" + promotionGood.getGoodsName() + "]已经在重叠的时间段参加了秒杀活动或拼团活动，不能参加拼团活动");
                }
            }
            PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
            searchParams.setPromotionId(pintuan.getId());
            searchParams.setPromotionType(PromotionTypeEnum.PINTUAN.name());
            promotionGoodsService.deletePromotionGoods(searchParams);
            promotionGoodsService.saveOrUpdateBatch(promotionGoods);
        }
    }

}