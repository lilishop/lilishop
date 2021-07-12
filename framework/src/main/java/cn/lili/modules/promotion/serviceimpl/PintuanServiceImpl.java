package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.util.StrUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.trigger.util.DelayQueueTools;
import cn.lili.common.trigger.enums.DelayTypeEnums;
import cn.lili.common.trigger.message.PromotionMessage;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.trigger.interfaces.TimeTrigger;
import cn.lili.common.trigger.model.TimeExecuteConstant;
import cn.lili.common.trigger.model.TimeTriggerMsg;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.config.rocketmq.RocketmqCustomProperties;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.promotion.entity.dos.Pintuan;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionTypeEnum;
import cn.lili.modules.promotion.entity.vos.PintuanMemberVO;
import cn.lili.modules.promotion.entity.vos.PintuanSearchParams;
import cn.lili.modules.promotion.entity.vos.PintuanShareVO;
import cn.lili.modules.promotion.entity.vos.PintuanVO;
import cn.lili.modules.promotion.mapper.PintuanMapper;
import cn.lili.modules.promotion.service.PintuanService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.tools.PromotionTools;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Query;
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
 * @date 2020/8/21
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class PintuanServiceImpl extends ServiceImpl<PintuanMapper, Pintuan> implements PintuanService {

    /**
     * 延时任务
     */
    @Autowired
    private TimeTrigger timeTrigger;
    /**
     * Mongo
     */
    @Autowired
    private MongoTemplate mongoTemplate;
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
     * RocketMQ
     */
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;
    /**
     * 订单
     */
    @Autowired
    private OrderService orderService;

    @Override
    public IPage<Pintuan> getPintuanByPage(PintuanSearchParams param, PageVO page) {
        QueryWrapper<Pintuan> queryWrapper = param.wrapper();
        return page(PageUtil.initPage(page), queryWrapper);
    }

    /**
     * 获取当前拼团的会员
     *
     * @param pintuanId 拼图id
     * @return 当前拼团的会员列表
     */
    @Override
    public List<PintuanMemberVO> getPintuanMember(String pintuanId) {
        List<PintuanMemberVO> members = new ArrayList<>();
        PintuanVO pintuan = this.getPintuanByIdFromMongo(pintuanId);
        if (pintuan == null) {
            log.error("拼团活动为" + pintuanId + "的拼团活动不存在！");
            return new ArrayList<>();
        }
        LambdaQueryWrapper<Order> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Order::getPromotionId, pintuanId)
                .eq(Order::getOrderPromotionType, PromotionTypeEnum.PINTUAN.name())
                .eq(Order::getOrderStatus, OrderStatusEnum.PAID.name())
                .eq(Order::getParentOrderSn, "");
        List<Order> orders = orderService.list(queryWrapper);
        //遍历订单状态为已支付，为团长的拼团订单
        for (Order order : orders) {
            Member member = memberService.getById(order.getMemberId());
            PintuanMemberVO memberVO = new PintuanMemberVO(member);
            LambdaQueryWrapper<Order> countQueryWrapper = new LambdaQueryWrapper<>();
            countQueryWrapper.eq(Order::getOrderStatus, OrderStatusEnum.PAID.name());
            countQueryWrapper.and(i -> i.eq(Order::getSn, order.getSn()).or(j -> j.eq(Order::getParentOrderSn, order.getSn())));
            //获取已参团人数
            int count = orderService.count(countQueryWrapper);
            //获取待参团人数
            int toBoGrouped = pintuan.getRequiredNum() - count;
            memberVO.setGroupNum(pintuan.getRequiredNum());
            memberVO.setGroupedNum(count);
            memberVO.setToBeGroupedNum(toBoGrouped);
            memberVO.setOrderSn(order.getSn());
            members.add(memberVO);
        }
        return members;
    }

    @Override
    public IPage<PintuanVO> getPintuanByPageFromMongo(PintuanSearchParams param, PageVO page) {
        IPage<PintuanVO> pintuanPage = new Page<>();
        Query query = param.mongoQuery();
        if (page != null) {
            page.setNotConvert(true);
            PromotionTools.mongoQueryPageParam(query, page);
            pintuanPage.setCurrent(page.getPageNumber());
            pintuanPage.setSize(page.getPageSize());
        }
        List<PintuanVO> pintuanVOS = mongoTemplate.find(query, PintuanVO.class);
        pintuanPage.setRecords(pintuanVOS);
        pintuanPage.setTotal(this.getPintuanByPageFromMongoCount(param));
        return pintuanPage;
    }

    /**
     * 从mongo中查询拼团活动详情
     *
     * @param id 拼团ID
     * @return 拼团活动详情
     */
    @Override
    public PintuanVO getPintuanByIdFromMongo(String id) {
        PintuanVO pintuanVO = mongoTemplate.findById(id, PintuanVO.class);
        if (pintuanVO == null) {
            log.error("拼团活动id[" + id + "]的拼团活动不存在！");
            throw new ServiceException(ResultCode.ERROR);
        }
        return pintuanVO;
    }

    /**
     * 从mysql中查询拼团活动详情
     *
     * @param id 拼团活动id
     * @return 拼团活动详情
     */
    @Override
    public Pintuan getPintuanById(String id) {
        Pintuan pintuan = this.getById(id);
        if (pintuan == null) {
            log.error("拼团活动id[" + id + "]的拼团活动不存在！");
            throw new ServiceException(ResultCode.ERROR);
        }
        return pintuan;
    }

    /**
     * 从mongo中根据条件查询拼团活动总数
     *
     * @param param 拼团活动查询参数
     * @return 总数
     */
    @Override
    public Long getPintuanByPageFromMongoCount(PintuanSearchParams param) {
        Query query = param.mongoQuery();
        return mongoTemplate.count(query, PintuanVO.class);
    }

    @Override
    public boolean addPintuan(PintuanVO pintuan) {
        PromotionTools.checkPromotionTime(pintuan.getStartTime().getTime(), pintuan.getEndTime().getTime());
        this.checkSamePromotion(pintuan.getStartTime(), pintuan.getEndTime(), pintuan.getStoreId(), null);
        pintuan.setPromotionStatus(PromotionStatusEnum.NEW.name());
        //保存到MYSQL中
        boolean result = this.save(pintuan);
        this.updatePintuanPromotionGoods(pintuan);
        this.mongoTemplate.save(pintuan);
        this.addPintuanStartTask(pintuan);
        return result;
    }

    @Override
    public boolean modifyPintuan(PintuanVO pintuan) {
        PintuanVO pintuanVO = this.checkExist(pintuan.getId());
        if (!pintuan.getPromotionStatus().equals(PromotionStatusEnum.NEW.name())) {
            throw new ServiceException(ResultCode.PINTUAN_EDIT_ERROR);
        }
        //检查促销时间
        PromotionTools.checkPromotionTime(pintuan.getStartTime().getTime(), pintuan.getEndTime().getTime());
        //检查同一时间，同一店铺，同一类型的促销活动
        this.checkSamePromotion(pintuan.getStartTime(), pintuan.getEndTime(), pintuan.getStoreId(), pintuan.getId());
        boolean result = this.updateById(pintuan);
        if (pintuan.getPromotionGoodsList() != null) {
            this.updatePintuanPromotionGoods(pintuan);
        }
        this.mongoTemplate.save(pintuan);
        //时间发生变化
        if (pintuan.getStartTime().getTime() != pintuanVO.getStartTime().getTime()) {
            PromotionMessage promotionMessage = new PromotionMessage(pintuan.getId(), PromotionTypeEnum.PINTUAN.name(), PromotionStatusEnum.START.name(), pintuan.getStartTime(), pintuan.getEndTime());
            //更新延时任务
            this.timeTrigger.edit(TimeExecuteConstant.PROMOTION_EXECUTOR,
                    promotionMessage,
                    pintuanVO.getStartTime().getTime(),
                    pintuan.getStartTime().getTime(),
                    DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.PROMOTION, (promotionMessage.getPromotionType() + promotionMessage.getPromotionId())),
                    DateUtil.getDelayTime(pintuanVO.getStartTime().getTime()),
                    rocketmqCustomProperties.getPromotionTopic());
        }
        return result;
    }

    @Override
    public boolean openPintuan(String pintuanId, Date startTime, Date endTime) {
        PintuanVO pintuan = checkExist(pintuanId);
        pintuan.setStartTime(startTime);
        pintuan.setEndTime(endTime);
        boolean result;

        long endTimeLong = endTime.getTime() / 1000;
        //如果还在活动时间内
        if (endTimeLong > DateUtil.getDateline()) {
            pintuan.setPromotionStatus(PromotionStatusEnum.NEW.name());
            updatePintuanPromotionGoods(pintuan);
            this.addPintuanStartTask(pintuan);
        } else {
            //活动时间范围外，修改状态为已结束
            pintuan.setPromotionStatus(PromotionStatusEnum.END.name());
        }

        pintuan.setPromotionGoodsList(new ArrayList<>());
        result = this.updateById(pintuan);
        this.mongoTemplate.save(pintuan);
        return result;
    }

    @Override
    public boolean closePintuan(String pintuanId) {
        PintuanVO pintuan = checkExist(pintuanId);

        long endTime = pintuan.getEndTime().getTime() / 1000;
        //如果还在活动时间内
        if (endTime > DateUtil.getDateline()) {
            //表示可以再次开启，则不处理未成团订单，因为可以开启
            pintuan.setPromotionStatus(PromotionStatusEnum.CLOSE.name());
        } else {
            pintuan.setPromotionStatus(PromotionStatusEnum.END.name());
            LambdaQueryWrapper<Order> queryWrapper = new LambdaQueryWrapper<>();
            queryWrapper.eq(Order::getOrderPromotionType, PromotionTypeEnum.PINTUAN.name());
            queryWrapper.eq(Order::getPromotionId, pintuanId);
            queryWrapper.nested(i -> i.eq(Order::getPayStatus, PayStatusEnum.PAID.name()).or().eq(Order::getOrderStatus, OrderStatusEnum.PAID.name()));
            //过滤父级拼团订单，根据父级拼团订单分组
            Map<String, List<Order>> collect = orderService.list(queryWrapper).stream().filter(i -> StrUtil.isNotEmpty(i.getParentOrderSn())).collect(Collectors.groupingBy(Order::getParentOrderSn));
            this.isOpenFictitiousPintuan(pintuan, collect);

        }
        LambdaUpdateWrapper<Pintuan> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(Pintuan::getId, pintuanId).set(Pintuan::getPromotionStatus, PromotionStatusEnum.CLOSE.name());
        boolean result = this.update(updateWrapper);
        if (pintuan.getPromotionGoodsList() != null && !pintuan.getPromotionGoodsList().isEmpty()) {
            LambdaQueryWrapper<PromotionGoods> deleteWrapper = new LambdaQueryWrapper<>();
            deleteWrapper.eq(PromotionGoods::getPromotionId, pintuanId);
            promotionGoodsService.remove(deleteWrapper);
            pintuan.setPromotionGoodsList(new ArrayList<>());
        }
        this.removePintuanGoodsFromEs(pintuanId, pintuan.getStartTime().getTime());
        this.mongoTemplate.save(pintuan);
        return result;
    }

    /**
     * 删除拼团
     *
     * @param pintuanId 拼团活动编号
     * @return 是否成功
     */
    @Override
    public boolean deletePintuan(String pintuanId) {
        PintuanVO pintuanVO = this.checkExist(pintuanId);
        pintuanVO.setDeleteFlag(true);
        if (pintuanVO.getPromotionGoodsList() != null && !pintuanVO.getPromotionGoodsList().isEmpty()) {
            LambdaQueryWrapper<PromotionGoods> deleteWrapper = new LambdaQueryWrapper<>();
            deleteWrapper.eq(PromotionGoods::getPromotionId, pintuanId);
            promotionGoodsService.remove(deleteWrapper);
            pintuanVO.setPromotionGoodsList(new ArrayList<>());
        }
        boolean result = this.updateById(pintuanVO);
        this.mongoTemplate.save(pintuanVO);
        this.removePintuanGoodsFromEs(pintuanId, pintuanVO.getStartTime().getTime());
        return result;
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
        LambdaQueryWrapper<Order> queryWrapper = new LambdaQueryWrapper<>();
        //查找团长订单和已和当前拼团订单拼团的订单
        queryWrapper.eq(Order::getOrderPromotionType, PromotionTypeEnum.PINTUAN.name())
                .eq(Order::getPayStatus, OrderStatusEnum.PAID.name())
                .and(i -> i.eq(Order::getParentOrderSn, parentOrderSn).or(j -> j.eq(Order::getSn, parentOrderSn)));
        List<Order> orders = orderService.list(queryWrapper);
        this.setPintuanOrderInfo(orders, pintuanShareVO, skuId);
        //如果为根据团员订单sn查询拼团订单信息时，找到团长订单sn，然后找到所有参与到同一拼团的订单信息
        if (!orders.isEmpty() && pintuanShareVO.getPromotionGoods() == null) {
            LambdaQueryWrapper<Order> orderLambdaQueryWrapper = new LambdaQueryWrapper<>();
            //查找团长订单和已和当前拼团订单拼团的订单
            orderLambdaQueryWrapper.eq(Order::getOrderPromotionType, PromotionTypeEnum.PINTUAN.name())
                    .eq(Order::getPayStatus, OrderStatusEnum.PAID.name())
                    .ne(Order::getSn, parentOrderSn)
                    .and(i -> i.eq(Order::getParentOrderSn, orders.get(0).getParentOrderSn()).or(j -> j.eq(Order::getSn, orders.get(0).getParentOrderSn())));
            List<Order> parentOrders = orderService.list(orderLambdaQueryWrapper);
            this.setPintuanOrderInfo(parentOrders, pintuanShareVO, skuId);

        }
        return pintuanShareVO;
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
            if (StrUtil.isEmpty(order.getParentOrderSn())) {
                memberVO.setOrderSn("");
                PromotionGoods promotionGoods = promotionGoodsService.getPromotionGoods(PromotionTypeEnum.PINTUAN, order.getPromotionId(), skuId);
                if (promotionGoods == null) {
                    throw new ServiceException(ResultCode.PINTUAN_NOT_EXIST_ERROR);
                }
                pintuanShareVO.setPromotionGoods(promotionGoods);
                Pintuan pintuanById = this.getPintuanById(order.getPromotionId());
                LambdaQueryWrapper<Order> countQueryWrapper = new LambdaQueryWrapper<>();
                countQueryWrapper.eq(Order::getPayStatus, PayStatusEnum.PAID.name());
                countQueryWrapper.and(i -> i.eq(Order::getSn, order.getSn()).or(j -> j.eq(Order::getParentOrderSn, order.getSn())));
                //获取已参团人数
                int count = orderService.count(countQueryWrapper);
                //获取待参团人数
                int toBoGrouped = pintuanById.getRequiredNum() - count;
                memberVO.setGroupNum(pintuanById.getRequiredNum());
                memberVO.setGroupedNum(count);
                memberVO.setToBeGroupedNum(toBoGrouped);
            }
            pintuanShareVO.getPintuanMemberVOS().add(memberVO);
        }
    }

    private void checkSamePromotion(Date startTime, Date endTime, String storeId, String pintuanId) {
        QueryWrapper<Pintuan> queryWrapper = PromotionTools.checkActiveTime(startTime, endTime, PromotionTypeEnum.PINTUAN, storeId, pintuanId);
        List<Pintuan> list = this.list(queryWrapper);
        if (!list.isEmpty()) {
            throw new ServiceException(ResultCode.PROMOTION_SAME_ERROR);
        }
    }

    private void addPintuanStartTask(PintuanVO pintuan) {
        PromotionMessage promotionMessage = new PromotionMessage(pintuan.getId(), PromotionTypeEnum.PINTUAN.name(), PromotionStatusEnum.START.name(), pintuan.getStartTime(), pintuan.getEndTime());
        TimeTriggerMsg timeTriggerMsg = new TimeTriggerMsg(TimeExecuteConstant.PROMOTION_EXECUTOR,
                pintuan.getStartTime().getTime(),
                promotionMessage,
                DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.PROMOTION, (promotionMessage.getPromotionType() + promotionMessage.getPromotionId())),
                rocketmqCustomProperties.getPromotionTopic());
        //发送促销活动开始的延时任务
        this.timeTrigger.addDelay(timeTriggerMsg);
    }

    /**
     * 从es商品索引中中移除拼团活动
     *
     * @param id              拼团活动ID
     * @param originStartTime 活动开始时间
     */
    private void removePintuanGoodsFromEs(String id, Long originStartTime) {
        this.timeTrigger.delete(TimeExecuteConstant.PROMOTION_EXECUTOR,
                originStartTime,
                DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.PROMOTION, (PromotionTypeEnum.PINTUAN.name() + id)),
                rocketmqCustomProperties.getPromotionTopic());
    }

    /**
     * 从指定订单列表中检查是否开始虚拟成团
     *
     * @param pintuan 拼团活动信息
     * @param collect 检查的订单列表
     */
    private void isOpenFictitiousPintuan(PintuanVO pintuan, Map<String, List<Order>> collect) {
        //成团人数
        Integer requiredNum = pintuan.getRequiredNum();

        for (Map.Entry<String, List<Order>> entry : collect.entrySet()) {
            //是否开启虚拟成团
            if (Boolean.FALSE.equals(pintuan.getFictitious()) && entry.getValue().size() < requiredNum) {
                //如果未开启虚拟成团且已参团人数小于成团人数，则自动取消订单
                LambdaUpdateWrapper<Order> updateWrapper = new LambdaUpdateWrapper<>();
                updateWrapper.eq(Order::getOrderPromotionType, PromotionTypeEnum.PINTUAN.name());
                updateWrapper.eq(Order::getPromotionId, pintuan.getId());
                updateWrapper.eq(Order::getParentOrderSn, entry.getKey());
                updateWrapper.set(Order::getOrderStatus, OrderStatusEnum.CANCELLED.name());
                updateWrapper.set(Order::getCancelReason, "拼团活动结束订单未付款，系统自动取消订单");
                orderService.update(updateWrapper);
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
     * 检查当前拼团活动是否存在
     *
     * @param pintuanId 拼团id
     * @return 拼团活动
     */
    private PintuanVO checkExist(String pintuanId) {
        PintuanVO pintuan = mongoTemplate.findById(pintuanId, PintuanVO.class);
        if (pintuan == null) {
            throw new ServiceException(ResultCode.PINTUAN_NOT_EXIST_ERROR);
        }
        return pintuan;
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
            LambdaQueryWrapper<PromotionGoods> queryWrapper = new LambdaQueryWrapper<>();
            queryWrapper.eq(PromotionGoods::getPromotionId, pintuan.getId()).eq(PromotionGoods::getPromotionType, PromotionTypeEnum.PINTUAN.name());
            promotionGoodsService.remove(queryWrapper);
            promotionGoodsService.saveOrUpdateBatch(promotionGoods);
        }
    }

}