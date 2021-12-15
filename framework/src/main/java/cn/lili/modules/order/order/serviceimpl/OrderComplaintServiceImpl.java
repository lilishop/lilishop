package cn.lili.modules.order.order.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.aftersale.entity.enums.ComplaintStatusEnum;
import cn.lili.modules.order.order.entity.dos.OrderComplaint;
import cn.lili.modules.order.order.entity.dos.OrderComplaintCommunication;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.OrderComplaintDTO;
import cn.lili.modules.order.order.entity.enums.OrderComplaintStatusEnum;
import cn.lili.modules.order.order.entity.vo.*;
import cn.lili.modules.order.order.mapper.OrderComplaintMapper;
import cn.lili.modules.order.order.service.OrderComplaintCommunicationService;
import cn.lili.modules.order.order.service.OrderComplaintService;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;
import java.util.Objects;

/**
 * 交易投诉业务层实现
 *
 * @author paulG
 * @since 2020/12/5
 **/
@Service
public class OrderComplaintServiceImpl extends ServiceImpl<OrderComplaintMapper, OrderComplaint> implements OrderComplaintService {

    /**
     * 订单
     */
    @Autowired
    private OrderService orderService;
    /**
     * 订单货物
     */
    @Autowired
    private OrderItemService orderItemService;
    /**
     * 商品规格
     */
    @Autowired
    private GoodsSkuService goodsSkuService;
    /**
     * 交易投诉沟通
     */
    @Autowired
    private OrderComplaintCommunicationService orderComplaintCommunicationService;

    /**
     * 分页获取交易投诉信息
     *
     * @param searchParams 查询参数
     * @param pageVO       分页参数
     * @return 交易投诉信息
     */
    @Override
    public IPage<OrderComplaint> getOrderComplainByPage(OrderComplaintSearchParams searchParams, PageVO pageVO) {
        return this.page(PageUtil.initPage(pageVO), searchParams.lambdaQueryWrapper());
    }

    /**
     * 获取交易投诉详情
     *
     * @param id 交易投诉ID
     * @return 交易投诉详情
     */
    @Override
    public OrderComplaintVO getOrderComplainById(String id) {
        OrderComplaint orderComplaint = this.checkOrderComplainExist(id);
        LambdaQueryWrapper<OrderComplaintCommunication> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(OrderComplaintCommunication::getComplainId, id);
        List<OrderComplaintCommunication> list = orderComplaintCommunicationService.list(queryWrapper);
        OrderComplaintVO orderComplainVO = new OrderComplaintVO(orderComplaint);
        orderComplainVO.setOrderComplaintCommunications(list);
        orderComplainVO.setOrderComplaintImages(orderComplaint.getImages().split(","));
        if (orderComplaint.getAppealImages() != null) {
            orderComplainVO.setAppealImagesList(orderComplaint.getAppealImages().split(","));
        }
        return orderComplainVO;
    }

    /**
     * 获取交易投诉详情
     *
     * @param storeId 店铺id
     * @return 交易投诉详情
     */
    @Override
    public OrderComplaint getOrderComplainByStoreId(String storeId) {
        return this.getOne(new LambdaQueryWrapper<OrderComplaint>().eq(OrderComplaint::getStoreId, storeId));
    }

    /**
     * 添加交易投诉
     *
     * @param orderComplaintDTO 交易投诉信息
     * @return 添加结果
     */
    @Override
    public OrderComplaint addOrderComplain(OrderComplaintDTO orderComplaintDTO) {

        try {
            AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
            //查询订单信息
            OrderDetailVO orderDetailVO = orderService.queryDetail(orderComplaintDTO.getOrderSn());
            List<OrderItem> orderItems = orderDetailVO.getOrderItems();
            OrderItem orderItem = orderItems.stream().filter(i -> orderComplaintDTO.getSkuId().equals(i.getSkuId())).findFirst().orElse(null);

            if (orderItem == null) {
                throw new ServiceException(ResultCode.COMPLAINT_ORDER_ITEM_EMPTY_ERROR);
            }

            //新建交易投诉
            OrderComplaint orderComplaint = new OrderComplaint();
            BeanUtil.copyProperties(orderComplaintDTO, orderComplaint);

            //获取商品规格信息
            GoodsSku goodsSku = goodsSkuService.getGoodsSkuByIdFromCache(orderItem.getSkuId());
            if (goodsSku == null) {
                throw new ServiceException(ResultCode.COMPLAINT_SKU_EMPTY_ERROR);
            }
            orderComplaint.setComplainStatus(ComplaintStatusEnum.NEW.name());
            orderComplaint.setGoodsId(goodsSku.getGoodsId());
            orderComplaint.setGoodsName(goodsSku.getGoodsName());
            orderComplaint.setGoodsImage(goodsSku.getThumbnail());
            orderComplaint.setGoodsPrice(goodsSku.getPrice());
            orderComplaint.setNum(orderItem.getNum());

            //获取订单信息
            orderComplaint.setOrderTime(orderDetailVO.getOrder().getCreateTime());
            orderComplaint.setOrderPrice(orderDetailVO.getOrder().getPriceDetailDTO().getBillPrice());
            orderComplaint.setNum(orderDetailVO.getOrder().getGoodsNum());
            orderComplaint.setFreightPrice(orderDetailVO.getOrder().getPriceDetailDTO().getFreightPrice());
            orderComplaint.setLogisticsNo(orderDetailVO.getOrder().getLogisticsNo());
            orderComplaint.setConsigneeMobile(orderDetailVO.getOrder().getConsigneeMobile());
            orderComplaint.setConsigneeAddressPath(orderDetailVO.getOrder().getConsigneeAddressPath());
            orderComplaint.setConsigneeName(orderDetailVO.getOrder().getConsigneeName());

            //获取商家信息
            orderComplaint.setStoreId(orderDetailVO.getOrder().getStoreId());
            orderComplaint.setStoreName(orderDetailVO.getOrder().getStoreName());

            orderComplaint.setMemberId(currentUser.getId());
            orderComplaint.setMemberName(currentUser.getUsername());
            //保存订单投诉
            this.save(orderComplaint);

            //更新订单投诉状态
            orderItemService.updateOrderItemsComplainStatus(orderComplaint.getOrderSn(), orderComplaint.getSkuId(), orderComplaint.getId(), OrderComplaintStatusEnum.APPLYING);
            return orderComplaint;
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            log.error("订单投诉异常：", e);
            throw new ServiceException(ResultCode.COMPLAINT_ERROR);
        }
    }

    /**
     * 更新交易投诉
     *
     * @param orderComplainVO 交易投诉信息
     * @return 更新结果
     */
    @Override
    public boolean updateOrderComplain(OrderComplaintVO orderComplainVO) {
        OperationalJudgment.judgment(this.checkOrderComplainExist(orderComplainVO.getId()));
        return this.updateById(orderComplainVO);
    }

    /**
     * 修改交易投诉状态
     *
     * @param operationParam 操作参数
     * @return 修改的交易投诉
     */
    @Override
    public OrderComplaint updateOrderComplainByStatus(OrderComplaintOperationParams operationParam) {
        OrderComplaint orderComplaint = OperationalJudgment.judgment(this.checkOrderComplainExist(operationParam.getComplainId()));
        this.checkOperationParams(operationParam, orderComplaint);
        orderComplaint.setComplainStatus(operationParam.getComplainStatus());
        this.updateById(orderComplaint);
        return orderComplaint;
    }

    @Override
    public long waitComplainNum() {
        QueryWrapper queryWrapper = Wrappers.query();
        queryWrapper.ne("complain_status", ComplaintStatusEnum.COMPLETE.name());
        queryWrapper.eq(CharSequenceUtil.equals(UserContext.getCurrentUser().getRole().name(), UserEnums.STORE.name()),
                "store_id", UserContext.getCurrentUser().getStoreId());
        return this.count(queryWrapper);
    }

    @Override
    public boolean cancel(String id) {
        OrderComplaint orderComplaint = OperationalJudgment.judgment(this.getById(id));
        //如果以及仲裁，则不可以进行申诉取消
        if (orderComplaint.getComplainStatus().equals(ComplaintStatusEnum.COMPLETE.name())) {
            throw new ServiceException(ResultCode.COMPLAINT_CANCEL_ERROR);
        }
        LambdaUpdateWrapper<OrderComplaint> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.eq(OrderComplaint::getId, id);
        lambdaUpdateWrapper.set(OrderComplaint::getComplainStatus, ComplaintStatusEnum.CANCEL.name());
        return this.update(lambdaUpdateWrapper);
    }

    @Override
    public boolean appeal(StoreAppealVO storeAppealVO) {
        //获取投诉信息
        OrderComplaint orderComplaint = OperationalJudgment.judgment(this.checkOrderComplainExist(storeAppealVO.getOrderComplaintId()));
        orderComplaint.setAppealContent(storeAppealVO.getAppealContent());
        orderComplaint.setAppealImages(storeAppealVO.getAppealImages());
        orderComplaint.setAppealTime(new Date());
        orderComplaint.setComplainStatus(ComplaintStatusEnum.WAIT_ARBITRATION.name());
        this.updateById(orderComplaint);
        return true;
    }

    private OrderComplaint checkOrderComplainExist(String id) {
        OrderComplaint orderComplaint = this.getById(id);
        if (orderComplaint == null) {
            throw new ServiceException(ResultCode.COMPLAINT_NOT_EXIT);
        }
        return orderComplaint;
    }

    private void checkOperationParams(OrderComplaintOperationParams operationParam, OrderComplaint orderComplaint) {
        ComplaintStatusEnum complaintStatusEnum = ComplaintStatusEnum.valueOf(operationParam.getComplainStatus());
        if (complaintStatusEnum == ComplaintStatusEnum.COMPLETE) {
            if (CharSequenceUtil.isEmpty(operationParam.getArbitrationResult())) {
                throw new ServiceException(ResultCode.COMPLAINT_ARBITRATION_RESULT_ERROR);
            }
            orderComplaint.setArbitrationResult(operationParam.getArbitrationResult());
        } else if (complaintStatusEnum == ComplaintStatusEnum.COMMUNICATION) {
            if (CharSequenceUtil.isEmpty(operationParam.getAppealContent()) || operationParam.getImages() == null) {
                throw new ServiceException(ResultCode.COMPLAINT_APPEAL_CONTENT_ERROR);
            }
            orderComplaint.setContent(operationParam.getAppealContent());
            orderComplaint.setImages(operationParam.getImages().get(0));
        }
    }

}
