package cn.lili.modules.store.serviceimpl;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.order.order.entity.enums.FlowTypeEnum;
import cn.lili.modules.order.order.service.StoreFlowService;
import cn.lili.modules.store.entity.dos.Bill;
import cn.lili.modules.store.entity.dto.BillSearchParams;
import cn.lili.modules.store.entity.enums.BillStatusEnum;
import cn.lili.modules.store.entity.vos.BillListVO;
import cn.lili.modules.store.entity.vos.StoreDetailVO;
import cn.lili.modules.store.mapper.BillMapper;
import cn.lili.modules.store.service.BillService;
import cn.lili.modules.store.service.StoreDetailService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;

/**
 * 结算单业务层实现
 *
 * @author Chopper
 * @date 2020/11/17 4:28 下午
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class BillServiceImpl extends ServiceImpl<BillMapper, Bill> implements BillService {

    /**
     * 店铺详情
     */
    @Autowired
    private StoreDetailService storeDetailService;
    /**
     * 商家流水
     */
    @Autowired
    private StoreFlowService storeFlowService;

    @Override
    public void createBill(String storeId, Date startTime, DateTime endTime) {

        //获取结算店铺
        StoreDetailVO store = storeDetailService.getStoreDetailVO(storeId);
        Bill bill = new Bill();

        //结算基础信息
        bill.setStartTime(startTime);
        bill.setEndTime(DateUtil.yesterday());
        bill.setBillStatus(BillStatusEnum.OUT.name());
        bill.setStoreId(storeId);
        bill.setStoreName(store.getStoreName());

        //设置结算信息
        bill.setBankAccountName(store.getSettlementBankAccountName());
        bill.setBankAccountNumber(store.getSettlementBankAccountNum());
        bill.setBankCode(store.getSettlementBankJointName());
        bill.setBankName(store.getSettlementBankBranchName());

        //店铺结算单号
        bill.setSn(SnowFlake.createStr("B"));

        //入账结算信息
        Bill orderBill = this.baseMapper.getOrderBill(new QueryWrapper<Bill>()
                .eq("store_id", storeId)
                .eq("flow_type", FlowTypeEnum.PAY.name())
                .between("create_time", startTime, endTime));
        Double orderPrice = 0D;
        if (orderBill != null) {
            bill.setOrderPrice(orderBill.getOrderPrice());
            bill.setCommissionPrice(orderBill.getCommissionPrice());
            bill.setDistributionCommission(orderBill.getDistributionCommission());
            bill.setSiteCouponCommission(orderBill.getSiteCouponCommission());
            orderPrice = orderBill.getBillPrice();
        }


        //退款结算信息
        Bill refundBill = this.baseMapper.getRefundBill(new QueryWrapper<Bill>()
                .eq("store_id", storeId)
                .eq("flow_type", FlowTypeEnum.REFUND.name())
                .between("create_time", startTime, endTime));
        Double refundPrice = 0D;
        if (refundBill != null) {
            bill.setRefundPrice(refundBill.getRefundPrice());
            bill.setRefundCommissionPrice(refundBill.getRefundCommissionPrice());
            bill.setDistributionRefundCommission(refundBill.getDistributionRefundCommission());
            bill.setSiteCouponRefundCommission(refundBill.getSiteCouponRefundCommission());
            refundPrice = refundBill.getBillPrice();
        }

        //最终结算金额=入款结算金额-退款结算金额
        Double finalPrice = CurrencyUtil.sub(orderPrice, refundPrice);
        bill.setBillPrice(finalPrice);

        //添加结算单
        this.save(bill);

    }

    /**
     * 立即结算
     *
     * @param storeId
     * @param endTime 结束时间
     */
    @Override
    @Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
    public void immediatelyBill(String storeId, Long endTime) {

//       Long now = DateUtil.getDateline();
//       //TODO 需要获取真实店铺
//       StoreDetailVO store = new StoreDetailVO();
//       Long startTime = store.getLastBillTime().getTime();
//
//       store.setLastBillTime(new Date(now));
////     TODO   store.save 保存新的结束时间
//
//       //TODO 获取结算周期内的结算详情
//       BillDTO billDTO = new BillDTO();
//
//       //如果没有需要结算单，那么就可以直接返回，也不需要保存新的结算单
//       if (billDTO.getOrderPrice() == 0 && billDTO.getRefundPrice() == 0) {
//           return;
//       }
//
//       this.createBill(storeId, startTime, endTime);
    }

    @Override
    public IPage<StoreFlow> getStoreFlow(String id, String type, PageVO pageVO) {
        Bill bill = this.getById(id);
        return storeFlowService.getStoreFlow(bill.getStoreId(), type, false, pageVO, bill.getStartTime(), bill.getCreateTime());
    }

    @Override
    public IPage<StoreFlow> getDistributionFlow(String id, PageVO pageVO) {
        Bill bill = this.getById(id);
        return storeFlowService.getStoreFlow(bill.getStoreId(), null, true, pageVO, bill.getStartTime(), bill.getCreateTime());
    }

    @Override
    public IPage<BillListVO> billPage(BillSearchParams billSearchParams) {
        QueryWrapper<BillListVO> queryWrapper = billSearchParams.queryWrapper();
        return this.baseMapper.queryBillPage(PageUtil.initPage(billSearchParams), queryWrapper);
    }

    @Override
    public boolean check(String id) {
        Bill bill = this.getById(id);
        //判断当前结算单状态为：出账
        if (!bill.getBillStatus().equals(BillStatusEnum.OUT.name())) {
            throw new ServiceException(ResultCode.BILL_CHECK_ERROR);
        }
        //判断操作人员为商家
        if (!UserContext.getCurrentUser().getRole().equals(UserEnums.STORE)) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        LambdaUpdateWrapper<Bill> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.eq(Bill::getId, id);
        lambdaUpdateWrapper.set(Bill::getBillStatus, BillStatusEnum.CHECK.name());
        return this.update(lambdaUpdateWrapper);
    }

    @Override
    public boolean complete(String id) {
        Bill bill = this.getById(id);
        //判断当前结算单状态为：已核对
        if (!bill.getBillStatus().equals(BillStatusEnum.CHECK.name())) {
            throw new ServiceException(ResultCode.BILL_COMPLETE_ERROR);
        }
        //判断操作人员为后台管理员
        if (!UserContext.getCurrentUser().getRole().equals(UserEnums.MANAGER)) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        LambdaUpdateWrapper<Bill> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.eq(Bill::getId, id);
        lambdaUpdateWrapper.set(Bill::getBillStatus, BillStatusEnum.COMPLETE.name());
        return this.update(lambdaUpdateWrapper);
    }

    @Override
    public Integer billNum(BillStatusEnum billStatusEnum) {
        LambdaUpdateWrapper<Bill> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.eq(Bill::getBillStatus, billStatusEnum.name());
        lambdaUpdateWrapper.eq(StringUtils.equals(UserContext.getCurrentUser().getRole().name(), UserEnums.STORE.name()),
                Bill::getStoreId, UserContext.getCurrentUser().getStoreId());
        return this.count(lambdaUpdateWrapper);
    }

}
