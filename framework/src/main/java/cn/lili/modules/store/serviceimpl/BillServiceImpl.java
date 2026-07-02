package cn.lili.modules.store.serviceimpl;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.modules.finance.service.FinanceExportService;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.SnowFlake;
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
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import jakarta.servlet.http.HttpServletResponse;
import java.util.Date;

/**
 * 结算单业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 4:28 下午
 */
@Service
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
    @Lazy
    private StoreFlowService storeFlowService;

    @Autowired
    private FinanceExportService financeExportService;

    @Override
    public void createBill(String storeId, Date startTime, DateTime endTime) {

        //获取结算店铺
        StoreDetailVO store = storeDetailService.getStoreDetailVO(storeId);
        Bill bill = new Bill();

        /**
         * @TODO 结算单基础信息
         */
        bill.setStartTime(startTime);
        bill.setEndTime(endTime);
        bill.setBillStatus(BillStatusEnum.OUT.name());
        bill.setStoreId(storeId);
        bill.setStoreName(store.getStoreName());

        /**
         * @TODO 结算单基础信息
         */
        bill.setBankAccountName(store.getSettlementBankAccountName());
        bill.setBankAccountNumber(store.getSettlementBankAccountNum());
        bill.setBankCode(store.getSettlementBankJointName());
        bill.setBankName(store.getSettlementBankBranchName());

        //店铺结算单号
        bill.setSn(SnowFlake.createStr("B"));

        //结算金额初始化
        initBillPrice(bill, storeId, startTime, endTime);
        //添加结算单
        this.save(bill);

    }

    /**
     * 结算单金额初始化
     *
     * @param bill      结算单
     * @param storeId   店铺ID
     * @param startTime 开始时间
     * @param endTime   结束时间
     */
    private void initBillPrice(Bill bill, String storeId, Date startTime, DateTime endTime) {

        //退款结算信息
        Bill refundBill = storeFlowService.getRefundBill(BillSearchParams.builder()
                .storeId(storeId)
                .flowType(FlowTypeEnum.REFUND.name())
                .startTime(startTime)
                .endTime(endTime)
                .build());
        //店铺退款金额
        if (refundBill != null) {
            //退单金额
            bill.setRefundPrice(refundBill.getRefundPrice() != null ? refundBill.getRefundPrice() : 0D);
            //退单 平台服务费返还
            bill.setRefundCommissionPrice(refundBill.getRefundCommissionPrice() != null ? refundBill.getRefundCommissionPrice() : 0D);
            //退单 分销佣金返还
            bill.setDistributionRefundCommission(refundBill.getDistributionRefundCommission() != null ? refundBill.getDistributionRefundCommission() : 0D);
            //退单 平台优惠券补贴返还
            bill.setSiteCouponRefundCommission(refundBill.getSiteCouponRefundCommission() != null ? refundBill.getSiteCouponRefundCommission() : 0D);
            //退单 平台优惠券补贴返还
            bill.setPointRefundSettlementPrice(refundBill.getPointRefundSettlementPrice() != null ? refundBill.getPointRefundSettlementPrice() : 0D);
            //退单 砍价补贴返还
            bill.setKanjiaRefundSettlementPrice(refundBill.getKanjiaRefundSettlementPrice() != null ? refundBill.getKanjiaRefundSettlementPrice() : 0D);
            bill.setGiftCardRefundSubsidy(refundBill.getGiftCardRefundSubsidy() != null ? refundBill.getGiftCardRefundSubsidy() : 0D);

        }


        //入账
        Bill orderBill = this.storeFlowService.getOrderBill(BillSearchParams.builder()
                .storeId(storeId)
                .flowType(FlowTypeEnum.PAY.name())
                .startTime(startTime)
                .endTime(endTime)
                .build());
        //店铺入款结算金额

        if (orderBill != null) {
            //结算周期内订单付款总金额
            bill.setOrderPrice(orderBill.getOrderPrice() != null ? orderBill.getOrderPrice() : 0D);
            //平台收取佣金
            bill.setCommissionPrice(orderBill.getCommissionPrice() != null ? orderBill.getCommissionPrice() : 0D);
            //分销返现支出
            bill.setDistributionCommission(orderBill.getDistributionCommission() != null ? orderBill.getDistributionCommission() : 0D);
            //平台优惠券补贴
            bill.setSiteCouponCommission(orderBill.getSiteCouponCommission() != null ? orderBill.getSiteCouponCommission() : 0D);
            //积分商品结算价格
            bill.setPointSettlementPrice(orderBill.getPointSettlementPrice() != null ? orderBill.getPointSettlementPrice() : 0D);
            //砍价商品结算价格
            bill.setKanjiaSettlementPrice(orderBill.getKanjiaSettlementPrice() != null ? orderBill.getKanjiaSettlementPrice() : 0D);
            bill.setGiftCardSubsidy(orderBill.getGiftCardSubsidy() != null ? orderBill.getGiftCardSubsidy() : 0D);
        }
        //最终结算金额=入款结算金额-退款结算金额
        Double finalPrice = CurrencyUtil.sub(orderBill.getBillPrice(), refundBill.getBillPrice());
        //店铺最终结算金额=最终结算金额
        bill.setBillPrice(finalPrice);

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
    public IPage<BillListVO> billPage(BillSearchParams billSearchParams) {
        QueryWrapper<BillListVO> queryWrapper = billSearchParams.queryWrapperBillList();
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
        lambdaUpdateWrapper.set(Bill::getPayTime, new Date());
        lambdaUpdateWrapper.set(Bill::getBillStatus, BillStatusEnum.COMPLETE.name());
        return this.update(lambdaUpdateWrapper);
    }

    /**
     * 下载结算单 Excel，委托 {@link FinanceExportService} 统一导出（xlsx，含礼品卡等完整字段）。
     * 保持原有 URL 兼容，管理端与商家端 BillController 均调用此方法。
     */
    @Override
    public void download(HttpServletResponse response, String id) {
        financeExportService.exportBill(response, id);
    }

}
