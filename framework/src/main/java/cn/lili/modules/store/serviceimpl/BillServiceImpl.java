package cn.lili.modules.store.serviceimpl;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.io.IoUtil;
import cn.hutool.poi.excel.ExcelUtil;
import cn.hutool.poi.excel.ExcelWriter;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.order.order.entity.dto.StoreFlowQueryDTO;
import cn.lili.modules.order.order.entity.enums.FlowTypeEnum;
import cn.lili.modules.order.order.service.StoreFlowService;
import cn.lili.modules.store.entity.dos.Bill;
import cn.lili.modules.store.entity.dto.BillSearchParams;
import cn.lili.modules.store.entity.enums.BillStatusEnum;
import cn.lili.modules.store.entity.vos.BillListVO;
import cn.lili.modules.store.entity.vos.StoreDetailVO;
import cn.lili.modules.store.entity.vos.StoreFlowPayDownloadVO;
import cn.lili.modules.store.entity.vos.StoreFlowRefundDownloadVO;
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
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;
import java.net.URLEncoder;
import java.util.Date;
import java.util.List;

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
    private StoreFlowService storeFlowService;

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

        //退款结算信息
        Bill refundBill = this.baseMapper.getRefundBill(new QueryWrapper<Bill>().eq("store_id", storeId).eq("flow_type", FlowTypeEnum.REFUND.name()).between("create_time", startTime, endTime));
        //店铺退款金额
        Double refundPrice = 0D;
        if (refundBill != null) {
            //退单金额
            bill.setRefundPrice(refundBill.getRefundPrice() != null ? refundBill.getRefundPrice() : 0D);
            //退单产生退还佣金金额
            bill.setRefundCommissionPrice(refundBill.getRefundCommissionPrice() != null ? refundBill.getRefundCommissionPrice() : 0D);
            //分销订单退还，返现佣金返还
            bill.setDistributionRefundCommission(refundBill.getDistributionRefundCommission() != null ? refundBill.getRefundCommissionPrice() : 0D);
            //退货平台优惠券补贴返还
            bill.setSiteCouponRefundCommission(refundBill.getSiteCouponRefundCommission() != null ? refundBill.getSiteCouponRefundCommission() : 0D);
            //退款金额=店铺最终退款结算金额
            refundPrice = refundBill.getBillPrice() != null ? refundBill.getBillPrice() : 0D;
        }


        /**
         *  @TODO 入账结算信息
         */
        Bill orderBill = this.baseMapper.getOrderBill(new QueryWrapper<Bill>().eq("store_id", storeId).eq("flow_type", FlowTypeEnum.PAY.name()).between("create_time", startTime, endTime));
        //店铺入款结算金额
        double orderPrice = 0D;

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

            //入款结算金额= 店铺支付结算金额 + 平台优惠券补贴 + 分销订单退还，返现佣金返还+退单产生退还佣金金额
            orderPrice = CurrencyUtil.add(orderBill.getBillPrice() == null ? 0 : orderBill.getBillPrice(), bill.getSiteCouponCommission() == null ? 0 : bill.getSiteCouponCommission(), bill.getDistributionRefundCommission() == null ? 0 : bill.getDistributionRefundCommission(), bill.getRefundCommissionPrice() == null ? 0 : bill.getRefundCommissionPrice());
        }
        //最终结算金额=入款结算金额-退款结算金额-退货平台优惠券补贴返还
        Double finalPrice = CurrencyUtil.sub(orderPrice, refundPrice, bill.getSiteCouponRefundCommission() == null ? 0 : bill.getSiteCouponRefundCommission());
        //店铺最终结算金额=最终结算金额
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
        lambdaUpdateWrapper.set(Bill::getPayTime, new Date());
        lambdaUpdateWrapper.set(Bill::getBillStatus, BillStatusEnum.COMPLETE.name());
        return this.update(lambdaUpdateWrapper);
    }

    @Override
    public void download(HttpServletResponse response, String id) {

        Bill bill = this.getById(id);
        ExcelWriter writer = ExcelUtil.getWriterWithSheet("入账订单");
        writer.setSheet("入账订单");
        writer.addHeaderAlias("createTime", "入账时间");
        writer.setColumnWidth(0, 20);
        writer.addHeaderAlias("orderSn", "订单编号");
        writer.setColumnWidth(1, 35);
        writer.addHeaderAlias("storeName", "店铺名称");
        writer.setColumnWidth(2, 20);
        writer.addHeaderAlias("goodsName", "商品名称");
        writer.setColumnWidth(3, 70);
        writer.addHeaderAlias("num", "销售量");
        writer.addHeaderAlias("finalPrice", "订单金额");
        writer.addHeaderAlias("commissionPrice", "平台分佣");
        writer.addHeaderAlias("siteCouponPrice", "平台优惠券");
        writer.setColumnWidth(7, 12);
        writer.addHeaderAlias("distributionRebate", "分销金额");
        writer.addHeaderAlias("pointSettlementPrice", "积分结算金额");
        writer.setColumnWidth(9, 12);
        writer.addHeaderAlias("kanjiaSettlementPrice", "砍价结算金额");
        writer.setColumnWidth(10, 12);
        writer.addHeaderAlias("billPrice", "应结金额");
        writer.setColumnWidth(11, 20);


        List<StoreFlowPayDownloadVO> storeFlowList = storeFlowService.getStoreFlowPayDownloadVO(StoreFlowQueryDTO.builder().type(FlowTypeEnum.PAY.name()).bill(bill).build());
        writer.write(storeFlowList, true);

        writer.setSheet("退款订单");
        writer.addHeaderAlias("createTime", "入账时间");
        writer.setColumnWidth(0, 20);
        writer.addHeaderAlias("orderSn", "订单编号");
        writer.setColumnWidth(1, 35);
        writer.addHeaderAlias("refundSn", "售后单号");
        writer.setColumnWidth(2, 35);
        writer.addHeaderAlias("storeName", "店铺名称");
        writer.setColumnWidth(3, 20);
        writer.addHeaderAlias("goodsName", "商品名称");
        writer.setColumnWidth(4, 70);
        writer.addHeaderAlias("num", "销售量");
        writer.addHeaderAlias("finalPrice", "退款金额");
        writer.addHeaderAlias("commissionPrice", "平台分佣");
        writer.addHeaderAlias("siteCouponPrice", "平台优惠券");
        writer.setColumnWidth(8, 12);
        writer.addHeaderAlias("distributionRebate", "分销金额");
        writer.addHeaderAlias("pointSettlementPrice", "积分结算金额");
        writer.setColumnWidth(10, 12);
        writer.addHeaderAlias("kanjiaSettlementPrice", "砍价结算金额");
        writer.setColumnWidth(11, 12);
        writer.addHeaderAlias("billPrice", "结算金额");
        writer.setColumnWidth(12, 20);


        List<StoreFlowRefundDownloadVO> storeFlowRefundDownloadVOList = storeFlowService.getStoreFlowRefundDownloadVO(StoreFlowQueryDTO.builder().type(FlowTypeEnum.REFUND.name()).bill(bill).build());
        writer.write(storeFlowRefundDownloadVOList, true);

        ServletOutputStream out = null;
        try {
            //设置公共属性，列表名称
            response.setContentType("application/vnd.ms-excel;charset=utf-8");
            response.setHeader("Content-Disposition", "attachment;filename=" + URLEncoder.encode(bill.getStoreName() + "-" + bill.getSn(), "UTF8") + ".xls");
            out = response.getOutputStream();
            writer.flush(out, true);
        } catch (Exception e) {
            log.error("下载结算单错误", e);
        } finally {
            writer.close();
            IoUtil.close(out);
        }
    }

}
