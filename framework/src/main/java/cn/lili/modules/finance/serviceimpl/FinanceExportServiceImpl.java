package cn.lili.modules.finance.serviceimpl;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.poi.excel.ExcelWriter;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.finance.entity.dto.FinanceStoreFlowSearchParams;
import cn.lili.modules.finance.export.FinanceExportHelper;
import cn.lili.modules.finance.service.FinanceExportService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.order.order.entity.dto.StoreFlowQueryDTO;
import cn.lili.modules.order.order.entity.enums.FlowTypeEnum;
import cn.lili.modules.order.order.entity.vo.PaymentLog;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.order.service.StoreFlowService;
import cn.lili.modules.order.trade.entity.vo.DepositQueryVO;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.service.RefundLogService;
import cn.lili.modules.store.entity.dos.Bill;
import cn.lili.modules.store.entity.dto.BillSearchParams;
import cn.lili.modules.store.entity.enums.BillStatusEnum;
import cn.lili.modules.store.entity.vos.BillListVO;
import cn.lili.modules.store.entity.vos.StoreFlowPayDownloadVO;
import cn.lili.modules.store.entity.vos.StoreFlowRefundDownloadVO;
import cn.lili.modules.store.service.BillService;
import cn.lili.modules.wallet.entity.dos.WalletLog;
import cn.lili.modules.wallet.service.WalletLogService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import java.io.ByteArrayOutputStream;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * 财务导出服务实现。
 * <p>
 * 大数据量导出采用分页批量写入（每批 {@link #BATCH_SIZE} 行），
 * 写入前通过 {@link FinanceExportHelper#checkRowLimit(long)} 校验总行数不超过 5 万。
 * 结算单导出包含三个 Sheet：汇总信息、入账订单、退款订单。
 */
@Service
public class FinanceExportServiceImpl implements FinanceExportService {

    /** 分页查询每批条数，用于流式写入 Excel，避免一次性加载全表 */
    private static final int BATCH_SIZE = 1000;

    @Autowired
    @Lazy
    private BillService billService;

    @Autowired
    private StoreFlowService storeFlowService;

    @Autowired
    private OrderService orderService;

    @Autowired
    private RefundLogService refundLogService;

    @Autowired
    private WalletLogService walletLogService;

    @Override
    public void exportBill(HttpServletResponse response, String billId) {
        Bill bill = billService.getById(billId);
        if (bill == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "结算单不存在");
        }
        writeBillExcel(response, bill, "店铺结算单-" + bill.getSn());
    }

    @Override
    public void exportBillList(HttpServletResponse response, BillSearchParams params) {
        params.setPageNumber(1);
        params.setPageSize(BATCH_SIZE);
        List<BillListVO> all = new ArrayList<>();
        while (true) {
            IPage<BillListVO> page = billService.billPage(params);
            if (page.getRecords() == null || page.getRecords().isEmpty()) {
                break;
            }
            all.addAll(page.getRecords());
            if (page.getCurrent() >= page.getPages()) {
                break;
            }
            params.setPageNumber(params.getPageNumber() + 1);
        }
        FinanceExportHelper.checkRowLimit(all.size());
        FinanceExportHelper.writeExcel(response, "结算单列表", writer -> {
            writer.setSheet("结算单列表");
            writer.writeHeadRow(List.of("账单号", "生成时间", "结算开始", "结算结束", "店铺", "结算金额", "状态"));
            List<List<Object>> rows = new ArrayList<>();
            for (BillListVO vo : all) {
                rows.add(List.of(vo.getSn(), formatDate(vo.getCreateTime()), formatDate(vo.getStartTime()),
                        formatDate(vo.getEndTime()), vo.getStoreName(), vo.getBillPrice(),
                        billStatusText(vo.getBillStatus())));
            }
            writer.write(rows, false);
        });
    }

    @Override
    public void batchDownloadBills(HttpServletResponse response, List<String> billIds) {
        if (billIds == null || billIds.isEmpty()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "请选择结算单");
        }
        FinanceExportHelper.checkRowLimit(billIds.size());
        try {
            response.setContentType("application/zip");
            response.setHeader("Content-Disposition",
                    "attachment;filename=" + URLEncoder.encode("结算单批量下载", StandardCharsets.UTF_8) + ".zip");
            ZipOutputStream zipOut = new ZipOutputStream(response.getOutputStream());
            for (String id : billIds) {
                Bill bill = billService.getById(id);
                if (bill == null) {
                    continue;
                }
                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                writeBillToStream(bos, bill);
                zipOut.putNextEntry(new ZipEntry("结算单-" + bill.getSn() + ".xlsx"));
                zipOut.write(bos.toByteArray());
                zipOut.closeEntry();
            }
            zipOut.finish();
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            throw new ServiceException(ResultCode.ERROR);
        }
    }

    @Override
    public void exportPaymentFlow(HttpServletResponse response, Order order, SearchVO searchVO) {
        long total = countPaymentLogs(order, searchVO);
        FinanceExportHelper.checkRowLimit(total);
        FinanceExportHelper.writeExcel(response, "支付流水", writer -> {
            writer.setSheet("支付流水");
            writer.writeHeadRow(List.of("订单号", "交易号", "店铺", "会员", "支付方式", "实付金额",
                    "支付时间", "第三方流水", "支付交易号", "客户端", "订单类型"));
            int pageNumber = 1;
            while (true) {
                PageVO pageVO = new PageVO();
                pageVO.setPageNumber(pageNumber);
                pageVO.setPageSize(BATCH_SIZE);
                IPage<PaymentLog> page = orderService.queryPaymentLogs(order, searchVO, pageVO);
                if (page.getRecords() == null || page.getRecords().isEmpty()) {
                    break;
                }
                List<List<Object>> rows = new ArrayList<>();
                for (PaymentLog log : page.getRecords()) {
                    rows.add(List.of(log.getSn(), log.getTradeSn(), log.getStoreName(), log.getMemberName(),
                            log.getPaymentMethod(), log.getFlowPrice(), log.getPaymentTime(),
                            log.getReceivableNo(), log.getPayOrderNo(), log.getClientType(), log.getOrderType()));
                }
                writer.write(rows, false);
                if (page.getCurrent() >= page.getPages()) {
                    break;
                }
                pageNumber++;
            }
        });
    }

    @Override
    public void exportRefundFlow(HttpServletResponse response, RefundLog entity, SearchVO searchVO) {
        long total = refundLogService.count(PageUtil.initWrapper(entity, searchVO));
        FinanceExportHelper.checkRowLimit(total);
        FinanceExportHelper.writeExcel(response, "退款流水", writer -> {
            writer.setSheet("退款流水");
            writer.writeHeadRow(List.of("售后单号", "订单号", "会员ID", "退款金额", "支付金额",
                    "是否已退款", "退款方式", "第三方流水", "退款请求流水", "创建时间"));
            int pageNumber = 1;
            while (true) {
                PageVO pageVO = new PageVO();
                pageVO.setPageNumber(pageNumber);
                pageVO.setPageSize(BATCH_SIZE);
                IPage<RefundLog> page = refundLogService.getByPage(entity, searchVO, pageVO);
                if (page.getRecords() == null || page.getRecords().isEmpty()) {
                    break;
                }
                List<List<Object>> rows = new ArrayList<>();
                for (RefundLog log : page.getRecords()) {
                    rows.add(List.of(log.getAfterSaleNo(), log.getOrderSn(), log.getMemberId(),
                            log.getTotalAmount(), log.getPayPrice(), log.getIsRefund(),
                            log.getPaymentName(), log.getPaymentReceivableNo(), log.getOutOrderNo(),
                            log.getCreateTime()));
                }
                writer.write(rows, false);
                if (page.getCurrent() >= page.getPages()) {
                    break;
                }
                pageNumber++;
            }
        });
    }

    @Override
    public void exportWalletLog(HttpServletResponse response, String memberId, String memberName,
                                String startDate, String endDate) {
        DepositQueryVO query = new DepositQueryVO(memberId, memberName, startDate, endDate);
        LambdaQueryWrapper<WalletLog> wrapper = buildWalletWrapper(query);
        long total = walletLogService.count(wrapper);
        FinanceExportHelper.checkRowLimit(total);
        FinanceExportHelper.writeExcel(response, "钱包流水", writer -> {
            writer.setSheet("钱包流水");
            writer.writeHeadRow(List.of("会员", "金额", "业务类型", "明细", "创建时间"));
            int pageNumber = 1;
            while (true) {
                Page<WalletLog> page = new Page<>(pageNumber, BATCH_SIZE);
                IPage<WalletLog> result = walletLogService.page(page, wrapper);
                if (result.getRecords() == null || result.getRecords().isEmpty()) {
                    break;
                }
                List<List<Object>> rows = new ArrayList<>();
                for (WalletLog log : result.getRecords()) {
                    rows.add(List.of(log.getMemberName(), log.getMoney(), log.getServiceType(),
                            log.getDetail(), log.getCreateTime()));
                }
                writer.write(rows, false);
                if (result.getCurrent() >= result.getPages()) {
                    break;
                }
                pageNumber++;
            }
        });
    }

    @Override
    public void exportStoreFlow(HttpServletResponse response, FinanceStoreFlowSearchParams params) {
        LambdaQueryWrapper<StoreFlow> wrapper = buildStoreFlowWrapper(params);
        long total = storeFlowService.count(wrapper);
        FinanceExportHelper.checkRowLimit(total);
        FinanceExportHelper.writeExcel(response, "店铺流水", writer -> {
            writer.setSheet("店铺流水");
            writer.writeHeadRow(List.of("时间", "类型", "订单号", "售后单号", "店铺", "商品", "数量",
                    "实付", "礼品卡补贴", "结算基数", "佣金", "券补贴", "分销", "应结", "支付方式", "第三方流水"));
            int pageNumber = 1;
            while (true) {
                PageVO pageVO = new PageVO();
                pageVO.setPageNumber(pageNumber);
                pageVO.setPageSize(BATCH_SIZE);
                Page<StoreFlow> page = new Page<>(pageNumber, BATCH_SIZE);
                IPage<StoreFlow> result = storeFlowService.page(page, wrapper);
                if (result.getRecords() == null || result.getRecords().isEmpty()) {
                    break;
                }
                List<List<Object>> rows = new ArrayList<>();
                for (StoreFlow flow : result.getRecords()) {
                    rows.add(List.of(flow.getCreateTime(), flow.getFlowType(), flow.getOrderSn(),
                            flow.getRefundSn(), flow.getStoreName(), flow.getGoodsName(), flow.getNum(),
                            flow.getFinalPrice(), flow.getGiftCardSubsidyPrice(), flow.getSettlementBasePrice(),
                            flow.getCommissionPrice(), flow.getSiteCouponCommission(), flow.getDistributionRebate(),
                            flow.getBillPrice(), flow.getPaymentName(), flow.getTransactionId()));
                }
                writer.write(rows, false);
                if (result.getCurrent() >= result.getPages()) {
                    break;
                }
                pageNumber++;
            }
        });
    }

    /** 写出单张结算单到 HTTP 响应 */
    private void writeBillExcel(HttpServletResponse response, Bill bill, String fileName) {
        FinanceExportHelper.writeExcel(response, fileName, writer -> writeBillContent(writer, bill));
    }

    /** 写出单张结算单到内存流，供 ZIP 批量打包使用 */
    private void writeBillToStream(ByteArrayOutputStream bos, Bill bill) {
        ExcelWriter writer = cn.hutool.poi.excel.ExcelUtil.getWriter(true);
        try {
            writeBillContent(writer, bill);
            writer.flush(bos, true);
        } finally {
            writer.close();
        }
    }

    /**
     * 填充结算单 Excel 内容。
     * 汇总 Sheet 字段与页面展示及 {@link Bill} 实体一一对应，含礼品卡补贴等完整口径。
     */
    private void writeBillContent(ExcelWriter writer, Bill bill) {
        writer.setSheet("店铺结算单");
        Map<String, Object> map = new LinkedHashMap<>();
        map.put("创建时间", DateUtil.format(bill.getCreateTime(), "yyyy-MM-dd"));
        map.put("账单号", bill.getSn());
        map.put("结算开始时间", DateUtil.format(bill.getStartTime(), "yyyy-MM-dd"));
        map.put("结算结束时间", DateUtil.format(bill.getEndTime(), "yyyy-MM-dd"));
        map.put("账单状态", BillStatusEnum.valueOf(bill.getBillStatus()).description());
        map.put("店铺名称", bill.getStoreName());
        map.put("平台付款时间", bill.getPayTime() != null ? DateUtil.format(bill.getPayTime(), "yyyy-MM-dd") : "");
        map.put("银行开户名", bill.getBankAccountName());
        map.put("银行账号", bill.getBankAccountNumber());
        map.put("开户行", bill.getBankName());
        map.put("联行号", bill.getBankCode());
        map.put("订单金额", bill.getOrderPrice());
        map.put("退单金额", bill.getRefundPrice());
        map.put("平台收取服务费", bill.getCommissionPrice());
        map.put("退单退回平台服务费", bill.getRefundCommissionPrice());
        map.put("分销佣金", bill.getDistributionCommission());
        map.put("退单退还分销佣金", bill.getDistributionRefundCommission());
        map.put("平台优惠券补贴", bill.getSiteCouponCommission());
        map.put("退单退回平台优惠券补贴", bill.getSiteCouponRefundCommission());
        map.put("积分商品补贴", bill.getPointSettlementPrice());
        map.put("退单退回积分商品补贴", bill.getPointRefundSettlementPrice());
        map.put("砍价商品补贴", bill.getKanjiaSettlementPrice());
        map.put("退单退回砍价补贴", bill.getKanjiaRefundSettlementPrice());
        map.put("礼品卡平台补贴", bill.getGiftCardSubsidy());
        map.put("退单退回礼品卡补贴", bill.getGiftCardRefundSubsidy());
        map.put("最终结算金额", bill.getBillPrice());
        writer.writeRow(map, true);

        writePayFlowSheet(writer, bill);
        writeRefundFlowSheet(writer, bill);
    }

    /** 入账订单明细 Sheet */
    private void writePayFlowSheet(ExcelWriter writer, Bill bill) {
        writer.setSheet("入账订单");
        configureFlowHeaders(writer, false);
        List<StoreFlowPayDownloadVO> list = storeFlowService.getStoreFlowPayDownloadVO(
                StoreFlowQueryDTO.builder().type(FlowTypeEnum.PAY.name()).bill(bill).build());
        FinanceExportHelper.checkRowLimit(list.size());
        writer.write(list, true);
        writer.setOnlyAlias(true);
    }

    /** 退款订单明细 Sheet */
    private void writeRefundFlowSheet(ExcelWriter writer, Bill bill) {
        writer.setSheet("退款订单");
        configureFlowHeaders(writer, true);
        List<StoreFlowRefundDownloadVO> list = storeFlowService.getStoreFlowRefundDownloadVO(
                StoreFlowQueryDTO.builder().type(FlowTypeEnum.REFUND.name()).bill(bill).build());
        FinanceExportHelper.checkRowLimit(list.size());
        writer.write(list, true);
        writer.setOnlyAlias(true);
    }

    /**
     * 配置流水明细列头别名。
     * 入账与退款共用字段映射，退款场景额外展示售后单号。
     */
    private void configureFlowHeaders(ExcelWriter writer, boolean refund) {
        writer.addHeaderAlias("createTime", "入账时间");
        writer.addHeaderAlias("orderSn", "订单编号");
        if (refund) {
            writer.addHeaderAlias("refundSn", "售后单号");
        }
        writer.addHeaderAlias("storeName", "店铺名称");
        writer.addHeaderAlias("goodsName", "商品名称");
        writer.addHeaderAlias("num", refund ? "退款量" : "销售量");
        writer.addHeaderAlias("finalPrice", refund ? "退款金额" : "订单金额");
        writer.addHeaderAlias("giftCardSubsidyPrice", "礼品卡补贴");
        writer.addHeaderAlias("settlementBasePrice", "结算基数");
        writer.addHeaderAlias("commissionPrice", "平台分佣");
        writer.addHeaderAlias("siteCouponPrice", "平台优惠券");
        writer.addHeaderAlias("distributionRebate", "分销金额");
        writer.addHeaderAlias("pointSettlementPrice", "积分结算金额");
        writer.addHeaderAlias("kanjiaSettlementPrice", "砍价结算金额");
        writer.addHeaderAlias("paymentName", "支付方式");
        writer.addHeaderAlias("transactionId", "第三方流水号");
        writer.addHeaderAlias("billPrice", refund ? "结算金额" : "应结金额");
    }

    private long countPaymentLogs(Order order, SearchVO searchVO) {
        PageVO pageVO = new PageVO();
        pageVO.setPageNumber(1);
        pageVO.setPageSize(1);
        return orderService.queryPaymentLogs(order, searchVO, pageVO).getTotal();
    }

    /** 构建钱包流水查询条件，与 WalletLogManagerController 列表筛选保持一致 */
    private LambdaQueryWrapper<WalletLog> buildWalletWrapper(DepositQueryVO query) {
        LambdaQueryWrapper<WalletLog> wrapper = Wrappers.lambdaQuery();
        wrapper.eq(CharSequenceUtil.isNotEmpty(query.getMemberId()), WalletLog::getMemberId, query.getMemberId());
        wrapper.like(CharSequenceUtil.isNotEmpty(query.getMemberName()), WalletLog::getMemberName, query.getMemberName());
        if (CharSequenceUtil.isNotEmpty(query.getStartDate()) && CharSequenceUtil.isNotEmpty(query.getEndDate())) {
            wrapper.between(WalletLog::getCreateTime, query.getStartDate(), query.getEndDate());
        } else if (CharSequenceUtil.isNotEmpty(query.getStartDate())) {
            wrapper.ge(WalletLog::getCreateTime, query.getStartDate());
        } else if (CharSequenceUtil.isNotEmpty(query.getEndDate())) {
            wrapper.le(WalletLog::getCreateTime, query.getEndDate());
        }
        wrapper.orderByDesc(WalletLog::getCreateTime);
        return wrapper;
    }

    /** 构建店铺流水查询条件；商家端由 Controller 强制注入 storeId */
    private LambdaQueryWrapper<StoreFlow> buildStoreFlowWrapper(FinanceStoreFlowSearchParams params) {
        LambdaQueryWrapper<StoreFlow> wrapper = Wrappers.lambdaQuery();
        wrapper.eq(CharSequenceUtil.isNotEmpty(params.getStoreId()), StoreFlow::getStoreId, params.getStoreId());
        wrapper.eq(CharSequenceUtil.isNotEmpty(params.getFlowType()), StoreFlow::getFlowType, params.getFlowType());
        if (params.getStartTime() != null && params.getEndTime() != null) {
            wrapper.between(StoreFlow::getCreateTime, params.getStartTime(), params.getEndTime());
        } else if (CharSequenceUtil.isNotEmpty(params.getStartDate()) && CharSequenceUtil.isNotEmpty(params.getEndDate())) {
            wrapper.between(StoreFlow::getCreateTime, params.getStartDate(), params.getEndDate());
        }
        wrapper.orderByDesc(StoreFlow::getCreateTime);
        return wrapper;
    }

    private String formatDate(Date date) {
        return date == null ? "" : DateUtil.format(date, "yyyy-MM-dd");
    }

    private String billStatusText(String status) {
        if (CharSequenceUtil.isEmpty(status)) {
            return "";
        }
        try {
            return BillStatusEnum.valueOf(status).description();
        } catch (Exception e) {
            return status;
        }
    }
}
