package cn.lili.modules.distribution.serviceimpl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionCash;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.entity.vos.DistributionCashSearchParams;
import cn.lili.modules.distribution.mapper.DistributionCashMapper;
import cn.lili.modules.distribution.service.DistributionCashService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.order.order.entity.dto.OrderExportDTO;
import cn.lili.modules.order.order.entity.dto.OrderExportDetailDTO;
import cn.lili.modules.order.order.entity.dto.OrderSearchParams;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.wallet.entity.dto.MemberWalletUpdateDTO;
import cn.lili.modules.wallet.entity.dto.MemberWithdrawalMessage;
import cn.lili.modules.wallet.entity.enums.DepositServiceTypeEnum;
import cn.lili.modules.wallet.entity.enums.MemberWithdrawalDestinationEnum;
import cn.lili.modules.wallet.entity.enums.WithdrawStatusEnum;
import cn.lili.modules.wallet.service.MemberWalletService;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.rocketmq.tags.MemberTagsEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.swagger.annotations.ApiOperation;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;


/**
 * 分销佣金业务层实现
 *
 * @author pikachu
 * @since 2020-03-126 18:04:56
 */
@Service
public class DistributionCashServiceImpl extends ServiceImpl<DistributionCashMapper, DistributionCash> implements DistributionCashService {
    /**
     * 分销员
     */
    @Autowired
    private DistributionService distributionService;
    /**
     * 会员余额
     */
    @Autowired
    private MemberWalletService memberWalletService;
    @Autowired
    private RocketMQTemplate rocketMQTemplate;
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean cash(Double applyMoney) {

        //检查分销功能开关
        distributionService.checkDistributionSetting();

        //获取分销员
        Distribution distribution = distributionService.getDistribution();
        //如果未找到分销员或者分销员状态不是已通过则无法申请提现
        if (distribution != null && distribution.getDistributionStatus().equals(DistributionStatusEnum.PASS.name())) {
            //校验分销佣金是否大于提现金额
            if (distribution.getCanRebate() < applyMoney) {
                throw new ServiceException(ResultCode.WALLET_WITHDRAWAL_INSUFFICIENT);
            }
            //将提现金额存入冻结金额,扣减可提现金额
            distribution.setCanRebate(CurrencyUtil.sub(distribution.getCanRebate(), applyMoney));
            distribution.setCommissionFrozen(CurrencyUtil.add(distribution.getCommissionFrozen(), applyMoney));
            distributionService.updateById(distribution);
            //提现申请记录
            DistributionCash distributionCash = new DistributionCash("D" + SnowFlake.getId(),applyMoney, distribution);
            boolean result = this.save(distributionCash);
            if (result) {
                //发送提现消息
                MemberWithdrawalMessage memberWithdrawalMessage = new MemberWithdrawalMessage();
                memberWithdrawalMessage.setMemberId(distribution.getMemberId());
                memberWithdrawalMessage.setPrice(applyMoney);
                memberWithdrawalMessage.setStatus(WithdrawStatusEnum.APPLY.name());
                String destination = rocketmqCustomProperties.getMemberTopic() + ":" + MemberTagsEnum.MEMBER_WITHDRAWAL.name();
                rocketMQTemplate.asyncSend(destination, memberWithdrawalMessage, RocketmqSendCallbackBuilder.commonCallback());
                return true;
            }
            return false;

        }
        throw new ServiceException(ResultCode.DISTRIBUTION_NOT_EXIST);

    }

    @Override
    public IPage<DistributionCash> getDistributionCash(PageVO page) {
        Distribution distribution = distributionService.getDistribution();
        QueryWrapper<DistributionCash> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("distribution_id", distribution.getId());
        return this.page(PageUtil.initPage(page), queryWrapper);
    }

    @Override
    public IPage<DistributionCash> getDistributionCash(DistributionCashSearchParams distributionCashSearchParams) {

        return this.page(PageUtil.initPage(distributionCashSearchParams), distributionCashSearchParams.queryWrapper());
    }

    @Override
    public void queryExport(HttpServletResponse response, DistributionCashSearchParams distributionCashSearchParams) {
        XSSFWorkbook workbook = initExportData(this.list(distributionCashSearchParams.queryWrapper()));
        try {
            // 设置响应头
            String fileName = URLEncoder.encode("分销提现列表", "UTF-8");
            response.setContentType("application/vnd.ms-excel;charset=UTF-8");
            response.setHeader("Content-Disposition", "attachment;filename=" + fileName + ".xlsx");

            ServletOutputStream out = response.getOutputStream();
            workbook.write(out);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                workbook.close();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public DistributionCash audit(String id, String result) {

        //检查分销功能开关
        distributionService.checkDistributionSetting();

        //获取分销佣金信息
        DistributionCash distributorCash = this.getById(id);
        //只有分销员和分销佣金记录存在的情况才可以审核
        if (distributorCash != null) {
            //获取分销员
            Distribution distribution = distributionService.getById(distributorCash.getDistributionId());
            if (distribution != null && distribution.getDistributionStatus().equals(DistributionStatusEnum.PASS.name())) {
                //审核通过
                if (result.equals(WithdrawStatusEnum.VIA_AUDITING.name())) {
                    //分销记录操作
                    distributorCash.setDistributionCashStatus(WithdrawStatusEnum.VIA_AUDITING.name());
                    distributorCash.setPayTime(new Date());
                } else {
                    //分销员可提现金额退回
                    distribution.setCanRebate(CurrencyUtil.add(distribution.getCanRebate(), distributorCash.getPrice()));
                    distributorCash.setDistributionCashStatus(WithdrawStatusEnum.FAIL_AUDITING.name());
                }
                distribution.setCommissionFrozen(CurrencyUtil.sub(distribution.getCommissionFrozen(), distributorCash.getPrice()));
                //分销员金额相关处理
                distributionService.updateById(distribution);
                //修改分销提现申请
                this.updateById(distributorCash);
                return distributorCash;
            }
            throw new ServiceException(ResultCode.DISTRIBUTION_NOT_EXIST);
        }
        throw new ServiceException(ResultCode.DISTRIBUTION_CASH_NOT_EXIST);

    }

    /**
     * 初始化填充导出数据
     *
     * @param distributionCashList 导出的数据
     * @return 填充导出数据
     */
    private XSSFWorkbook initExportData(List<DistributionCash> distributionCashList) {

        XSSFWorkbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet("订单列表");

        // 创建表头
        Row header = sheet.createRow(0);
        String[] headers = {"编号", "姓名", "身份证号", "结算银行开户行名称", "结算银行开户账号", "结算银行开户支行名称"};

        for (int i = 0; i < headers.length; i++) {
            Cell cell = header.createCell(i);
            cell.setCellValue(headers[i]);
        }
        // 填充数据
        for (int i = 0; i < distributionCashList.size(); i++) {
            DistributionCash distributionCash = distributionCashList.get(i);
            Row row = sheet.createRow(i + 1);
            row.createCell(0).setCellValue(distributionCash.getSn());
            row.createCell(1).setCellValue(distributionCash.getName());
            row.createCell(2).setCellValue(distributionCash.getIdNumber());
            row.createCell(3).setCellValue(distributionCash.getSettlementBankAccountName());
            row.createCell(4).setCellValue(distributionCash.getSettlementBankAccountNum());
            row.createCell(5).setCellValue(distributionCash.getSettlementBankBranchName());
        }
        return workbook;
    }

}