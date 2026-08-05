package cn.lili.modules.distribution.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionCash;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.entity.vos.DistributionCashSearchParams;
import cn.lili.modules.distribution.mapper.DistributionCashMapper;
import cn.lili.modules.distribution.service.DistributionCashService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.distribution.service.DistributionWalletAccountService;
import cn.lili.modules.wallet.entity.enums.WithdrawStatusEnum;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.net.URLEncoder;
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

    @Autowired
    private DistributionService distributionService;

    @Autowired
    private DistributionWalletAccountService walletAccountService;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean cash(Double applyMoney) {
        distributionService.checkDistributionSetting();
        Distribution distribution = distributionService.getDistribution();
        // 仅已通过审核的分销员可申请提现：冻结钱包余额并创建提现单
        if (distribution != null && distribution.getDistributionStatus().equals(DistributionStatusEnum.PASS.name())) {
            if (distributionService.isRetreated(distribution.getMemberId())) {
                throw new ServiceException(ResultCode.DISTRIBUTION_NOT_EXIST);
            }
            BigDecimal amount = BigDecimal.valueOf(applyMoney);
            String sn = "D" + SnowFlake.getId();
            // 钱包侧预扣款（幂等键防重复提交）
            walletAccountService.applyWithdraw(distribution.getId(), amount, sn,
                    "withdraw:apply:" + sn, "分销提现申请");
            DistributionCash distributionCash = new DistributionCash(sn, applyMoney, distribution);
            return this.save(distributionCash);
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
        distributionService.checkDistributionSetting();
        DistributionCash distributorCash = this.getById(id);
        if (distributorCash != null) {
            Distribution distribution = distributionService.getById(distributorCash.getDistributionId());
            if (distribution != null) {
                BigDecimal amount = BigDecimal.valueOf(distributorCash.getPrice());
                String idempotentKey = "withdraw:audit:" + distributorCash.getSn() + ":" + result;
                // 通过：确认扣款并记录打款时间；拒绝：解冻退回可提现余额
                if (result.equals(WithdrawStatusEnum.VIA_AUDITING.name())) {
                    distributorCash.setDistributionCashStatus(WithdrawStatusEnum.VIA_AUDITING.name());
                    distributorCash.setPayTime(new Date());
                    walletAccountService.approveWithdraw(distribution.getId(), amount, distributorCash.getSn(),
                            idempotentKey, "分销提现审核通过");
                } else {
                    distributorCash.setDistributionCashStatus(WithdrawStatusEnum.FAIL_AUDITING.name());
                    walletAccountService.rejectWithdraw(distribution.getId(), amount, distributorCash.getSn(),
                            idempotentKey, "分销提现审核拒绝");
                }
                this.updateById(distributorCash);
                return distributorCash;
            }
            throw new ServiceException(ResultCode.DISTRIBUTION_NOT_EXIST);
        }
        throw new ServiceException(ResultCode.DISTRIBUTION_CASH_NOT_EXIST);
    }

    /**
     * 初始化分销提现导出 Excel 数据。
     *
     * @param distributionCashList 导出的数据
     * @return 工作簿
     */
    private XSSFWorkbook initExportData(List<DistributionCash> distributionCashList) {
        XSSFWorkbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet("订单列表");
        Row header = sheet.createRow(0);
        String[] headers = {"编号", "姓名", "身份证号", "结算银行开户行名称", "结算银行开户账号", "结算银行开户支行名称"};
        for (int i = 0; i < headers.length; i++) {
            Cell cell = header.createCell(i);
            cell.setCellValue(headers[i]);
        }
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
