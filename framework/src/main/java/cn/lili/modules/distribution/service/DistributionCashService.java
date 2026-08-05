package cn.lili.modules.distribution.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.distribution.entity.dos.DistributionCash;
import cn.lili.modules.distribution.entity.vos.DistributionCashSearchParams;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;

import jakarta.servlet.http.HttpServletResponse;

/**
 * 分销提现业务层。
 * <p>
 * 负责分销员佣金提现申请的提交、分页查询、导出及平台审核，
 * 并与分销员钱包冻结/解冻逻辑联动。
 * </p>
 *
 * @author pikachu
 * @since 2020-03-14
 */
public interface DistributionCashService extends IService<DistributionCash> {

    /**
     * 分销员提交提现申请。
     *
     * @param applyMoney 申请提现金额
     * @return 是否提交成功
     */
    Boolean cash(Double applyMoney);

    /**
     * 买家端分页查询当前登录分销员的提现记录。
     *
     * @param page 分页参数
     * @return 提现申请分页
     */
    IPage<DistributionCash> getDistributionCash(PageVO page);

    /**
     * 管理端按条件分页查询分销提现申请。
     *
     * @param distributionCashSearchParams 搜索条件（状态、分销员等）
     * @return 提现申请分页
     */
    IPage<DistributionCash> getDistributionCash(DistributionCashSearchParams distributionCashSearchParams);

    /**
     * 按搜索条件导出分销提现记录。
     *
     * @param response                     HTTP 响应
     * @param distributionCashSearchParams 搜索条件
     */
    void queryExport(HttpServletResponse response, DistributionCashSearchParams distributionCashSearchParams);

    /**
     * 审核分销提现申请。
     *
     * @param id     提现申请 ID
     * @param result 审核结果（通过/拒绝）
     * @return 更新后的提现申请
     */
    DistributionCash audit(@PathVariable String id, @RequestParam String result);

}
