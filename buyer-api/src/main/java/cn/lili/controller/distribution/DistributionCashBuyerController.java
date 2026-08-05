package cn.lili.controller.distribution;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionCash;
import cn.lili.modules.distribution.entity.dos.DistributionWalletLog;
import cn.lili.modules.distribution.service.DistributionCashService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.distribution.service.DistributionWalletLogService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;


/**
 * 买家端,分销商品佣金提现接口
 *
 * @author pikachu
 * @since 2020/11/16 10:03 下午
 */
@RestController
@Tag(name = "买家端,分销商品佣金提现接口")
@RequestMapping("/buyer/distribution/cash")
@Validated
public class DistributionCashBuyerController {

    /**
     * 分销佣金
     */
    @Autowired
    private DistributionCashService distributionCashService;
    /**
     * 分销员提现
     */
    @Autowired
    private DistributionCashService distributorCashService;

    @Autowired
    private DistributionService distributionService;

    @Autowired
    private DistributionWalletLogService walletLogService;


    @PreventDuplicateSubmissions
    @Operation(summary = "分销员提现")
    @Parameters({
            @Parameter(name = "price", description = "申请金额", required = true)
    })
    @PostMapping
    public ResultMessage<Object> cash(@Validated @Max(value = 9999, message = "提现金额单次最多允许提现9999元")
                                          @Min(value = 1, message = "提现金额单次最少提现金额为1元")
                                          @NotNull Double price) {
        if (Boolean.TRUE.equals(distributionCashService.cash(price))) {
            return ResultUtil.success();
        }
        throw new ServiceException(ResultCode.ERROR);
    }

    @Operation(summary = "分销员提现历史")
    @GetMapping
    public ResultMessage<IPage<DistributionCash>> casHistory(PageVO page) {
        return ResultUtil.data(distributorCashService.getDistributionCash(page));
    }

    @Operation(summary = "分销钱包流水分页")
    @GetMapping("/walletLog")
    public ResultMessage<IPage<DistributionWalletLog>> walletLog(PageVO page) {
        Distribution distribution = distributionService.getDistribution();
        if (distribution == null || distributionService.isRetreated(distribution.getMemberId())) {
            return ResultUtil.data(null);
        }
        return ResultUtil.data(walletLogService.pageByDistribution(distribution.getId(), null, null, page));
    }

}
