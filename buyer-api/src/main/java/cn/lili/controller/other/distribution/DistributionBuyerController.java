package cn.lili.controller.other.distribution;

import cn.lili.common.security.context.UserContext;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionOrder;
import cn.lili.modules.distribution.entity.vos.DistributionOrderSearchParams;
import cn.lili.modules.distribution.service.DistributionOrderService;
import cn.lili.modules.distribution.service.DistributionService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;


/**
 * 买家端,分销员接口
 *
 * @author pikachu
 * @date: 2020/11/16 10:03 下午
 */
@RestController
@Api(tags = "买家端,分销员接口")
@RequestMapping("/buyer/distribution")
public class DistributionBuyerController {

    /**
     * 分销员
     */
    @Autowired
    private DistributionService distributionService;
    /**
     * 分销员订单
     */
    @Autowired
    private DistributionOrderService distributionOrderService;

    //申请分销员
    @ApiOperation(value = "申请分销员")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "name", value = "姓名", required = true, paramType = "query", dataType = "String"),
            @ApiImplicitParam(name = "idNumber", value = "身份证号", required = true, paramType = "query", dataType = "String")
    })
    @PostMapping
    public ResultMessage<Object> applyDistribution(@RequestParam String name, @RequestParam String idNumber) {
        return ResultUtil.data(distributionService.applyDistribution(name, idNumber));
    }

    @ApiOperation(value = "获取分销员分页订单列表")
    @GetMapping("/distributionOrder")
    public ResultMessage<IPage<DistributionOrder>> distributionOrderPage(DistributionOrderSearchParams distributionOrderSearchParams) {
        distributionOrderSearchParams.setDistributionId(UserContext.getCurrentUser().getId());
        return ResultUtil.data(distributionOrderService.getDistributionOrderPage(distributionOrderSearchParams));
    }

    @ApiOperation(value = "获取当前会员的分销员信息", notes = "可根据分销员信息查询待提现金额以及冻结金额等信息")
    @GetMapping
    public ResultMessage<Distribution> getDistribution() {
        //检查分销开关
        distributionService.checkDistributionSetting();

        return ResultUtil.data(distributionService.getDistribution());
    }
}
