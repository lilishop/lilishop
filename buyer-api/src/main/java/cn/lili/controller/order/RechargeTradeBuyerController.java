package cn.lili.controller.order;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.wallet.entity.dos.Recharge;
import cn.lili.modules.wallet.service.RechargeService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;

/**
 * 买家端,预存款充值记录接口
 *
 * @author paulG
 * @since 2020/11/16 10:07 下午
 */
@RestController
@Api(tags = "买家端,预存款充值记录接口")
@RequestMapping("/buyer/trade/recharge")
public class RechargeTradeBuyerController {

    @Autowired
    private RechargeService rechargeService;

    @PostMapping
    @ApiOperation(value = "创建余额充值订单")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "price", value = "充值金额", required = true, dataType = "double", paramType = "query")
    })
    public ResultMessage<Recharge> create(@Max(value = 10000, message = "充值金额单次最多允许充值10000元") @Min(value = 1, message = "充值金额单次最少充值金额为1元") Double price) {
        Recharge recharge = this.rechargeService.recharge(price);
        return ResultUtil.data(recharge);
    }

}
