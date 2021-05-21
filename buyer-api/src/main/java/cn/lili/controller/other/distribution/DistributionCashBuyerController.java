package cn.lili.controller.other.distribution;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dos.DistributionCash;
import cn.lili.modules.distribution.service.DistributionCashService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import springfox.documentation.annotations.ApiIgnore;

import javax.validation.constraints.NotNull;


/**
 * 买家端,分销商品佣金提现接口
 *
 * @author pikachu
 * @date: 2020/11/16 10:03 下午
 */
@RestController
@Api(tags = "买家端,分销商品佣金提现接口")
@RequestMapping("/buyer/distribution/cash")
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


    @ApiOperation(value = "分销员提现")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "price", value = "申请金额", required = true, paramType = "query", dataType = "double")
    })
    @PostMapping
    public ResultMessage<Object> cash(@NotNull @ApiIgnore Double price) {
        if(distributionCashService.cash(price)){
            return ResultUtil.success();
        }
        throw new ServiceException(ResultCode.ERROR);
    }

    @ApiOperation(value = "分销员提现历史")
    @GetMapping
    public ResultMessage<IPage<DistributionCash>> casHistory(PageVO page) {
        return ResultUtil.data(distributorCashService.getDistributionCash(page));
    }


}
