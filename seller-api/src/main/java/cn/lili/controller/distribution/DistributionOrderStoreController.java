package cn.lili.controller.distribution;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dos.DistributionOrder;
import cn.lili.modules.distribution.entity.vos.DistributionOrderSearchParams;
import cn.lili.modules.distribution.service.DistributionOrderService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;

/**
 * 店铺端,分销订单接口
 *
 * @author Bulbasaur
 * @since 2020/11/16 10:06 下午
 */
@RestController
@Api(tags = "店铺端,分销订单接口")
@RequestMapping("/store/distribution/order")
public class DistributionOrderStoreController {

    /**
     * 分销订单
     */
    @Autowired
    private DistributionOrderService distributionOrderService;

    @ApiOperation(value = "获取分销订单列表")
    @GetMapping
    public ResultMessage<IPage<DistributionOrder>> distributionOrder(DistributionOrderSearchParams distributionOrderSearchParams) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        //获取当前登录商家账号-查询当前店铺的分销订单
        distributionOrderSearchParams.setStoreId(storeId);
        //查询分销订单列表
        IPage<DistributionOrder> distributionOrderPage = distributionOrderService.getDistributionOrderPage(distributionOrderSearchParams);
        return ResultUtil.data(distributionOrderPage);
    }

}
