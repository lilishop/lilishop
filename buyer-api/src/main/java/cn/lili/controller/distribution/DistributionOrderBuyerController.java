package cn.lili.controller.distribution;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dto.DistributionBuyerOrderQueryDTO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerOrderDetailVO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerOrderPageVO;
import cn.lili.modules.distribution.service.DistributionBuyerOrderService;
import cn.lili.modules.distribution.service.DistributionService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;


/**
 * 买家端,分销订单接口
 *
 * @author pikachu
 * @since 2020/11/16 10:03 下午
 */
@RestController
@Tag(name = "买家端,分销订单接口")
@RequestMapping("/buyer/distribution/order")
public class DistributionOrderBuyerController {

    @Autowired
    private DistributionBuyerOrderService buyerOrderService;

    @Autowired
    private DistributionService distributionService;

    @Operation(summary = "推广/邀请订单分页列表")
    @GetMapping("/orders")
    public ResultMessage<DistributionBuyerOrderPageVO> buyerOrders(DistributionBuyerOrderQueryDTO query) {
        Distribution distribution = getActiveDistribution();
        if (distribution == null) {
            return ResultUtil.data(new DistributionBuyerOrderPageVO());
        }
        return ResultUtil.data(buyerOrderService.pageOrders(distribution.getId(), query));
    }

    @Operation(summary = "推广/邀请订单详情")
    @GetMapping("/orders/detail")
    public ResultMessage<DistributionBuyerOrderDetailVO> buyerOrderDetail(@RequestParam String orderItemSn,
                                                                          @RequestParam(defaultValue = "PROMOTION") String orderType) {
        Distribution distribution = getActiveDistribution();
        if (distribution == null) {
            return ResultUtil.error(ResultCode.ERROR);
        }
        return ResultUtil.data(buyerOrderService.getOrderDetail(distribution.getId(), orderItemSn, orderType));
    }

    /**
     * 获取当前有效分销员，未开通或已清退时返回 null。
     */
    private Distribution getActiveDistribution() {
        distributionService.checkDistributionSetting();
        Distribution distribution = distributionService.getDistribution();
        if (distribution == null || distributionService.isRetreated(distribution.getMemberId())) {
            return null;
        }
        return distribution;
    }

}
