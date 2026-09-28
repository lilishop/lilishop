package cn.lili.controller.appointment;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.appointment.entity.dto.AppointmentCheckoutDTO;
import cn.lili.modules.appointment.entity.vo.AppointmentBuyerOrderDetailVO;
import cn.lili.modules.appointment.entity.vo.AppointmentGoodsExtVO;
import cn.lili.modules.appointment.entity.vo.AppointmentSlotStockVO;
import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.service.CartService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.*;

import java.util.Date;
import java.util.List;

/**
 * 买家端预约结算辅助接口（移动端）
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@RestController
@Tag(name = "买家端,预约服务")
@RequestMapping("/buyer/appointment")
public class AppointmentBuyerController {

    @Autowired
    private AppointmentService appointmentService;

    @Autowired
    private CartService cartService;

    @Operation(summary = "查询可预约时段")
    @GetMapping("/slots")
    public ResultMessage<List<AppointmentSlotStockVO>> listSlots(@RequestParam String skuId,
                                                                 @RequestParam @DateTimeFormat(pattern = "yyyy-MM-dd") Date slotDate) {
        return ResultUtil.data(appointmentService.listAvailableSlots(skuId, slotDate));
    }

    @Operation(summary = "绑定预约结算上下文（立即购买前调用）")
    @PostMapping("/checkout/bind")
    public ResultMessage<Object> bindCheckout(@Valid @RequestBody AppointmentCheckoutDTO checkout) {
        TradeDTO tradeDTO = cartService.readDTO(CartTypeEnum.BUY_NOW);
        if (tradeDTO.getMemberId() == null && UserContext.getCurrentUser() != null) {
            tradeDTO.setMemberId(UserContext.getCurrentUser().getId());
        }
        appointmentService.bindCheckoutContext(tradeDTO, checkout);
        cartService.resetTradeDTO(tradeDTO);
        return ResultUtil.success();
    }

    @Operation(summary = "预约商品扩展配置（买家下单页）")
    @GetMapping("/goods/{goodsId}/ext")
    public ResultMessage<AppointmentGoodsExtVO> getGoodsExt(@PathVariable String goodsId) {
        return ResultUtil.data(appointmentService.getGoodsExt(goodsId));
    }

    @Operation(summary = "买家预约订单详情（时段/核销码）")
    @GetMapping("/order/{orderSn}")
    public ResultMessage<AppointmentBuyerOrderDetailVO> getBuyerOrderDetail(@PathVariable String orderSn) {
        return ResultUtil.data(appointmentService.getBuyerOrderDetail(
                UserContext.getCurrentUser().getId(), orderSn));
    }

}
