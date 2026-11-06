package cn.lili.controller.promotion;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.promotion.entity.dto.PickupCardRedeemDTO;
import cn.lili.modules.promotion.entity.vos.PickupCardDetailVO;
import cn.lili.modules.promotion.entity.vos.PickupCardPreviewVO;
import cn.lili.modules.promotion.service.PickupCardService;
import io.swagger.v3.oas.annotations.Operation;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 买家提货卡选品、试算和兑换接口，会员身份从登录态读取。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
@RestController
@RequestMapping("/buyer/promotion/pickupCard")
public class PickupCardBuyerController {
    @Autowired
    private PickupCardService pickupCardService;

    /**
     * 查询当前会员提货卡详情及可兑换商品，不返回卡密。
     */
    @Operation(summary = "提货卡兑换详情")
    @GetMapping("/{id}")
    public ResultMessage<PickupCardDetailVO> detail(@PathVariable String id) {
        return ResultUtil.data(pickupCardService.detail(memberId(), id));
    }

    /**
     * 校验商品及地址，返回免邮的零现金试算，不消耗权益。
     */
    @Operation(summary = "提货卡兑换试算")
    @PostMapping("/preview")
    public ResultMessage<PickupCardPreviewVO> preview(@Valid @RequestBody PickupCardRedeemDTO dto) {
        TradeDTO trade = pickupCardService.preview(memberId(), dto);
        PickupCardPreviewVO result = new PickupCardPreviewVO();
        result.setSkuCount(trade.getSkuList().size());
        result.setCashPayAmount(java.math.BigDecimal.ZERO.setScale(2));
        result.setFreightAmount(java.math.BigDecimal.ZERO.setScale(2));
        return ResultUtil.data(result);
    }

    /**
     * 原子兑换并创建已支付订单，重复请求由卡行锁及原交易号保证幂等。
     */
    @Operation(summary = "提货卡一次兑换")
    @PostMapping("/redeem")
    public ResultMessage<String> redeem(@Valid @RequestBody PickupCardRedeemDTO dto) {
        return ResultUtil.data(pickupCardService.redeem(memberId(), dto).getSn());
    }

    /**
     * 从认证上下文读取会员 ID，拒绝未登录访问。
     */
    private String memberId() {
        if (UserContext.getCurrentUser() == null) throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        return UserContext.getCurrentUser().getId();
    }
}
