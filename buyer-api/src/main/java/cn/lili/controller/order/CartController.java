package cn.lili.controller.order;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.vo.TradeParams;
import cn.lili.modules.order.cart.service.CartService;
import cn.lili.modules.order.order.entity.vo.ReceiptVO;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;

/**
 * 买家端，购物车接口
 *
 * @author Chopper
 * @since 2020/11/16 10:04 下午
 */
@Slf4j
@RestController
@Tag(name = "买家端，购物车接口")
@RequestMapping("/buyer/trade/carts")
public class CartController {

    /**
     * 购物车
     */
    @Autowired
    private CartService cartService;


    @Operation(summary = "向购物车中添加一个产品")
    @Parameter(name = "skuId", description = "产品ID", required = true)
    @Parameter(name = "num", description = "此产品的购买数量", required = true)
    @Parameter(name = "cartType", description = "购物车类型，默认加入购物车")
    @PostMapping
    @PreventDuplicateSubmissions
    public ResultMessage<Object> add(@NotNull(message = "产品id不能为空") String skuId,
                                     @NotNull(message = "购买数量不能为空") @Min(value = 1, message = "加入购物车数量必须大于0") Integer num,
                                     String cartType) {
        try {
            //读取选中的列表
            cartService.add(skuId, num, cartType, false);
            return ResultUtil.success();
        } catch (ServiceException se) {
            log.info(se.getMsg(), se);
            throw se;
        } catch (Exception e) {
            log.error(ResultCode.CART_ERROR.message(), e);
            throw new ServiceException(ResultCode.CART_ERROR);
        }
    }


    @Operation(summary = "获取购物车页面购物车详情")
    @GetMapping("/all")
    public ResultMessage<TradeDTO> cartAll() {
        return ResultUtil.data(this.cartService.getAllTradeDTO());
    }

    @Operation(summary = "获取购物车数量")   
    @GetMapping("/count")
    public ResultMessage<Long> cartCount(@RequestParam(required = false) Boolean checked) {
        return ResultUtil.data(this.cartService.getCartNum(checked));
    }

    @Operation(summary = "获取购物车可用优惠券数量")
    @Parameter(name = "way", description = "购物车购买：CART/立即购买：BUY_NOW/拼团购买：PINTUAN / 积分购买：POINT ", required = true)
    @GetMapping("/coupon/num")
    public ResultMessage<Long> cartCouponNum(String way) {
        return ResultUtil.data(this.cartService.getCanUseCoupon(CartTypeEnum.valueOf(way)));
    }

    @Operation(summary = "更新购物车中单个产品数量")
    @Parameter(name = "skuId", description = "产品id数组", required = true)
    @Parameter(name = "num", description = "产品数量", required = true)
    @PostMapping("/sku/num/{skuId}")
    public ResultMessage<Object> update(@NotNull(message = "产品id不能为空") @PathVariable(name = "skuId") String skuId,
                                        Integer num) {
        cartService.add(skuId, num, CartTypeEnum.CART.name(), true);
        return ResultUtil.success();
    }


    @Operation(summary = "更新购物车中单个产品")
    @Parameter(name = "skuId", description = "产品id数组", required = true)
    @Parameter(name = "checked", description = "是否选中", required = true)
    @PostMapping("/sku/checked/{skuId}")
    public ResultMessage<Object> updateChecked(@NotNull(message = "产品id不能为空") @PathVariable(name = "skuId") String skuId,
                                               boolean checked) {
        cartService.checked(skuId, checked);
        return ResultUtil.success();
    }


    @Operation(summary = "购物车选中设置")
    @Parameter(name = "checked", description = "是否选中", required = true)
    @PostMapping("/sku/checked")
    public ResultMessage<Object> updateAll(boolean checked) {
        cartService.checkedAll(checked);
        return ResultUtil.success();
    }


    @Operation(summary = "批量设置某商家的商品为选中或不选中")
    @Parameter(name = "storeId", description = "卖家id", required = true)
    @Parameter(name = "checked", description = "是否选中", required = true)
    @PostMapping("/store/{storeId}")
    public ResultMessage<Object> updateStoreAll(@NotNull(message = "卖家id不能为空") @PathVariable(name = "storeId") String storeId, boolean checked) {
        cartService.checkedStore(storeId, checked);
        return ResultUtil.success();
    }

    @Operation(summary = "清空购物车")
    @DeleteMapping()
    public ResultMessage<Object> clean() {
        cartService.clean();
        return ResultUtil.success();
    }


    @Operation(summary = "删除购物车中的一个或多个产品")
    @Parameter(name = "skuIds", description = "产品id", required = true)
    @DeleteMapping("/sku/remove")
    public ResultMessage<Object> delete(String[] skuIds) {
        cartService.delete(skuIds);
        return ResultUtil.success();
    }

    @Operation(summary = "获取结算页面购物车详情")
    @Parameter(name = "way", description = "购物车购买：CART/立即购买：BUY_NOW/拼团购买：PINTUAN / 积分购买：POINT", required = true)
    @GetMapping("/checked")
    public ResultMessage<TradeDTO> cartChecked(@NotNull(message = "读取选中列表") String way) {
        try {
            //读取选中的列表
            return ResultUtil.data(this.cartService.getCheckedTradeDTO(CartTypeEnum.valueOf(way)));
        } catch (ServiceException se) {
            log.error(se.getMsg(), se);
            throw se;
        } catch (Exception e) {
            log.error(ResultCode.CART_ERROR.message(), e);
            throw new ServiceException(ResultCode.CART_ERROR);
        }
    }

    @Operation(summary = "选择收货地址")
    @Parameter(name = "shippingAddressId", description = "收货地址id ", required = true)
    @Parameter(name = "way", description = "购物车类型 ", required = true)
    @GetMapping("/shippingAddress")
    public ResultMessage<Object> shippingAddress(@NotNull(message = "收货地址ID不能为空") String shippingAddressId,
                                                 String way) {
        try {
            cartService.shippingAddress(shippingAddressId, way);
            return ResultUtil.success();
        } catch (ServiceException se) {
            log.error(ResultCode.SHIPPING_NOT_APPLY.message(), se);
            throw new ServiceException(ResultCode.SHIPPING_NOT_APPLY);
        } catch (Exception e) {
            log.error(ResultCode.CART_ERROR.message(), e);
            throw new ServiceException(ResultCode.CART_ERROR);
        }
    }

    @Operation(summary = "选择自提地址")
    @Parameter(name = "storeAddressId", description = "自提地址id ", required = true)
    @Parameter(name = "way", description = "购物车类型 ", required = true)
    @GetMapping("/storeAddress")
    public ResultMessage<Object> shippingSelfPickAddress(@NotNull(message = "自提地址ID不能为空") String storeAddressId,
                                                 String way) {
        try {
            cartService.shippingSelfAddress(storeAddressId, way);
            return ResultUtil.success();
        } catch (ServiceException se) {
            log.error(ResultCode.SHIPPING_NOT_APPLY.message(), se);
            throw new ServiceException(ResultCode.SHIPPING_NOT_APPLY);
        } catch (Exception e) {
            log.error(ResultCode.CART_ERROR.message(), e);
            throw new ServiceException(ResultCode.CART_ERROR);
        }
    }

    @Operation(summary = "选择配送方式")
    @Parameter(name = "shippingMethod", description = "配送方式：SELF_PICK_UP(自提)," +
                    "LOCAL_TOWN_DELIVERY(同城配送)," +
                    "LOGISTICS(物流) ", required = true)
    @Parameter(name = "way", description = "购物车类型 ", required = true)
    @PutMapping("/shippingMethod")
    public ResultMessage<Object> shippingMethod(@NotNull(message = "配送方式不能为空") String shippingMethod,
                                                String way) {
        try {
            cartService.shippingMethod( shippingMethod, way);
            return ResultUtil.success();
        } catch (ServiceException se) {
            log.error(se.getMsg(), se);
            throw se;
        } catch (Exception e) {
            log.error(ResultCode.CART_ERROR.message(), e);
            throw new ServiceException(ResultCode.CART_ERROR);
        }
    }

    @Operation(summary = "获取用户可选择的物流方式")
    @Parameter(name = "way", description = "购物车类型 ", required = true)
    @GetMapping("/shippingMethodList")
    public ResultMessage<Object> shippingMethodList(String way) {
        try {
            return ResultUtil.data(cartService.shippingMethodList(way));
        }
        catch (Exception e) {
            e.printStackTrace();
            return ResultUtil.error(ResultCode.ERROR);
        }
    }

    @Operation(summary = "选择发票")
    @Parameter(name = "way", description = "购物车购买：CART/立即购买：BUY_NOW/拼团购买：PINTUAN / 积分购买：POINT ", required = true)
    @Parameter(name = "receiptVO", description = "发票信息 ", required = true)
    @GetMapping("/select/receipt")
    public ResultMessage<Object> selectReceipt(String way, ReceiptVO receiptVO) {
        this.cartService.shippingReceipt(receiptVO, way);
        return ResultUtil.success();
    }

    @Operation(summary = "选择优惠券")
    @Parameter(name = "way", description = "购物车购买：CART/立即购买：BUY_NOW/拼团购买：PINTUAN / 积分购买：POINT ", required = true)
    @Parameter(name = "memberCouponId", description = "优惠券id ", required = true)
    @Parameter(name = "used", description = "使用true 弃用false ", required = true)
    @GetMapping("/select/coupon")
    public ResultMessage<Object> selectCoupon(String way, @NotNull(message = "优惠券id不能为空") String memberCouponId, boolean used) {
        this.cartService.selectCoupon(memberCouponId, way, used);
        return ResultUtil.success();
    }

    @Operation(summary = "选择礼品卡")
    @Parameter(name = "way", description = "购物车购买：CART/立即购买：BUY_NOW/拼团购买：PINTUAN / 积分购买：POINT ", required = true)
    @Parameter(name = "credentialId", description = "礼品卡凭证id（used=false且不传则清空全部）")
    @Parameter(name = "used", description = "使用true 弃用false ", required = true)
    @GetMapping("/select/giftCard")
    public ResultMessage<Object> selectGiftCard(String way, String credentialId, boolean used) {
        this.cartService.selectGiftCard(credentialId, way, used);
        return ResultUtil.success();
    }


    @PreventDuplicateSubmissions
    @Operation(summary = "创建交易")
    @Parameter(name = "tradeParams", description = "交易参数 ", required = true)
    @PostMapping(path = "/create/trade", consumes = "application/json", produces = "application/json")
    public ResultMessage<Object> crateTrade(@RequestBody TradeParams tradeParams) {
        try {
            //读取选中的列表
            return ResultUtil.data(this.cartService.createTrade(tradeParams));
        } catch (ServiceException se) {
            log.info(se.getMsg(), se);
            throw se;
        } catch (Exception e) {
            log.error(ResultCode.ORDER_ERROR.message(), e);
            throw e;
        }
    }
}
