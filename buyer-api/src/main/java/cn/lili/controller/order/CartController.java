package cn.lili.controller.order;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.vo.TradeParams;
import cn.lili.modules.order.cart.service.CartService;
import cn.lili.modules.order.order.entity.vo.ReceiptVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;

/**
 * 买家端，购物车接口
 *
 * @author Chopper
 * @since 2020/11/16 10:04 下午
 */
@Slf4j
@RestController
@Api(tags = "买家端，购物车接口")
@RequestMapping("/buyer/trade/carts")
public class CartController {

    /**
     * 购物车
     */
    @Autowired
    private CartService cartService;


    @ApiOperation(value = "向购物车中添加一个产品")
    @PostMapping
    @ApiImplicitParams({
            @ApiImplicitParam(name = "skuId", value = "产品ID", required = true, dataType = "Long", paramType = "query"),
            @ApiImplicitParam(name = "num", value = "此产品的购买数量", required = true, dataType = "int", paramType = "query"),
            @ApiImplicitParam(name = "cartType", value = "购物车类型，默认加入购物车", paramType = "query")
    })
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


    @ApiOperation(value = "获取购物车页面购物车详情")
    @GetMapping("/all")
    public ResultMessage<TradeDTO> cartAll() {
        return ResultUtil.data(this.cartService.getAllTradeDTO());
    }

    @ApiOperation(value = "获取购物车数量")
    @GetMapping("/count")
    public ResultMessage<Long> cartCount(@RequestParam(required = false) Boolean checked) {
        return ResultUtil.data(this.cartService.getCartNum(checked));
    }

    @ApiOperation(value = "获取购物车可用优惠券数量")
    @GetMapping("/coupon/num")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "way", value = "购物车购买：CART/立即购买：BUY_NOW/拼团购买：PINTUAN / 积分购买：POINT ", required = true, paramType = "query")
    })
    public ResultMessage<Long> cartCouponNum(String way) {
        return ResultUtil.data(this.cartService.getCanUseCoupon(CartTypeEnum.valueOf(way)));
    }

    @ApiOperation(value = "更新购物车中单个产品数量", notes = "更新购物车中的多个产品的数量或选中状态")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "skuId", value = "产品id数组", required = true, dataType = "Long", paramType = "path"),
            @ApiImplicitParam(name = "num", value = "产品数量", dataType = "int", paramType = "query"),
    })
    @PostMapping(value = "/sku/num/{skuId}")
    public ResultMessage<Object> update(@NotNull(message = "产品id不能为空") @PathVariable(name = "skuId") String skuId,
                                        Integer num) {
        cartService.add(skuId, num, CartTypeEnum.CART.name(), true);
        return ResultUtil.success();
    }


    @ApiOperation(value = "更新购物车中单个产品", notes = "更新购物车中的多个产品的数量或选中状态")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "skuId", value = "产品id数组", required = true, dataType = "Long", paramType = "path")
    })
    @PostMapping(value = "/sku/checked/{skuId}")
    public ResultMessage<Object> updateChecked(@NotNull(message = "产品id不能为空") @PathVariable(name = "skuId") String skuId,
                                               boolean checked) {
        cartService.checked(skuId, checked);
        return ResultUtil.success();
    }


    @ApiOperation(value = "购物车选中设置")
    @PostMapping(value = "/sku/checked", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResultMessage<Object> updateAll(boolean checked) {
        cartService.checkedAll(checked);
        return ResultUtil.success();
    }


    @ApiOperation(value = "批量设置某商家的商品为选中或不选中")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storeId", value = "卖家id", required = true, dataType = "Long", paramType = "path"),
            @ApiImplicitParam(name = "checked", value = "是否选中", required = true, dataType = "int", paramType = "query", allowableValues = "0,1")
    })
    @ResponseBody
    @PostMapping(value = "/store/{storeId}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResultMessage<Object> updateStoreAll(@NotNull(message = "卖家id不能为空") @PathVariable(name = "storeId") String storeId, boolean checked) {
        cartService.checkedStore(storeId, checked);
        return ResultUtil.success();
    }


    @ApiOperation(value = "清空购物车")
    @DeleteMapping()
    public ResultMessage<Object> clean() {
        cartService.clean();
        return ResultUtil.success();
    }


    @ApiOperation(value = "删除购物车中的一个或多个产品")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "skuIds", value = "产品id", required = true, dataType = "Long", paramType = "path", allowMultiple = true)
    })
    @DeleteMapping(value = "/sku/remove")
    public ResultMessage<Object> delete(String[] skuIds) {
        cartService.delete(skuIds);
        return ResultUtil.success();
    }


    @ApiOperation(value = "获取结算页面购物车详情")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "way", value = "购物车购买：CART/立即购买：BUY_NOW/拼团购买：PINTUAN / 积分购买：POINT ", required = true, paramType = "query")
    })
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

    @ApiOperation(value = "选择收货地址")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "shippingAddressId", value = "收货地址id ", required = true, paramType = "query"),
            @ApiImplicitParam(name = "way", value = "购物车类型 ", paramType = "query")
    })
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

    @ApiOperation(value = "选择配送方式")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "shippingMethod", value = "配送方式：SELF_PICK_UP(自提)," +
                    "LOCAL_TOWN_DELIVERY(同城配送)," +
                    "LOGISTICS(物流) ", required = true, paramType = "query"),
            @ApiImplicitParam(name = "selleId", value = "店铺id", paramType = "query"),
            @ApiImplicitParam(name = "way", value = "购物车类型 ", paramType = "query")
    })
    @GetMapping("/shippingMethod")
    public ResultMessage<Object> shippingMethod(@NotNull(message = "配送方式不能为空") String shippingMethod,
                                                String selleId,
                                                String way) {
        try {
            cartService.shippingMethod(selleId, shippingMethod, way);
            return ResultUtil.success();
        } catch (ServiceException se) {
            log.error(se.getMsg(), se);
            throw se;
        } catch (Exception e) {
            log.error(ResultCode.CART_ERROR.message(), e);
            throw new ServiceException(ResultCode.CART_ERROR);
        }
    }

    @ApiOperation(value = "选择发票")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "way", value = "购物车购买：CART/立即购买：BUY_NOW/拼团购买：PINTUAN / 积分购买：POINT ", required = true, paramType = "query"),
    })
    @GetMapping("/select/receipt")
    public ResultMessage<Object> selectReceipt(String way, ReceiptVO receiptVO) {
        this.cartService.shippingReceipt(receiptVO, way);
        return ResultUtil.success();
    }

    @ApiOperation(value = "选择优惠券")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "way", value = "购物车购买：CART/立即购买：BUY_NOW/拼团购买：PINTUAN / 积分购买：POINT ", required = true, paramType = "query"),
            @ApiImplicitParam(name = "memberCouponId", value = "优惠券id ", required = true, paramType = "query"),
            @ApiImplicitParam(name = "used", value = "使用true 弃用false ", required = true, paramType = "query")
    })
    @GetMapping("/select/coupon")
    public ResultMessage<Object> selectCoupon(String way, @NotNull(message = "优惠券id不能为空") String memberCouponId, boolean used) {
        this.cartService.selectCoupon(memberCouponId, way, used);
        return ResultUtil.success();
    }


    @ApiOperation(value = "创建交易")
    @PostMapping(value = "/create/trade", consumes = "application/json", produces = "application/json")
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
