package cn.lili.controller.seller.settings;


import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.store.entity.dto.StoreAfterSaleAddressDTO;
import cn.lili.modules.store.entity.dto.StoreDeliverGoodsAddressDTO;
import cn.lili.modules.store.entity.dto.StoreSettingDTO;
import cn.lili.modules.store.entity.vos.StoreVO;
import cn.lili.modules.store.service.StoreDetailService;
import cn.lili.modules.store.service.StoreService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import jakarta.validation.Valid;

/**
 * 店铺端,店铺设置接口
 *
 * @author Bulbasaur
 * @since 2020/11/22 14:23
 */
@RestController
@Tag(name = "店铺端,店铺设置接口")
@RequestMapping("/store/settings/storeSettings")
public class StoreSettingsController {

    /**
     * 店铺
     */
    @Autowired
    private StoreService storeService;
    /**
     * 店铺详情
     */
    @Autowired
    private StoreDetailService storeDetailService;

    @Operation(description = "获取商家设置")
    @GetMapping
    public ResultMessage<StoreVO> get() {
        //获取当前登录商家内容
        return ResultUtil.data(storeService.getStoreDetail());
    }

    @Operation(description = "修改商家设置")
    @Parameter(name = "storeSettingDTO", description = "商家设置DTO", required = true)
    @PutMapping
    public ResultMessage<Object> edit(@Valid StoreSettingDTO storeSettingDTO) {
        //修改商家设置
        Boolean result = storeDetailService.editStoreSetting(storeSettingDTO);
        return ResultUtil.data(result);
    }

    @Operation(description = "修改商家设置")
    @Parameter(name = "merchantEuid", description = "商家EUID", required = true)
    @PutMapping("/merchantEuid")
    public ResultMessage<Object> edit(String merchantEuid) {
        //修改UDESK设置
        Boolean result = storeDetailService.editMerchantEuid(merchantEuid);
        return ResultUtil.data(result);
    }

    @Operation(description = "修改店铺库存预警数量")
    @Parameter(name = "stockWarning", description = "库存预警数量", required = true)
    @PutMapping("/updateStockWarning")
    public ResultMessage<Object> updateStockWarning(Integer stockWarning) {
        //修改商家设置
        boolean result = storeDetailService.updateStockWarning(stockWarning);
        return ResultUtil.data(result);
    }

    @Operation(description = "获取商家退货收件地址")
    @GetMapping("/storeAfterSaleAddress")
    public ResultMessage<StoreAfterSaleAddressDTO> getStoreAfterSaleAddress() {
        //获取当前登录商家内容
        return ResultUtil.data(storeDetailService.getStoreAfterSaleAddressDTO());
    }

    @Operation(description = "修改商家退货收件地址")
    @Parameter(name = "storeAfterSaleAddressDTO", description = "商家退货收件地址DTO", required = true)
    @PutMapping("/storeAfterSaleAddress")
    public ResultMessage<Object> editStoreAfterSaleAddress(@Valid StoreAfterSaleAddressDTO storeAfterSaleAddressDTO) {
        //修改商家退货收件地址
        boolean result = storeDetailService.editStoreAfterSaleAddressDTO(storeAfterSaleAddressDTO);
        return ResultUtil.data(result);
    }


    @Operation(description = "获取商家发货地址")
    @GetMapping("/storeDeliverGoodsAddress")
    public ResultMessage<StoreDeliverGoodsAddressDTO> getStoreDeliverGoodsAddress(){
        return ResultUtil.data(storeDetailService.getStoreDeliverGoodsAddressDto());
    }

    @Operation(description = "修改商家发货地址")
    @Parameter(name = "storeDeliverGoodsAddressDTO", description = "商家发货地址DTO", required = true)
    @PutMapping("/storeDeliverGoodsAddress")
    public ResultMessage<Object> editStoreDeliverGoodsAddress(@Valid StoreDeliverGoodsAddressDTO storeDeliverGoodsAddressDTO) {
        //修改商家退货收件地址
        boolean result = storeDetailService.editStoreDeliverGoodsAddressDTO(storeDeliverGoodsAddressDTO);
        return ResultUtil.data(result);
    }
}
