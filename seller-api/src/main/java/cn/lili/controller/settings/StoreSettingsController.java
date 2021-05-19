package cn.lili.controller.settings;


import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.store.entity.dos.StoreDetail;
import cn.lili.modules.store.entity.dto.StoreAfterSaleAddressDTO;
import cn.lili.modules.store.entity.dto.StoreSettingDTO;
import cn.lili.modules.store.entity.vos.StoreVO;
import cn.lili.modules.store.service.StoreDetailService;
import cn.lili.modules.store.service.StoreService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

/**
 * 店铺端,店铺设置接口
 *
 * @author Bulbasaur
 * @date: 2020/11/22 14:23
 */
@RestController
@Api(tags = "店铺端,店铺设置接口")
@RequestMapping("/store/settings")
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

    @ApiOperation(value = "获取商家设置")
    @GetMapping
    public ResultMessage<StoreVO> get() {
        //获取当前登录商家内容
        return ResultUtil.data(storeService.getStoreDetail());
    }

    @ApiOperation(value = "修改商家设置")
    @PutMapping
    public ResultMessage<StoreDetail> edit(@Valid StoreSettingDTO storeSettingDTO) {
        //修改商家设置
        storeDetailService.editStoreSetting(storeSettingDTO);
        return ResultUtil.success();
    }

    @ApiOperation(value = "修改店铺库存预警数量")
    @ApiImplicitParam(name = "stockWarning", value = "库存预警数量", required = true, dataType = "Integer", paramType = "query")
    @PutMapping("/updateStockWarning")
    public ResultMessage<StoreDetail> updateStockWarning(Integer stockWarning) {
        //修改商家设置
        storeDetailService.updateStockWarning(stockWarning);
        return ResultUtil.success();
    }

    @ApiOperation(value = "获取商家退货收件地址")
    @GetMapping("/storeAfterSaleAddress")
    public ResultMessage<StoreAfterSaleAddressDTO> getStoreAfterSaleAddress() {
        //获取当前登录商家内容
        return ResultUtil.data(storeDetailService.getStoreAfterSaleAddressDTO());
    }

    @ApiOperation(value = "修改商家退货收件地址")
    @PutMapping("/storeAfterSaleAddress")
    public ResultMessage<StoreDetail> editStoreAfterSaleAddress(@Valid StoreAfterSaleAddressDTO storeAfterSaleAddressDTO) {
        //修改商家退货收件地址
        storeDetailService.editStoreAfterSaleAddressDTO(storeAfterSaleAddressDTO);
        return ResultUtil.success();
    }
}
