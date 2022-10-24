package cn.lili.controller.other;


import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.service.StoreLogisticsService;
import cn.lili.modules.store.entity.dos.StoreLogistics;
import cn.lili.modules.store.entity.dto.StoreLogisticsCustomerDTO;
import cn.lili.modules.system.entity.vo.StoreLogisticsVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Objects;

/**
 * 店铺端,物流公司接口
 *
 * @author Bulbasaur
 * @since 2020/11/22 14:23
 */
@RestController
@Api(tags = "店铺端,物流公司接口")
@RequestMapping("/store/other/logistics")
public class LogisticsStoreController {

    /**
     * 物流公司
     */
    @Autowired
    private StoreLogisticsService storeLogisticsService;

    @ApiOperation(value = "获取商家物流公司列表，如果已选择则checked有值")
    @GetMapping
    public ResultMessage<List<StoreLogisticsVO>> get() {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        //获取已开启的物流公司
        List<StoreLogisticsVO> storeLogistics = storeLogisticsService.getOpenStoreLogistics(storeId);
        //获取未开启的物流公司
        List<StoreLogisticsVO> closeStoreLogistics = storeLogisticsService.getCloseStoreLogistics(storeId);
        storeLogistics.addAll(closeStoreLogistics);
        return ResultUtil.data(storeLogistics);
    }

    @ApiOperation(value = "获取商家已选择物流公司列表")
    @GetMapping("/getChecked")
    public ResultMessage<List<StoreLogisticsVO>> getChecked() {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(storeLogisticsService.getStoreSelectedLogistics(storeId));
    }

    @ApiOperation(value = "选择物流公司")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "logisticsId", value = "物流公司ID", required = true, paramType = "path"),
    })
    @PostMapping("/{logisticsId}")
    public ResultMessage<StoreLogistics> checked(@PathVariable String logisticsId,@RequestBody StoreLogisticsCustomerDTO storeLogisticsCustomerDTO) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(storeLogisticsService.add(logisticsId, storeId,storeLogisticsCustomerDTO));
    }


    @ApiOperation(value = "取消选择物流公司")
    @ApiImplicitParam(name = "id", value = "物流公司ID", required = true, paramType = "path")
    @DeleteMapping(value = "/{id}")
    public ResultMessage<Object> cancel(@PathVariable String id) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        boolean remove = storeLogisticsService.remove(new LambdaQueryWrapper<StoreLogistics>().eq(StoreLogistics::getId, id).eq(StoreLogistics::getStoreId, storeId));
        return ResultUtil.data(remove);
    }

    @ApiOperation(value = "修改电子面单参数")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "logisticsId", value = "物流公司ID", required = true, paramType = "path"),
    })
    @PutMapping("/{logisticsId}/updateStoreLogistics")
    public ResultMessage<StoreLogistics> updateStoreLogistics(@PathVariable String logisticsId,StoreLogisticsCustomerDTO storeLogisticsCustomerDTO){
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(storeLogisticsService.update(logisticsId, storeId,storeLogisticsCustomerDTO));
    }

    @ApiOperation(value = "获取商家已选择物流公司并且使用电子面单列表")
    @GetMapping("/getCheckedFaceSheet")
    public ResultMessage<List<StoreLogisticsVO>> getCheckedFaceSheet() {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(storeLogisticsService.getStoreSelectedLogisticsUseFaceSheet(storeId));
    }

    @ApiOperation(value = "获取店铺-物流公司详细信息")
    @ApiImplicitParam(name = "logisticsId", value = "物流公司ID", required = true, paramType = "path")
    @GetMapping("/{logisticsId}/getStoreLogistics")
    public ResultMessage<StoreLogistics> getStoreLogistics(@PathVariable String logisticsId){
        return ResultUtil.data(storeLogisticsService.getStoreLogisticsInfo(logisticsId));
    }

}
