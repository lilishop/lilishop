package cn.lili.controller.other;


import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.service.StoreLogisticsService;
import cn.lili.modules.store.entity.dos.StoreLogistics;
import cn.lili.modules.system.entity.vo.StoreLogisticsVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
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
        return ResultUtil.data(storeLogisticsService.getStoreLogistics(storeId));
    }

    @ApiOperation(value = "获取商家已选择物流公司列表")
    @GetMapping("/getChecked")
    public ResultMessage<List<StoreLogisticsVO>> getChecked() {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(storeLogisticsService.getStoreSelectedLogistics(storeId));
    }

    @ApiOperation(value = "选择物流公司")
    @ApiImplicitParam(name = "logisticsId", value = "物流公司ID", required = true, paramType = "path")
    @PostMapping("/{logisticsId}")
    public ResultMessage<StoreLogistics> checked(@PathVariable String logisticsId) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(storeLogisticsService.add(logisticsId, storeId));
    }


    @ApiOperation(value = "取消选择物流公司")
    @ApiImplicitParam(name = "id", value = "物流公司ID", required = true, paramType = "path")
    @DeleteMapping(value = "/{id}")
    public ResultMessage<Object> cancel(@PathVariable String id) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        boolean remove = storeLogisticsService.remove(new LambdaQueryWrapper<StoreLogistics>().eq(StoreLogistics::getId, id).eq(StoreLogistics::getStoreId, storeId));
        return ResultUtil.data(remove);
    }

}
