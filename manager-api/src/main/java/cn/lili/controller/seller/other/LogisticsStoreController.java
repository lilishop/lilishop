package cn.lili.controller.seller.other;


import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.service.StoreLogisticsService;
import cn.lili.modules.store.entity.dos.StoreLogistics;
import cn.lili.modules.store.entity.dto.StoreLogisticsCustomerDTO;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.LogisticsSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.entity.vo.StoreLogisticsVO;
import cn.lili.modules.system.service.SettingService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
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
@Tag(name = "店铺端,物流公司接口")
@RequestMapping("/store/other/logistics")
public class LogisticsStoreController {

    /**
     * 物流公司
     */
    @Autowired
    private StoreLogisticsService storeLogisticsService;

    @Autowired
    private SettingService settingService;

    @Operation(description = "获取商家物流公司列表，如果已选择则checked有值")
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

    @Operation(description = "获取商家已选择物流公司列表")
    @GetMapping("/getChecked")
    public ResultMessage<List<StoreLogisticsVO>> getChecked() {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(storeLogisticsService.getStoreSelectedLogistics(storeId));
    }

    @Operation(description = "选择物流公司")
    @Parameter(name = "logisticsId", description = "物流公司ID", required = true)
    @PostMapping("/{logisticsId}")
    public ResultMessage<StoreLogistics> checked(@PathVariable String logisticsId,@RequestBody StoreLogisticsCustomerDTO storeLogisticsCustomerDTO) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(storeLogisticsService.add(logisticsId, storeId,storeLogisticsCustomerDTO));
    }


    @Operation(description = "取消选择物流公司")
    @Parameter(name = "id", description = "物流公司ID", required = true)
    @DeleteMapping("/{id}")
    public ResultMessage<Object> cancel(@PathVariable String id) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        boolean remove = storeLogisticsService.removeStoreLogistics(id, storeId);
        return ResultUtil.data(remove);
    }

    @Operation(description = "修改电子面单参数")
    @Parameter(name = "logisticsId", description = "物流公司ID", required = true)
    @PutMapping("/{logisticsId}/updateStoreLogistics")
    public ResultMessage<StoreLogistics> updateStoreLogistics(@PathVariable String logisticsId,StoreLogisticsCustomerDTO storeLogisticsCustomerDTO){
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(storeLogisticsService.update(logisticsId, storeId,storeLogisticsCustomerDTO));
    }

    @Operation(description = "获取商家已选择物流公司并且使用电子面单列表")
    @GetMapping("/getCheckedFaceSheet")
    public ResultMessage<List<StoreLogisticsVO>> getCheckedFaceSheet() {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(storeLogisticsService.getStoreSelectedLogisticsUseFaceSheet(storeId));
    }

    @Operation(description = "获取店铺-物流公司详细信息")
    @Parameter(name = "logisticsId", description = "物流公司ID", required = true)
    @GetMapping("/{logisticsId}/getStoreLogistics")
    public ResultMessage<StoreLogistics> getStoreLogistics(@PathVariable String logisticsId){
        return ResultUtil.data(storeLogisticsService.getStoreLogisticsInfo(logisticsId));
    }

    @Operation(description = "获取IM接口前缀")
    @GetMapping("/setting")
    public ResultMessage<String> getUrl() {
        String logisticsType;
        try {
            Setting logisticsSettingVal = settingService.get(SettingEnum.LOGISTICS_SETTING.name());
            LogisticsSetting logisticsSetting = JSONUtil.toBean(logisticsSettingVal.getSettingValue(), LogisticsSetting.class);
            logisticsType = logisticsSetting.getType();
        } catch (Exception e) {
            throw new ServiceException(ResultCode.ORDER_LOGISTICS_ERROR);
        }
        return ResultUtil.data(logisticsType);
    }

}
