package cn.lili.controller.store.seat;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.im.entity.dos.SeatSetting;
import cn.lili.modules.im.service.SeatSettingService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 店铺端,坐席设置管理接口
 *
 * @author pikachu
 * @since 2020-02-18 15:18:56
 */
@RestController
@Tag(name = "店铺端,坐席设置")
@RequestMapping("/store/seat/setting")
@Transactional(rollbackFor = Exception.class)
public class SeatSettingStoreController {

    @Autowired
    private SeatSettingService seatSettingService;

    @Operation(description = "查询坐席设置")
    @GetMapping
    public ResultMessage<SeatSetting> getSetting() {
        return ResultUtil.data(seatSettingService.getSetting(UserContext.getCurrentUser().getTenantId()));
    }

    @Operation(description = "更新坐席设置")
    @Parameter(name = "seatSetting", description = "坐席设置")
    @PutMapping
    public void update(SeatSetting seatSetting) {
        seatSetting.setTenantId(UserContext.getCurrentUser().getTenantId());
        seatSettingService.updateByStore(seatSetting);
    }
}
