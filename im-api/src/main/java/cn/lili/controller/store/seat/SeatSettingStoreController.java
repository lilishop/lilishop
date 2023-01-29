package cn.lili.controller.store.seat;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.im.entity.dos.SeatSetting;
import cn.lili.modules.im.service.SeatSettingService;;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 店铺端,分类绑定参数组管理接口
 *
 * @author pikachu
 * @since 2020-02-18 15:18:56
 */
@RestController
@Api(tags = "店铺端,坐席设置")
@RequestMapping("/store/seat/setting")
@Transactional(rollbackFor = Exception.class)
public class SeatSettingStoreController {

    @Autowired
    private SeatSettingService seatSettingService;

    @ApiOperation(value = "查询坐席设置")
    @GetMapping
    public ResultMessage<SeatSetting> getSetting() {
        return ResultUtil.data(seatSettingService.getSetting(UserContext.getCurrentUser().getTenantId()));
    }

    @ApiOperation(value = "更新坐席设置")
    @PutMapping
    public void update(SeatSetting seatSetting) {
        seatSetting.setTenantId(UserContext.getCurrentUser().getTenantId());
        seatSettingService.updateByStore(seatSetting);
    }
}
