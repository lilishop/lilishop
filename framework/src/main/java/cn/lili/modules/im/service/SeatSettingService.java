package cn.lili.modules.im.service;

import cn.lili.modules.im.entity.dos.SeatSetting;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 坐席设置业务
 *
 * @author pikachu
 * @since 2020-02-18 16:18:56
 */
public interface SeatSettingService extends IService<SeatSetting> {


    /**
     * 根据店铺id获取坐席配置
     *
     * @param storeId
     * @return
     */
    SeatSetting getSetting(String storeId);

    /**
     * 根据店铺修改坐席设置
     *
     * @param seatSetting 坐席设置
     * @return
     */
    SeatSetting updateByStore(SeatSetting seatSetting);
}