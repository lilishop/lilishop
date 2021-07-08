package cn.lili.modules.message.service;

import cn.lili.modules.message.entity.dos.SmsReach;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 短信任务业务层
 *
 * @author Bulbasaur
 * @date 2021/1/30 3:19 下午
 */
public interface SmsReachService extends IService<SmsReach> {

    /**
     * 添加短信任务
     *
     * @param smsReach 短信签名
     * @param mobile   手机号
     */
    void addSmsReach(SmsReach smsReach, List<String> mobile);


}
