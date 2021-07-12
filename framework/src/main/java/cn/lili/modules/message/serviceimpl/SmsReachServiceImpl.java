package cn.lili.modules.message.serviceimpl;

import cn.hutool.json.JSONUtil;
import cn.lili.common.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.common.rocketmq.tags.OtherTagsEnum;
import cn.lili.common.utils.BeanUtil;
import cn.lili.config.rocketmq.RocketmqCustomProperties;
import cn.lili.modules.message.entity.dos.SmsReach;
import cn.lili.modules.message.entity.dto.SmsReachDTO;
import cn.lili.modules.message.mapper.SmsReachMapper;
import cn.lili.modules.message.service.SmsReachService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 短信任务业务层实现
 *
 * @author Bulbasaur
 * @date 2021/1/30 3:19 下午
 */
@Service
public class SmsReachServiceImpl extends ServiceImpl<SmsReachMapper, SmsReach> implements SmsReachService {
    @Autowired
    private RocketMQTemplate rocketMQTemplate;
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;


    @Override
    public void addSmsReach(SmsReach smsReach,List<String> mobile) {
        String destination = rocketmqCustomProperties.getNoticeSendTopic() + ":" + OtherTagsEnum.SMS.name();
        SmsReachDTO smsReachDTO = new SmsReachDTO();
        BeanUtil.copyProperties(smsReach,smsReachDTO);
        smsReachDTO.setMobile(mobile);
        this.save(smsReach);
        //发送短信批量发送mq消息
        rocketMQTemplate.asyncSend(destination, JSONUtil.toJsonStr(smsReachDTO), RocketmqSendCallbackBuilder.commonCallback());

    }
}
