package cn.lili.modules.message.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import cn.lili.modules.message.entity.dos.WechatMPMessage;
import org.apache.ibatis.annotations.Delete;

/**
 * 微信小程序消息订阅 Dao层
 *
 * @author Chopper
 */
public interface WechatMPMessageMapper extends BaseMapper<WechatMPMessage> {

    /**
     * 删除微信服务消息
     */
    @Delete("delete from li_wechat_mp_message")
    void deleteAll();
}