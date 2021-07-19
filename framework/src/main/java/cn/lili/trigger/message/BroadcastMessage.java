package cn.lili.common.delayqueue;

import cn.hutool.core.date.DateTime;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

/**
 * 直播消息实体
 *
 * @author Bulbasaur
 * @date: 2021/6/1 4:48 下午
 */
@Data
@NoArgsConstructor
public class BroadcastMessage {

    /**
     * 直播间ID
     */
    private String studioId;

    /**
     * 状态
     */
    private String status;


    public BroadcastMessage(String studioId,  String status) {
        this.studioId = studioId;
        this.status = status;
    }
}
