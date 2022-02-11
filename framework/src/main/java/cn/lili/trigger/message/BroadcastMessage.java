package cn.lili.trigger.message;

import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 直播消息实体
 *
 * @author Bulbasaur
 * @since 2021/6/1 4:48 下午
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
