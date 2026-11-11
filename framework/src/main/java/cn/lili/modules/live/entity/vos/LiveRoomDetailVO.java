package cn.lili.modules.live.entity.vos;

import cn.lili.modules.live.entity.dos.LiveRoom;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class LiveRoomDetailVO extends LiveRoom {

    /**
     * 密钥Id
     */
    private String secretId;

    /**
     * 直播IM SDK APPID
     */
    private String imSdkAppid;
}
