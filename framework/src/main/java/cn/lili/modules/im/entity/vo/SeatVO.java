package cn.lili.modules.im.entity.vo;

import cn.lili.modules.im.entity.dos.Seat;
import lombok.Data;

/**
 * 客服VO
 *
 * @author Chopper
 * @version v1.0
 * 2022-02-10 15:02
 */
@Data
public class SeatVO extends Seat {

    /**
     * 在线状态
     */
    private String onlineStatus;


}
