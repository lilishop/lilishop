package cn.lili.modules.im.entity.vo;

import cn.lili.modules.im.entity.dos.Seat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * 客服坐席VO
 *
 * @author Chopper
 * @version v1.0
 * 2022-02-10 15:02
 */
@Data
@Schema(description = "客服坐席VO")
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
public class SeatVO extends Seat {

    private static final long serialVersionUID = 1L;

    @Schema(description = "在线状态")
    private String onlineStatus;

}
