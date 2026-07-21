package cn.lili.modules.store.entity.vos;

import cn.lili.modules.store.entity.dos.Store;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 店铺VO
 *
 * @author pikachu
 * @since 2020-03-07 17:02:05
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class StoreVO extends Store {

    @Schema(description = "库存预警数量")
    private Integer stockWarning;

    @Schema(description = "登录用户的昵称")
    private String nickName;

    @Schema(description = "订单总数")
    private Long orderNum;

}
