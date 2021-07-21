package cn.lili.modules.store.entity.vos;

import cn.lili.modules.store.entity.dos.Store;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 店铺VO
 *
 * @author pikachu
 * @since 2020-03-07 17:02:05
 */
@Data
public class StoreVO extends Store {

    @ApiModelProperty(value = "库存预警数量")
    private Integer stockWarning;

    @ApiModelProperty(value = "登录用户的昵称")
    private String nickName;

}
