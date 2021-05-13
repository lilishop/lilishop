package cn.lili.modules.store.entity.vos;

import cn.lili.modules.store.entity.dto.StoreEditDTO;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 店铺详细VO
 *
 * @author pikachu
 * @date 2020-03-09 21:53:20
 */
@Data
public class StoreDetailVO extends StoreEditDTO {

    @ApiModelProperty(value = "会员名称")
    private String memberName;

}
