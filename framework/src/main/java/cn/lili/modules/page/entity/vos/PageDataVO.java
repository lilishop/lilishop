package cn.lili.modules.page.entity.vos;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 楼层装修数据VO
 *
 * @author Bulbasaur
 * @since 2020/12/10 17:42
 */
@Data
public class PageDataVO {

    @ApiModelProperty(value = "页面数据")
    private String pageData;
}
