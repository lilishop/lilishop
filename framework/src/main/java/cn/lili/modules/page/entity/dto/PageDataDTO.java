package cn.lili.modules.page.entity.dto;

import cn.lili.modules.page.entity.enums.PageEnum;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 楼层装修数据DTO
 *
 * @author Bulbasaur
 * @date 2020/12/10 17:44
 */
@Data
@NoArgsConstructor
public class PageDataDTO {

    @ApiModelProperty(value = "值")
    private String num;

    /**
     * @see PageEnum
     */
    @ApiModelProperty(value = "页面类型", allowableValues = "INDEX,STORE,SPECIAL")
    private String pageType;

    /**
     * @see cn.lili.modules.base.entity.enums.ClientTypeEnum
     */
    @ApiModelProperty(value = "客户端类型", allowableValues = "PC,H5,WECHAT_MP,APP")
    private String pageClientType;

    public PageDataDTO(String pageType) {
        this.pageType = pageType;
    }
}
