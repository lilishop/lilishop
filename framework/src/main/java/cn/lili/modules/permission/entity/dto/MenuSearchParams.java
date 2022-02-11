package cn.lili.modules.permission.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 权限搜索参数
 *
 * @author Chopper
 * @since 2020-11-19 16:46
 */
@Data
public class MenuSearchParams {

    @ApiModelProperty(value = "菜单/权限名称")
    private String name;

    @ApiModelProperty(value = "层级")
    private Integer level;

    @ApiModelProperty(value = "菜单标题")
    private String title;

    @ApiModelProperty(value = "赋权API地址,正则表达式")
    private String path;

    @ApiModelProperty(value = "前端路由")
    private String frontRoute;

    @ApiModelProperty(value = "图标")
    private String icon;

}
