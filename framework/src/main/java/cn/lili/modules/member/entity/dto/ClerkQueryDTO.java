package cn.lili.modules.member.entity.dto;

import cn.lili.mybatis.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.hibernate.validator.constraints.Length;

/**
 * 店员查询
 *
 * @author Chopper
 * @since 2020/11/16 19:55
 */
@Data
@ApiModel(value = "店员查询")
public class ClerkQueryDTO extends BaseEntity {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty(value = "店员名称")
    @Length(max = 20, message = "用户名长度不能超过20个字符")
    private String clerkName;

    @ApiModelProperty(value = "手机")
    @Length(max = 11, message = "手机号长度不能超过11")
    private String mobile;

    @ApiModelProperty(value = "所属部门id")
    private String departmentId;

    @ApiModelProperty(value = "是否为超级管理员")
    private Boolean isSuper;

    @ApiModelProperty(value = "状态")
    private Boolean status;

    @ApiModelProperty(value = "店铺id", hidden = true)
    private String storeId;
}
