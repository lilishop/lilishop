package cn.lili.modules.member.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;


/**
 * 部门
 *
 * @author Chopper
 * @since 2020/11/19 11:57
 */
@Data
@TableName("li_store_role")
@ApiModel(value = "店铺角色")
public class StoreRole extends BaseEntity {

    @ApiModelProperty(value = "角色名")
    @NotEmpty(message = "角色名称必填")
    private String name;

    @ApiModelProperty(value = "店铺id", hidden = true)
    private String storeId;

    @ApiModelProperty(value = "是否为注册默认角色")
    private Boolean defaultRole = false;

    @ApiModelProperty(value = "备注")
    private String description;
}