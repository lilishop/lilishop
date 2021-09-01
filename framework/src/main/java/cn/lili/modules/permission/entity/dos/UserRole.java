package cn.lili.modules.permission.entity.dos;

import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 用户角色
 *
 * @author Chopper
 * @since 2020/11/19 12:18
 */
@Data
@TableName("li_user_role")
@ApiModel(value = "用户角色")
public class UserRole extends BaseIdEntity {

    @ApiModelProperty(value = "用户唯一id")
    private String userId;

    @ApiModelProperty(value = "角色唯一id")
    private String roleId;

    public UserRole(String userId, String roleId) {
        this.userId = userId;
        this.roleId = roleId;
    }

    public UserRole() {

    }
}
