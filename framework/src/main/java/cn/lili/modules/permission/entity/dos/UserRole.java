package cn.lili.modules.permission.entity.dos;

import cn.lili.base.IdEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 用户角色
 *
 * @author Chopper
 * @date 2020/11/19 12:18
 */
@Data
@Entity
@Table(name = "li_user_role")
@TableName("li_user_role")
@ApiModel(value = "用户角色")
public class UserRole extends IdEntity {

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
