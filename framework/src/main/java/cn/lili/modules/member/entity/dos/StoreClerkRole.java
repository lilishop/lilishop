package cn.lili.modules.member.entity.dos;

import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 用户角色
 *
 * @author Chopper
 * @since 2020/11/19 12:18
 */
@Data
@TableName("li_clerk_role")
@ApiModel(value = "用户角色")
@Builder
@NoArgsConstructor
public class StoreClerkRole extends BaseIdEntity {

    @ApiModelProperty(value = "店员唯一id")
    private String clerkId;

    @ApiModelProperty(value = "角色唯一id")
    private String roleId;

    public StoreClerkRole(String clerkId, String roleId) {
        this.clerkId = clerkId;
        this.roleId = roleId;
    }

}

