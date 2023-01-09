package cn.lili.mybatis;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;


/**
 * 租户超级类
 *
 * @author Chopper
 * @version v1.0
 * @since 2020/8/20 14:34
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public abstract class BaseTenantEntity extends BaseEntity {

    @ApiModelProperty(value = "租户id", hidden = true)
    private String tenantId;

}
