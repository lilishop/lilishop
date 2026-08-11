package cn.lili.modules.im.entity.dos;

import cn.lili.mybatis.BaseTenantEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotEmpty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * 坐席
 *
 * @author Chopper
 * @version v1.0
 * 2022-02-09 17:08
 */
@Data
@TableName("li_seat")
@Schema(description = "坐席")
@NoArgsConstructor
@EqualsAndHashCode(callSuper = false)
public class Seat extends BaseTenantEntity {

    @Schema(description = "租户id")
    private String tenantId;

    @Schema(description = "坐席用户名")
    private String username;

    @Schema(description = "客户头像")
    private String face;

    @Schema(description = "坐席密码")
    private String password;

    @Schema(description = "昵称")
    private String nickName;

    @Schema(description = "坐席状态")
    private Boolean disabled;

    @NotEmpty(message = "手机号码不能为空")
    @Schema(description = "手机号码", requiredMode = Schema.RequiredMode.REQUIRED)
    private String mobile;

}
