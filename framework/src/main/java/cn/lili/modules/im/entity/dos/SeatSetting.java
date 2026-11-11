package cn.lili.modules.im.entity.dos;

import cn.lili.mybatis.BaseTenantEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * 坐席设置
 *
 * @author Chopper
 * @version v1.0
 * 2022-02-09 17:55
 */
@Data
@TableName("li_seat_setting")
@Schema(description = "坐席设置")
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
public class SeatSetting extends BaseTenantEntity {

    private static final long serialVersionUID = 1L;

    @Schema(description = "租户ID")
    private String tenantId;

    @Schema(description = "欢迎语")
    private String welcome;

    @Schema(description = "离线自动回复")
    private String outLineAutoReply;

    @Schema(description = "长时间自动回复")
    private String longTermAutoReply;

}
