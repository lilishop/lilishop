package cn.lili.modules.distribution.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 上级分销员信息VO
 *
 * @author pikachu
 */
@Data
public class DistributionParentInfoVO {

    @Schema(description = "上级分销员ID")
    private String parentDistributionId;

    @Schema(description = "上级昵称")
    private String parentNickname;
}
