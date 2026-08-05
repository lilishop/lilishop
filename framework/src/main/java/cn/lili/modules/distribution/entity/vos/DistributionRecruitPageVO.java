package cn.lili.modules.distribution.entity.vos;

import cn.lili.modules.distribution.entity.dos.DistributionRecruitApplication;
import cn.lili.modules.system.entity.dto.DistributionRecruitFieldSetting;
import cn.lili.modules.system.entity.dto.DistributionRecruitSetting;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.List;

/**
 * 分销招募页VO
 *
 * @author pikachu
 */
@Data
public class DistributionRecruitPageVO {

    @Schema(description = "分销招募设置")
    private DistributionRecruitSetting plan;

    @Schema(description = "动态字段")
    private List<DistributionRecruitFieldSetting> fields;

    @Schema(description = "是否展示线上入口")
    private Boolean onlineEntryVisible;

    @Schema(description = "当前会员分销员状态")
    private String distributionStatus;

    @Schema(description = "当前会员最新招募申请")
    private DistributionRecruitApplication currentApplication;
}
