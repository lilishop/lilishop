package cn.lili.modules.distribution.entity.vos;

import cn.lili.modules.distribution.entity.dos.DistributionGrade;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.List;

/**
 * 买家端分销等级列表（附带是否展示各等级规则与权益）。
 * 
 * @author Mike
 * @date 2026-09-11
 */
@Data
public class DistributionBuyerGradeListVO {

    @Schema(description = "是否展示各等级规则与权益")
    private Boolean upgradeConditionVisible;

    @Schema(description = "分销等级列表")
    private List<DistributionGrade> grades;
}
