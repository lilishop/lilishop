package cn.lili.modules.member.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * 买家端经验值任务列表响应，包含经验值说明及当前客户已开启任务的完成状态、累计获得经验值。
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Data
public class ExperienceRuleProgressListVO implements Serializable {

    private static final long serialVersionUID = 4680553575916909550L;

    @Schema(description = "经验值说明")
    private String description;

    @Schema(description = "经验值规则列表")
    private List<ExperienceRuleProgressVO> items;
}
