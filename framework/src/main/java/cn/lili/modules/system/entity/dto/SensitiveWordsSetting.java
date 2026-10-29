package cn.lili.modules.system.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * 敏感词过滤设置
 *
 * @author Bulbasaur
 * @date 2026-10-29
 */
@Data
public class SensitiveWordsSetting implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "敏感词过滤总开关")
    private Boolean enabled = true;

    @Schema(description = "关闭了敏感词过滤的分组key列表（SensitiveFilterScene.name()），默认为空即全部开启")
    private List<String> disabledGroups = new ArrayList<>();
}
